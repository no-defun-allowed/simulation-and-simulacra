/* -*- mode: c -*- */

#define ITERATIONS  1000000
#define ROD_LIMIT   211
#define PEARL_LIMIT 42

/* Generate this with generate-cdf-table.lisp */
__constant static const unsigned long binomial_table[] = {
  0xFFFFFFFFFFFFFFFF, 0xFFFFCC4D8E5B5800, 0xFFFD2C1CF200D000, 0xFFEC2AB9775B1800,
  0xFFA30678F43A4000, 0xFEB7FDF86B720000, 0xFC5E1ED41D0E2800, 0xF75EB523A0C4D800,
  0xEE4CA01ADA0E3800, 0xDFF35B4202CF7000, 0xCBDA8D49915DC000, 0xB29ED274B93D8800,
  0x95EE93BA0EF0D800, 0x7826E5FC50660000, 0x5BBB01DE506BD400, 0x42A4D3E0B12DC000,
  0x2E0F58CEF370EC00, 0x1E4A3EA6852CAD00, 0x12F706D5B6A42500, 0x0B50A2BCD8204280,
  0x067080E64CA6DBC0, 0x037FE85DF2331780, 0x01D17F4949B51D50, 0x00E7812DA80EBE20,
  0x006E548F2D2EC48C, 0x003271DB37642DFA, 0x001626C3CF96D2E8, 0x00095A2944C442B3,
  0x0003CCD2F63E5659, 0x00017CCF6917E37B, 0x00008FBDC1CC6850, 0x0000345D6266AF6A,
  0x0000126D4532FDB5, 0x00000644BF8078B9, 0x0000021027D8B74B, 0x000000A84BD877E8,
  0x00000033F51CE72F, 0x0000000F8D2DBB06, 0x0000000484025495, 0x0000000145D6265E,
  0x000000005933E6CC, 0x0000000017BB6B20, 0x0000000006239A56, 0x00000000018B7DC4,
  0x000000000060DD7E, 0x00000000001719ED, 0x0000000000055DF5, 0x0000000000013716,
  0x00000000000044AC, 0x0000000000000EC9, 0x000000000000031B, 0x00000000000000A3,
  0x0000000000000020, 0x0000000000000006, 0x0000000000000001, 0x0000000000000000
};
#define BINOMIAL_TABLE_LENGTH 56

inline unsigned short pearl_drops(unsigned long uniform64) {
  unsigned short first = 0, last = BINOMIAL_TABLE_LENGTH - 1;
  while (first != last) {
    unsigned short middle = (first + last + 1) / 2;
    unsigned short m = middle - 1;
    /* Set the MSB if this is true. */
    unsigned short pull_left = binomial_table[middle] > uniform64;
    first = select(first, middle, pull_left);
    last  = select(m,     last,   pull_left);
  }
  return first;
}

inline unsigned int MWC64X(unsigned long* state) {
  unsigned int c = (*state) >> 32, x = (*state) & 0xFFFFFFFF;
  *state = x * ((unsigned long)4294883355U) + c;
  return x^c;
}

__kernel void simulate(__global unsigned long* initial_seeds, __global ushort4* results) {
  size_t tid = get_global_id(0);
  
#define SAMPLE(low, high) ((MWC64X(&seed) % high) < low)
#define MASK(bits) ((1L << (bits)) - 1L)
#define REPEAT(n) for (int zz = 0; zz < n; zz++)
  unsigned long seed = initial_seeds[tid];

  ushort2 maximum_rods = {0, 0}, maximum_pearls = {0, 0};

  for (int i = 0; i < ITERATIONS; i++) {
    unsigned short rods = 0, pearls = 0;
    /* Thanks to benjamin from ##symbolics2 for this idea. 
       We know that each bit is going to have a 0.5 probability of being set,
       and blaze drops have a probability of 0.5. So popcount(random-value)
       conveniently lets us perform many tests in parallel. */
    REPEAT (305 / 32) {
      /* This basically performs 32 tests from one random 32-bit value. */
      rods += (unsigned short)popcount(MWC64X(&seed));
    }
    /* Now perform the last few tests. */
    rods += (unsigned short)popcount(MWC64X(&seed) & MASK(305L % 32L));

    /* Compute the pearl drops by generating a uniform double. */
    unsigned long uniform64 = ((unsigned long)MWC64X(&seed) << 32L) |
                              (unsigned long)(MWC64X(&seed));
    /* Use the inverse CDF table to realize the binomial distribution for 
       pearl drops. */
    pearls += pearl_drops(uniform64);
      
    if ((maximum_rods.x < ROD_LIMIT && rods > maximum_rods.x) ||
        (rods >= ROD_LIMIT && pearls > maximum_rods.y))
      maximum_rods = (ushort2){rods, pearls};
    if ((maximum_pearls.y < PEARL_LIMIT && pearls > maximum_pearls.y) ||
        (pearls >= PEARL_LIMIT && rods > maximum_pearls.x))
      maximum_pearls = (ushort2){rods, pearls};
  }

  results[tid] = (ushort4){maximum_rods, maximum_pearls};
}
