/* -*- mode: c -*- */

#define ITERATIONS  1000000
#define ROD_LIMIT   211
#define PEARL_LIMIT 42

/* Generate this with generate-cdf-table.lisp */
/* This, of course, has an error of up to 2^-64, but that is hopefully a
   sufficiently small margin. Even Pr(Pearls=42) is within 1e-8× its actual value. */
__constant unsigned long binomial_table[] = {
  0xFFFFFFFFFFFFFFFF, 0xFFFFCC4D8E5B559E, 0xFFFD2C1CF200D08B, 0xFFEC2AB9775B187F,
  0xFFA30678F43A3D73, 0xFEB7FDF86B7202F9, 0xFC5E1ED41D0E2BCC, 0xF75EB523A0C4D7B2,
  0xEE4CA01ADA0E34B5, 0xDFF35B4202CF7080, 0xCBDA8D49915DC41B, 0xB29ED274B93D9215,
  0x95EE93BA0EF0E3A2, 0x7826E5FC50660062, 0x5BBB01DE506BD55C, 0x42A4D3E0B12DBCA3,
  0x2E0F58CEF370F08E, 0x1E4A3EA6852CAF0A, 0x12F706D5B6A42735, 0x0B50A2BCD82042E1,
  0x067080E64CA6DBA7, 0x037FE85DF2331802, 0x01D17F4949B51D8C, 0x00E7812DA80EBE45,
  0x006E548F2D2EC49D, 0x003271DB37642E05, 0x001626C3CF96D2EC, 0x00095A2944C442B5,
  0x0003CCD2F63E565A, 0x00017CCF6917E37B, 0x00008FBDC1CC6850, 0x0000345D6266AF6A,
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
    unsigned short pull_left = uniform64 >= binomial_table[middle];
    first = select(middle, first, pull_left);
    last  = select(last,   m,     pull_left);
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
