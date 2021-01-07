/* -*- mode: c -*- */

#define ITERATIONS  40000
#define ROD_LIMIT   211
#define PEARL_LIMIT 42

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
    
    REPEAT (262) {
      pearls += (unsigned short)SAMPLE(20, 423);
    }
    if ((maximum_rods.x < ROD_LIMIT && rods > maximum_rods.x) ||
        (rods >= ROD_LIMIT && pearls > maximum_rods.y))
      maximum_rods = (ushort2){rods, pearls};
    if ((maximum_pearls.y < PEARL_LIMIT && pearls > maximum_pearls.y) ||
        (pearls >= PEARL_LIMIT && rods > maximum_pearls.x))
      maximum_pearls = (ushort2){rods, pearls};
  }

  results[tid] = (ushort4){maximum_rods, maximum_pearls};
}
