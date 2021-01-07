/* -*- mode: c -*- */

#define ITERATIONS  40000
#define ROD_LIMIT   211
#define PEARL_LIMIT 42

inline unsigned int MWC64X(unsigned long* state) {
    unsigned int c = (*state) >> 32, x = (*state) & 0xFFFFFFFF;
    *state = x * ((unsigned long)4294883355U) + c;
    return x^c;
}

__kernel void simulate(__global unsigned long* initial_seeds, __global ulong4* results) {
  size_t tid = get_global_id(0);
  
#define SAMPLE(low, high) ((MWC64X(&seed) % high) < low)
#define REPEAT(n) for (int zz = 0; zz < n; zz++)
  unsigned long seed = initial_seeds[tid];

  ulong2 maximum_rods = {0, 0}, maximum_pearls = {0, 0};

  for (int i = 0; i < ITERATIONS; i++) {
    unsigned long rods = 0, pearls = 0;
    /* Thanks to benjamin from ##symbolics2 for this idea. 
       We know that each bit is going to have a 0.5 probability of being set,
       and blaze drops have a probability of 0.5. So popcount(random-value)
       conveniently lets us perform many tests in parallel. */
    REPEAT (305 / 32) {
      /* This basically performs 32 tests from one random 32-bit value. */
      rods += popcount(MWC64X(&seed));
    }
    /* Now perform the last few tests. */
    rods += popcount(MWC64X(&seed) & ((1L << (305L % 32L)) - 1L));
    REPEAT (262) {
      pearls += SAMPLE(20, 423);
    }
    if ((maximum_rods.x < ROD_LIMIT && rods > maximum_rods.x) ||
        (rods == maximum_rods.x && pearls > maximum_rods.y))
      maximum_rods = (ulong2){rods, pearls};
    if ((maximum_pearls.y < PEARL_LIMIT && pearls > maximum_pearls.y) ||
        (pearls == maximum_pearls.y && rods > maximum_pearls.x))
      maximum_pearls = (ulong2){rods, pearls};
  }

  results[tid] = (ulong4){maximum_rods, maximum_pearls};
}
