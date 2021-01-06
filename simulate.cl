/* -*- mode: c -*- */

#define ITERATIONS 40000

inline unsigned int MWC64X(unsigned long* state)
{
    unsigned int c = (*state) >> 32, x = (*state) & 0xFFFFFFFF;
    *state = x * ((unsigned long)4294883355U) + c;
    return x^c;
}

__kernel void simulate(__global unsigned long* initial_seeds, __global unsigned long* results) {
  size_t tid = get_global_id(0);
  
#define SAMPLE(low, high) ((MWC64X(&seed) % high) < low)
#define REPEAT(n) for (int zz = 0; zz < n; zz++)
  unsigned long seed = initial_seeds[tid];

  unsigned long maximum_rods = 0, maximum_pearls = 0;

  for (int i = 0; i < ITERATIONS; i++) {
    unsigned long rods = 0, pearls = 0;
    /* Thanks to benjamin from ##symbolics2 for this idea. */
    REPEAT (305 / 32) {
      /* This basically performs 32 tests from one random 32-bit value. */
      rods += popcount(MWC64X(&seed));
    }
    REPEAT (305 % 32) {
      rods += SAMPLE(1, 2);
    }
    REPEAT (262) {
      pearls += SAMPLE(20, 423);
    }
    maximum_rods = max(maximum_rods, rods);
    maximum_pearls = max(maximum_pearls, pearls);
  }

  results[tid * 2]     = maximum_rods;
  results[tid * 2 + 1] = maximum_pearls;
}

__kernel void hello(__global unsigned long* initial_seeds, __global unsigned long* results) {
  size_t tid = get_global_id(0);
  unsigned long seed = initial_seeds[tid];
  results[tid * 2]     = seed;
  results[tid * 2 + 1] = seed;
}
