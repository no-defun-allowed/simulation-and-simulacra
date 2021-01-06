/* -*- mode: c -*- */

#define MULTIPLIER 0x5DEECE6DDL
#define ADDEND     0xBL
#define MASK       ((1L << 48) - 1)
#define ITERATIONS 10000

__kernel void simulate(__global unsigned long* initial_seeds, __global unsigned long* results) {
  size_t tid = get_global_id(0);
  
#define NEXT_VALUE seed = (seed * MULTIPLIER + ADDEND) & MASK
#define SAMPLE(low, high) (((seed >> 32L) % high) < low)
#define REPEAT(n) for (int zz = 0; zz < n; zz++)
  unsigned long seed = initial_seeds[tid];

  unsigned long maximum_rods = 0, maximum_pearls = 0;

  for (int i = 0; i < ITERATIONS; i++) {
    unsigned long rods = 0, pearls = 0;
    REPEAT (305) {
      rods += SAMPLE(1, 2);
      NEXT_VALUE;
    }
    REPEAT (262) {
      pearls += SAMPLE(20, 423);
      NEXT_VALUE;
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
