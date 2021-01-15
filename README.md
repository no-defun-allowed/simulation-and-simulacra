# SIMULATION AND SIMULACRA

![A plot of 2 trillion simulations.](2-trillion-runs.png)

A program that simulates "playing Minecraft", watching how many blaze rods drop
and how many ender pearls are traded for.

On a Radeon RX 580 card, this code takes about 2.00 seconds to perform 
(the default value of) 9000 simulation jobs, each performing 1,000,000 
simulations. This results in performing about 4.51 billion simulations a second.

This program repeatedly prints out lines like

```
2.294e+09 iterations: (209 rods, 13) (151, 39 pearls)    this run: (209 rods, 13) (151, 39 pearls)
```

which report the best runs of the loop, and the best runs of the batch. We 
measure the "best" as:

- If we have not reached the primary target (211 rods for the blaze rod loot,
  and 42 pearl trades for the ender pearl trades), then maximize the primary
  value.
- If we aren't decreasing the primary value, maximize the secondary value.

You win if either run reaches at least `(212 rods, 42 pearls)`. One run must
achieve both values; it does not take too long to reach something like
`(212 rods, 10) (167, 42 pearls)`, but neither run is in fact successful.


## How do I run this?

Clone this repository and [eazy-opencl](https://github.com/guicho271828/eazy-opencl)
somewhere where Quicklisp can find them.

Then start Lisp and evaluate the following forms. Your REPL should look like
this:

```lisp
CL-USER> (ql:quickload :simulation-and-simulacra)
To load "simulation-and-simulacra":
  [Quicklisp noises]
(:SIMULATION-AND-SIMULACRA)
CL-USER> (simulation-and-simulacra:list-platforms)
Platform 0: AMD Accelerated Parallel Processing from Advanced Micro Devices, Inc., OpenCL 2.1 AMD-APP (3075.10)
Device  0, 0: Ellesmere, 1411 MHz, 7985 MiB memory
; No value
CL-USER> (simulation-and-simulacra:choose-device 0)
#S(EAZY-OPENCL.BINDINGS:BOXED-PROGRAM :VALUE 139878696236304)
CL-USER> (simulation-and-simulacra:simulation-loop)
9.000e+09 iterations: (207 rods, 15) (157, 39 pearls)    this run: (207 rods, 15) (157, 39 pearls)
1.800e+10 iterations: (210 rods, 13) (157, 39 pearls)    this run: (210 rods, 13) (149, 38 pearls)
2.700e+10 iterations: (210 rods, 13) (145, 40 pearls)    this run: (207 rods, 13) (145, 40 pearls)
3.600e+10 iterations: (210 rods, 13) (145, 40 pearls)    this run: (208 rods, 10) (148, 39 pearls)
4.500e+10 iterations: (210 rods, 13) (145, 40 pearls)    this run: (207 rods, 17) (156, 39 pearls)
```

## How did you make it go fast?

The first thing we did was port a simple simulator to OpenCL, to exploit the
parallel execution on a GPU. This immediately provided something like a 8× 
speedup.

The next obvious change to make is to ditch the Java RNG algorithm. This should
not affect simulation results, assuming that there are no properties of the RNG
that would change the probability of interesting events. The [first Dream 
investigation paper](https://mcspeedrun.com/dream.pdf) analyses the RNG in 
section 9, and we feel we are not affecting the results significantly as 
"extra calls to the world random can be viewed as an extra source of randomness.
The chaotic nature of when Minecraft needs numbers, especially taken across both
runs and streams, only adds entropy to the system. This provided a 2.5× speedup.

We then looked at speeding up generating random variables from a binomial
distribution. Blaze rods have a 50% probability of dropping, and conveniently,
if you generate a random number in [0, 2^n) for some integer n, then every bit
has a 50% probability of dropping.  We use the *Hamming weight* (or `popcount`)
of this random number to sample many rods at once - the RNG we use provides 32
bits, so we sample 32 at a time. This provided another 2× speedup.

However, we couldn't exactly use the same trick for pearl trades - those have
an awkward probability of 20/423. Instead, we use 
[an inverse CDF table](https://math.stackexchange.com/questions/1427288/how-to-sample-a-binomial-random-variable/1427350#1427350),
generating a uniform 64-bit random integer, and using binary search on this 
table to generate from a binomial distribution. This induces a small inaccuracy,
as we have rounded every probability to the nearest 2^(-64). We have ensured 
that this error should make it very slightly harder to win, instead of very
slightly easier to win, so the results should still be valid; and we do this by
rounding the table values down, as to lower the probability a value is in each
"band" of values, and then treating uniform values which are equal to the table
values as belonging in the lower band. The inaccuracy is still tiny, making it
approximately a hundred millionth harder to achieve 42 pearl trades, which is
not a big deal. It also provided a 10× speedup, so we certainly compensate with
higher throughput.

These optimisations were schemed up in `#symbolics2:matrix.org` 
(or `##symbolics2` on Freenode) - if this kind of thing interests you, and you're
bored, joining in might make you less bored.
