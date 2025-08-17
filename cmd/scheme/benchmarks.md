# VM Speed

Benchmarks are run on MacBook Pro 1.4 GHz Quad-Core Intel Core i5. The
Python benchmarks are run with Python 3.7.

Best version: cde886da0f7e3cc79909a86b049eab6a503f455d

| Optimization     | fib 30 | fib30.py | Py/Scm | fib 40 | fib40.py | Py/Scm |
|:-----------------|-------:|---------:|-------:|-------:|---------:|-------:|
| Base             | 13.363 |    0.366 |  .0273 |        |   35.591 |        |
| Frame FL         |  9.920 |          |  .0368 |        |          |        |
| Stack+Env        |  4.900 |          |  .0747 |        |          |        |
| Optimized +-     |  1.471 |          |  .2488 |        |          |        |
| Inline not/null? |  1.333 |          |  .2745 |        |          |        |
| Inline +/-       |  1.282 |          |  .2854 |        |          |        |
| Lambda{,Impl}    |  1.151 |          |  .3180 |        |          |        |
| Inline <=        |  0.270 |          | 1.3555 | 29.730 |          | 1.1971 |
| {+,-}const       |  0.220 |          | 1.6636 | 24.023 |          | 1.4815 |

Showing top 10 nodes out of 23
      flat  flat%   sum%        cum   cum%
    18.95s 83.22% 83.22%     21.81s 95.78%  github.com/markkurossi/scheme.(*Scheme).Apply
     0.81s  3.56% 86.78%      0.81s  3.56%  runtime.pthread_cond_signal
     0.74s  3.25% 90.03%      0.76s  3.34%  github.com/markkurossi/scheme.numGt
     0.46s  2.02% 92.05%      0.98s  4.30%  github.com/markkurossi/scheme.numAdd
     0.44s  1.93% 93.98%      0.44s  1.93%  github.com/markkurossi/scheme.(*Scheme).popFrame (inline)
     0.38s  1.67% 95.65%      0.77s  3.38%  runtime.convT64
     0.36s  1.58% 97.23%      0.39s  1.71%  runtime.mallocgcTiny
     0.27s  1.19% 98.42%      0.27s  1.19%  github.com/markkurossi/scheme.IsTrue (inline)
     0.18s  0.79% 99.21%      0.18s  0.79%  runtime.asyncPreempt
         0     0% 99.21%     21.81s 95.78%  github.com/markkurossi/scheme.(*Scheme).Eval

Master:

| Optimization   | fib 30 | fib30.py | Py/Scm | fib 40 | fib40.py | Py/Scm |
|:---------------|-------:|---------:|-------:|-------:|---------:|-------:|
| with all above |        |          |        | 29.420 |          |        |

Showing top 10 nodes out of 29
      flat  flat%   sum%        cum   cum%
    22.19s 85.38% 85.38%     25.07s 96.46%  github.com/markkurossi/scheme.(*Scheme).Apply
     0.83s  3.19% 88.57%      0.83s  3.19%  github.com/markkurossi/scheme.numGt
     0.72s  2.77% 91.34%      0.72s  2.77%  runtime.pthread_cond_signal
     0.46s  1.77% 93.11%      0.85s  3.27%  runtime.convT64
     0.44s  1.69% 94.81%      0.98s  3.77%  github.com/markkurossi/scheme.numAdd
     0.39s  1.50% 96.31%      0.39s  1.50%  runtime.mallocgcTiny
     0.28s  1.08% 97.38%      0.28s  1.08%  github.com/markkurossi/scheme.IsTrue (inline)
     0.26s  1.00% 98.38%      0.26s  1.00%  github.com/markkurossi/scheme.(*Scheme).popFrame (inline)
     0.22s  0.85% 99.23%      0.22s  0.85%  runtime.asyncPreempt
     0.14s  0.54% 99.77%      0.14s  0.54%  runtime.madvise


## leibniz.scm

| Optimization | Scheme | Python | Py/Scm |
|:-------------|-------:|--------|--------|
| Base         | 23.194 | 6.224  | 0.2683 |
| *const       | 20.364 |        | 0.3056 |
| No pushs 0   | 19.997 |        | 0.3112 |
