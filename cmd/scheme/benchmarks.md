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
