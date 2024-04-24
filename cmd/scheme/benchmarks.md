# VM Speed

Benchmarks are run on MacBook Pro 1.4 GHz Quad-Core Intel Core i5. The
Python benchmarks are run with Python 3.7.

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

## leibniz.scm

| Optimization | Scheme | Python | Py/Scm |
|:-------------|-------:|--------|--------|
| Base         | 23.194 | 6.224  | 0.2683 |
| *const       | 20.364 |        | 0.3056 |
| No pushs 0   | 19.997 |        | 0.3112 |
