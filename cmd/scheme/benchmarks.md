# VM Speed

Benchmarks are run on MacBook Pro 1.4 GHz Quad-Core Intel Core i5. The
Python benchmarks are run with Python 3.7.

| Optimization     | fib 30 | fib 40 | fib30.py | Py/Scm | fib40.py | Py/Scm |
|:-----------------|-------:|-------:|---------:|-------:|---------:|-------:|
| Base             | 13.363 |        |    0.366 |  .0273 |   35.591 |        |
| Frame FL         |  9.920 |        |          |  .0368 |          |        |
| Stack+Env        |  4.900 |        |          |  .0747 |          |        |
| Optimized +-     |  1.471 |        |          |  .2488 |          |        |
| Inline not/null? |  1.333 |        |          |  .2745 |          |        |
| Inline +/-       |  1.282 |        |          |  .2854 |          |        |
| Lambda{,Impl}    |  1.151 |        |          |  .3180 |          |        |
| Inline <=        |  0.270 | 29.730 |          | 1.3555 |          | 1.1971 |

## test.scm

| Optimization     | (fib 33) | Comments           |
|:-----------------|---------:|--------------------|
| Base             |    4.147 |                    |
| Stack+Env        |    1.975 |                    |
| Inline not/null? |    1.431 |                    |
| Inline +/-       |    1.194 |                    |
| Lambda{,Impl}    |    1.193 |                    |
| Inline <=        |    1.045 | fibo.scm is faster |
