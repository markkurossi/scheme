# VM Speed

| Optimization     | (fib 30) | (fib 40) | fib30.py3 | fib40.py3 | Tag     |
|:-----------------|---------:|---------:|----------:|----------:|:--------|
| Base             |   13.363 |          |     0.366 |    35.591 | 00b05ea |
| Frame FL         |    9.920 |          |           |           |         |
| Stack+Env        |    4.900 |          |           |           |         |
| Optimized +-     |    1.471 |          |           |           |         |
| Inline not/null? |    1.333 |          |           |           |         |
| Inline +/-       |    1.282 |          |           |           |         |
| Lambda{,Impl}    |    1.151 |          |           |           |         |
| Inline <=        |    0.270 |   29.730 |           |           |         |

## test.scm

| Optimization     | (fib 33) | Comments           |
|:-----------------|---------:|--------------------|
| Base             |    4.147 |                    |
| Stack+Env        |    1.975 |                    |
| Inline not/null? |    1.431 |                    |
| Inline +/-       |    1.194 |                    |
| Lambda{,Impl}    |    1.193 |                    |
| Inline <=        |    1.045 | fibo.scm is faster |
