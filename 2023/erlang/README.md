# Run

- We expect input file in *input/day01*
- You can compile the erlang file and run it in the eshell:

```Erlang
❯ erl
Erlang/OTP 25 [erts-13.1.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.1.5  (abort with ^G)
1> c("day01.erl").
{ok,day01}
2> day01:main().
Reading input file
Part 1: 54561
Part 2: 54076
ok
3> q().
ok
```
