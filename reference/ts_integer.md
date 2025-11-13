# Integer type

Integers are represented in Zod schema as either a number
(`z.number()`), or a Int32Array (`z.instanceof(Int32Array)`).

## Usage

``` r
ts_integer(n = -1L)
```

## Arguments

- n:

  The length of the integer vector. If `n = 1` then a single integer is
  expected. If `n = 0` then any length is expected. If `n > 1` then an
  integer vector of length `n` is expected.

## Value

A ts object that accepts integer scalars or vectors of length `n`.

## Examples

``` r
x <- ts_integer(1)
x$check(1L)
#> [1] 1

if (FALSE) { # \dontrun{
# this will fail
x$check(1:10)
} # }
```
