# Numeric type

Numbers are represented in Zod schema as either a number (`z.number()`),
or a Float64Array (`z.instanceof(Float64Array)`).

## Usage

``` r
ts_numeric(n = -1L, default = NULL)
```

## Arguments

- n:

  The length of the numeric vector. If `n = 1` then a single number is
  expected. If `n = 0` then any length is expected. If `n > 1` then a
  numeric vector of length `n` is expected.

- default:

  Default value for the type (optional).

## Value

A ts object that accepts numeric scalars or vectors of length `n`.

## Examples

``` r
x <- ts_numeric(1)
x$check(1)
#> [1] 1

if (FALSE) { # \dontrun{
# this will fail
x$check(c(1, 2, 3))
x$check("a")
} # }
```
