# Logical or boolean type

Booleans are represented in Zod schema as either a boolean
(`z.boolean()`), or a typed Uint8Array (`z.instanceof(Uint8Array)`).

## Usage

``` r
ts_logical(n = -1L, default = NULL)
```

## Arguments

- n:

  The length of the boolean vector. If `n = 1` then a single boolean is
  expected. If `n = 0` then any length is expected. If `n > 1` then a
  boolean vector of length `n` is expected.

- default:

  Default value for the type (optional).

## Value

A ts object that accepts logical scalars or vectors of length `n`.

## Examples

``` r
x <- ts_logical(1)
x$check(TRUE)
#> [1] TRUE

if (FALSE) { # \dontrun{
# this will fail
x$check(5)
} # }
```
