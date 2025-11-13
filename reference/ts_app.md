# Generate an Rserve app from a ts function

Anything that is not a function simply returns itself. However,
functions are wrapped with
[`Rserve::ocap()`](https://rdrr.io/pkg/Rserve/man/ocap.html), and the
result is subsequently wrapped with `ts_app()`.

## Usage

``` r
ts_app(x)
```

## Arguments

- x:

  A ts function object
  ([`ts_function()`](http://tomelliott.co.nz/RserveTS/reference/ts_function.md))

## Value

An object of class 'OCref', see
[`Rserve::ocap()`](https://rdrr.io/pkg/Rserve/man/ocap.html)

## Examples

``` r
f <- ts_function(function(x = ts_integer(1), y = ts_character(1)) {
    x + nchar(y)
}, result = ts_integer(1))
app <- ts_app(f) # class of 'OCref'
# this can now be used in an Rserve application, for example
```
