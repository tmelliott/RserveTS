# TS function definition

TS function definition

## Usage

``` r
ts_function(f, ..., result = ts_void(), export = FALSE)
```

## Arguments

- f:

  an R function

- ...:

  argument definitions (only required if f does not specify these in its
  formals)

- result:

  return type (ignored if overloads are provided)

- export:

  if `TRUE`, and defined in the global namespace of the app at compile
  time, the function will be part of the initial functions available to
  Rserve; otherwise it will need to be sent as the result of another
  ocap.

## Value

a ts function object which has a `call` method that will call the
function with the given arguments, which will be checked for type
correctness.

## Details

Defining functions is the core of writing Rserve apps. Functions are
referred to as *object capabilities* (ocaps), as they are 'objects' that
allow Javascript to access capabilities of R with a restricted
interface. Only arguments can be adjusted.

TS functions can be defined using existing (named) or anonymous
functions. Anonymous functions are useful in that the arguments to the
functions can explicitly be defined with their types as formal
arguments:

    ts_function(function(x = ts_integer(), y = ts_string()) { ... })

## Examples

``` r
f <- ts_function(function(x = ts_integer(1), y = ts_character(1)) {
    x + nchar(y)
}, result = ts_integer(1))
f$call(1, "hello")
#> [1] 6
```
