# 'JavaScript' functions callable from R

If `result` is `NULL`, it will be an oobSend (R process will continue);
otherwise the R process will wait for a response (oobMessage).

## Usage

``` r
js_function(..., result = NULL)
```

## Arguments

- ...:

  arguments passed to the function

- result:

  the type of value returned from 'JavaScript' to R

## Value

A ts object that accepts 'JavaScript' functions as input. Using
'JavaScript' functions as output (R to 'JavaScript') is not supported
yet.

## Examples

``` r
# Fire-and-forget callback from 'JavaScript' (oobSend)
cb <- js_function(ts_character(1))

# Callback that returns a value to R (oobMessage)
ask <- js_function(ts_integer(1), result = ts_logical(1))
```
