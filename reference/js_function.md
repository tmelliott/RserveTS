# JS functions callable from R

If result is NULL, it will be an oobSend (R process will continue),
otherwise R process will wait for a response (oobMessage).

## Usage

``` r
js_function(..., result = NULL)
```

## Arguments

- ...:

  arguments passed to the function

- result:

  the type of value returned from JS to R

## Value

A ts object that accepts js functions (as input). Currently not able to
pass as output (but should, in future ...).

## Details

TODO: when compiling, automatically wrap in self.oobMessage() or
self.oobSend(), as necessary... ?

- how about naked js functions? i.e., we might want to pass a function
  *back* to javascript, for some reason?
