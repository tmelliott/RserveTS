# Typed list

A list is a vector of other robjects, which may or may not be named.

## Usage

``` r
ts_list(..., default = NULL)
```

## Arguments

- ...:

  A list of types, named or unnamed.

- default:

  Default value for the type (optional).

## Value

A ts object that accepts lists with the specified types.

## Details

There are five types of lists we can define:

1.  Unknown list

2.  Known, named list (e.g., list(x = 1:5, y = 'hello world')). This is
    an object in JS.

3.  Known, unnamed list (e.g., list(1:5, 'hello world')). This is an
    array in JS.

4.  Named list of a single datatype (e.g., list(fit1 = lm(...), fit2 =
    lm(...), ...)), where the names and length are not known ahead of
    time. This is a record\<string, type\> in JS.

5.  Unnamed list of a single datatype (e.g., list(lm(...), lm(...),
    ...)), where the length is unknown ahead of time. This is an Array
    in JS.

## Examples

``` r
x <- ts_list(a = ts_integer(1), b = ts_character(1))
x$check(list(a = 1L, b = "a"))
#> $a
#> [1] 1
#> 
#> $b
#> [1] "a"
#> 
```
