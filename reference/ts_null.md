# Null type

This is a type that only accepts `NULL`. For function return types, use
`ts_void`.

## Usage

``` r
ts_null()
```

## Value

A ts object that only accepts `NULL`.

## Examples

``` r
x <- ts_null()
x$check(NULL)
#> NULL
```
