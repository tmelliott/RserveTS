# Union type

Create a union of types. Currently this only accepts schemas as strings.

## Usage

``` r
ts_union(..., default = NULL)
```

## Arguments

- ...:

  Type objects to merge

- default:

  Default value for the type (optional).

## Examples

``` r
x <- ts_union(ts_numeric(1), ts_character(1))
```
