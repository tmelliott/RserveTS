# Union type

Create a union of types. Currently this only accepts schemas as strings.

## Usage

``` r
ts_union(...)
```

## Arguments

- ...:

  Type objects to merge

## Examples

``` r
x <- ts_union(ts_numeric(1), ts_character(1))
```
