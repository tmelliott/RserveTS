# Array type

An array of typed objects. In zod, these are represented by `z.array()`;
returned objects must be R lists, `Robj.list()`.

## Usage

``` r
ts_array(type)
```

## Arguments

- type:

  The input type, either a zod-style string ("z.number()") or a
  ts_object.

## Value

An array object
