# Log a debug message

Log a debug message

## Usage

``` r
rts_log(..., tag = "general")
```

## Arguments

- ...:

  Message parts (passed to paste0)

- tag:

  Character tag for filtering

## Value

`invisible(NULL)`. Emits a
[`message()`](https://rdrr.io/r/base/message.html) when the tag is
enabled.
