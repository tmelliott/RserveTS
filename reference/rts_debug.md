# Debug logging for 'RserveTS'

Controlled via the `RSERVETS_DEBUG` environment variable. Set to `*` for
all tags, or a comma-separated list of tags: `widget`, `ocap`, `init`,
`state`, `child`.

## See also

[`rts_debug_enabled()`](https://tomelliott.co.nz/RserveTS/reference/rts_debug_enabled.md),
[`rts_log()`](https://tomelliott.co.nz/RserveTS/reference/rts_log.md)

## Examples

``` r
withr::with_envvar(
    c(RSERVETS_DEBUG = "*"),
    {
        rts_debug_enabled("widget")
    }
)
#> [1] TRUE
withr::with_envvar(
    c(RSERVETS_DEBUG = "widget,child,init"),
    {
        rts_debug_enabled("child")
    }
)
#> [1] TRUE
withr::with_envvar(
    c(RSERVETS_DEBUG = ""),
    {
        rts_debug_enabled()
    }
)
#> [1] FALSE
```
