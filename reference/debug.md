# Debug Logging for RserveTS

Controlled via the `RSERVETS_DEBUG` environment variable. Set to `*` for
all tags, or a comma-separated list of tags: `widget`, `ocap`, `init`,
`state`, `child`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Enable all debug logging
Sys.setenv(RSERVETS_DEBUG = "*")

# Enable specific tags
Sys.setenv(RSERVETS_DEBUG = "widget,child,init")

# Disable
Sys.setenv(RSERVETS_DEBUG = "")
} # }
```
