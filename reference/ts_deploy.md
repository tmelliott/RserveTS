# Deploy a typed 'Rserve' app

Writes an 'Rserve' launcher script for an app source file. By default
the script is written under the same directory as
[`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
file output (`RserveTS.compile_dir` / `RSERVETS_COMPILE_DIR` /
[`tempdir()`](https://rdrr.io/r/base/tempfile.html)) as
`{basename(f)}.rserve.R`. Pass `file` explicitly to choose another path.

## Usage

``` r
ts_deploy(
  f,
  file = NULL,
  init = NULL,
  port = 6311,
  run = c("no", "here", "background"),
  silent = FALSE
)
```

## Arguments

- f:

  The path to the application files

- file:

  The file to write the deployment script to. When `NULL` (default),
  uses `{basename(f)}.rserve.R` under the default compile directory (see
  [`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)).

- init:

  Names of
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  objects to make available to the initialisation function

- port:

  The port to deploy the app on

- run:

  Whether to run the deployment script, takes values "no", "here",
  "background"

- silent:

  Whether to print the deployment script

## Value

The path written to (`file`), invisibly. With `run = "here"` or
`"background"`, also starts 'Rserve' as requested.

## Examples

``` r
src <- tempfile(fileext = ".R")
writeLines(
    "add <- ts_function(function(x = ts_integer(1)) x, result = ts_integer(1), export = TRUE)",
    src
)
out <- ts_deploy(src, silent = TRUE, run = "no")
file.exists(out)
#> [1] TRUE
```
