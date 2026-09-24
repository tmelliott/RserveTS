# Compile R functions

Generates 'TypeScript' schema for the given R function or file path. If
a path, the R app is also generated.

## Usage

``` r
ts_compile(f, ...)
```

## Arguments

- f:

  A function or file path (length-one character string for file
  compilation).

- ...:

  Additional arguments. For the **file path** method, named arguments
  passed to
  [`ts_deploy()`](https://tomelliott.co.nz/RserveTS/reference/ts_deploy.md)
  (e.g. `init`, `port`, `run`). For
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  / `ts_widget` objects, `format` and `prettier_cmd` are supported (see
  details); other arguments are ignored.

## Value

For a
[`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
/ `ts_widget`, a character string of 'TypeScript'. For a file path,
writes `.ts` / `.R` beside `filename` and returns the output base path
invisibly.

## Details

**[`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
method:** `name` defaults to `deparse(substitute(f))` and sets the
generated `export const` symbol.

**Character (file) method:** `filename` is the base path for output;
`.R` and `.ts` extensions are appended. When omitted, output goes under
the default compile directory (see below) as `{basename(f)}.rserve`.
Arguments `filename`, `format`, and `prettier_cmd` must be passed by
name; they are not part of `...`.

Default output directory (CRAN-safe; does not write beside the source by
default):

1.  option `RserveTS.compile_dir` if set to a non-empty path;

2.  else environment variable `RSERVETS_COMPILE_DIR` if set;

3.  else [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

For local development, set e.g. `options(RserveTS.compile_dir = ".")` or
`RSERVETS_COMPILE_DIR=.` so `ts_compile("app.R")` writes `app.rserve.ts`
/ `app.rserve.R` in the working directory; or pass `filename`
explicitly.

- `format` – If `TRUE`, run 'Prettier' (or a compatible CLI) on the
  generated 'TypeScript' (returned string for functions; written `.ts`
  for files). Defaults to `getOption("RserveTS.format", FALSE)` so you
  can enable it once for a session (e.g. 'pkgdown' site builds) without
  changing call sites.

- `prettier_cmd` – Character vector argv (executable first). Defaults to
  `getOption("RserveTS.prettier_cmd")` (`NULL` until you set it, e.g.
  `options(RserveTS.prettier_cmd = c("prettier", "--parser", "typescript"))`).
  When still `NULL`, falls back to environment variable
  `RserveTS_PRETTIER_CMD` (space-separated tokens), then `prettier` or
  `npx prettier` on `PATH`. A temporary `.ts` copy of the generated
  source is appended as the last argument (as with
  `prettier --parser typescript path/to/file.ts`). Another formatter
  (e.g. 'Biome') can be used if it accepts that invocation pattern.

## Examples

``` r
# Compile a typed function to a 'TypeScript' schema string (no files written)
f <- ts_function(function(x = ts_integer(1)) x + 1L, result = ts_integer(1))
ts_compile(f)
#> export const f = Robj.ocap([z.number()], Robj.integer(1));

# File compilation writes under tempdir() by default (or RSERVETS_COMPILE_DIR)
src <- tempfile(fileext = ".R")
writeLines(
    "add <- ts_function(function(x = ts_integer(1)) x + 1L, result = ts_integer(1), export = TRUE)",
    src
)
out <- ts_compile(src)
file.exists(paste0(out, ".ts"))
#> [1] TRUE
file.exists(paste0(out, ".R"))
#> [1] TRUE
```
