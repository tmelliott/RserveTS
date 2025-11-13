# Compile R functions

Generates TypeScript schema for the given R function or file path. If a
path, the R app is also generated.

## Usage

``` r
ts_compile(f, ..., name, filename)
```

## Arguments

- f:

  A function or file path

- ...:

  Additional arguments (passed to ts_deploy)

- name:

  The name of the function

- filename:

  The base file path to write the TypeScript schema and R app to
  (optional, uses `[path of f].rserve` by default). `.R` and `.ts` file
  extensions are appended automatically. If `""`, the output is printed
  to the standard output console (see `cat`).

## Value

Character vector of TypeScript schema, or NULL if writing to file
