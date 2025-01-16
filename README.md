
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ts

<!-- badges: start -->

<!-- badges: end -->

The **ts** package makes it easy for users to write functions that can
be used in [**rserve-ts**](https://www.npmjs.com/package/rserve-ts)
applications.

## Installation

You can install the development version of ts from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tmelliott/ts")
```

## Example

Writing functions is easy, just use the `ts_*()` functions to define
formals and return types.

``` r
library(ts)
addFn <- ts_function(
  function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
  result = ts_numeric(1)
)
sampleFn <- ts_function(
  function(x = ts_character(), n = ts_integer(1)) sample(x, n),
  result = ts_character()
)
app <- ts_function(
  function() {
    list(
      add = addFn,
      sample = sampleFn
    )
  },
  result = ts_list(
    add = appFn,
    sample = sampleFn
  )
)

ts_compile(app)
```

This will generate the following rserve-ts function definitions:

``` typescript
import { types as R } from "rserve-ts";

export const app = {
  add: z.function(
    z.tuple([z.number(), z.number()]),
    z.promise(R.numeric(1))
  ),
  sample: z.function(
    z.tuple([z.array(z.string()), z.integer()]),
    z.promise(R.character())
  )
};
```

which will generate the following types:

``` typescript
type App = {
  add: (x: number, y: number) => Promise<Robj.Numeric<1>>;
  sample: (x: string[], n: number) => Promise<Robj.Character>;
};
```

## State of the project

Here’s what’s currently working:

``` r
library(ts)

myfun <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
myfun(1:5)
#> Error in myfun(1:5): could not find function "myfun"

myfun("hello world")
#> Error in myfun("hello world"): could not find function "myfun"

cat(readLines("tests/testthat/app.R"), sep = "\n")
#> library(ts)
#> 
#> fn_mean <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
#> fn_first <- ts_function(function(x) x[1],
#>     x = ts_character(-1), result = ts_character(1)
#> )
#> 
#> sample_num <- ts_function(
#>     sample,
#>     x = ts_numeric(0),
#>     result = ts_numeric(1)
#> )

ts_compile("tests/testthat/app.R", file = "")
#> import {  } from 'rserve-ts';
#> 
#> character(0)
#> character(0)
#> character(0)
```

## TODO

  - [ ] Add support for more types

  - [ ] Allow generic types (e.g., `<T>(x: T) => T`)

  - [ ] Add support for conditional return types
    
    e.g., `const sample = <T, N extends number>(x: T[], n: N) => N
    extends 1 ? T : T[]`

  - [ ] Function overloads? Perhaps just a wrapper around several
    function definitions…
