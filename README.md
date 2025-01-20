
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

Besides generating the schema as shown above, the app object can also be
‘deployed’ using Rserve:

``` r
ts_deploy(app, port = 6311, daemon = FALSE)
# listening on port 6311
```

## State of the project

Here’s what’s currently working:

``` r
library(ts)

myfun <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
myfun$call(1:5)
#> [1] 3

myfun$call("hello world")
#> Error: Invalid argument 'x': Expected a number

cat(readLines("tests/testthat/app.R"), sep = "\n")
#> library(ts)
#> 
#> fn_mean <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
#> fn_first <- ts_function(function(x = ts_character(-1)) x[1],
#>     result = ts_character(1)
#> )
#> 
#> sample_num <- ts_function(
#>     sample,
#>     x = ts_numeric(0),
#>     result = ts_numeric(1)
#> )

ts_compile("tests/testthat/app.R", file = "")
#> import { Robj } from 'rserve-ts';
#> import { z } from 'zod';
#> 
#> 
#> const fn_first = Robj.ocap([z.union([z.string(), Robj.character(0)])], Robj.character(1));
#> const fn_mean = Robj.ocap([z.union([z.number(), z.instanceof(Float64Array)])], Robj.numeric(1));
#> const sample_num = Robj.ocap([z.instanceof(Float64Array)], Robj.numeric(1));
```
