
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ts

<!-- badges: start -->

<!-- badges: end -->

The **ts** package makes it easy for users to write functions that can
be used in **rserve-ts** applications.

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
app <- ts_app(
  add = ts_fun(
    function(x, y) {
      x + y
    },
    x = ts_number(),
    y = ts_number(),
    # ideally this will use a generic type where x OR y can be vectors
    # and, if one is a vector, the return type will be a vector too...
    result = ts_number()
  ),
  sample = ts_fun(
    function(x, n) {
      sample(x, n)
    },
    x = ts_character_vector(),
    n = ts_integer(),
    result = ts_condition(n,
      1 = ts_character(),
      ts_character_vector()
    )
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
    z.tuple([z.character_vector(), z.integer()]),
    z.promise(R.character())
  )
};
```

which will generate the following types:

``` typescript
type App = {
  add: (x: number, y: number) => Promise<{ data: number }>;
  sample: (x: string[], n: number) => Promise<{ data: string | string[] }>;
  // or, if possible, even better:
  sample: <N extends number>(x: string[], n: N) =>
    Promise<{ data: N extends 1 ? string : string[] }>;
};
```

## State of the project

Here’s what’s currently working:

``` r
library(ts)

myfun <- ts_function(mean, x = ts_numeric(), result = ts_numeric())
ts_compile(myfun)
#> const myfun = (x: number | number[]) => numeric();
```
