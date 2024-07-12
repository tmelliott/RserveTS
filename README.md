
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

Writing functions is easy, just use the `ts::x()` functions to define
formals and return types.

*Note: we recommend not importing the library, and instead using the
fully qualified name `ts::x()` to avoid conflicts with other libraries.*

``` r
app <- ts::app(
  add = ts::fun(
    function(x = ts::number(), y = ts::number()) {
      result <- x + y
      ts::result(result, ts::number())
    }
  ),
  sample = ts::fun(
    function(x = ts::character_vector(), n = ts::integer()) {
      result <- sample(x, n)
      ts::result(result,
        ts::condition(n,
          1 = ts::character(),
          ts::character_vector()
        )
      )
    }
  )
)

ts::compile(app)
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
