# RserveTS: Deploy Rserve Applications Supporting TypeScript

The **RserveTS** package makes it easy for users to write functions that
can be used in [**rserve-ts**](https://www.npmjs.com/package/rserve-ts)
applications.

## Installation

You can install the development version of `RserveTS` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tmelliott/RserveTS")
```

## Example

Writing functions is easy, just use the `ts_*()` functions to define
formals and return types.

``` r
# demo.R
library(RserveTS)
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
    add = addFn,
    sample = sampleFn
  )
)

# TODO: specify exactly which functions to export in the entry point
# ts_export(app)
```

Then use
[`ts_compile()`](http://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
to generate the TypeScript schemas:

``` typescript
import { Robj } from 'rserve-ts';
import { z } from 'zod';

const addFn = Robj.ocap([z.number(), z.number()], Robj.numeric(1));
const app = Robj.ocap([],
  Robj.list({
    add: Robj.ocap(),
    sample: Robj.ocap()
  })
);
const sampleFn = Robj.ocap(
  [z.union([z.string(), z.array(z.string())]), z.number()],
  Robj.character()
);

export default {
  addFn,
  app,
  sampleFn
};
```

You can then import this into your
[rserve-ts](https://www.npmjs.com/package/rserve-ts) application. See
`tests/testthat/sampler` for an example.

It is also possible to generate a sourceable file to deploy an Rserve
instance with your app code using
[`ts_deploy()`](http://tomelliott.co.nz/RserveTS/reference/ts_deploy.md):

``` r
ts_deploy(app)
# run with: Rscript app.rserve.R
```
