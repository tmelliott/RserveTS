# Deploy a ts Rserve app

Deploy a ts Rserve app

## Usage

``` r
ts_deploy(
  f,
  file = sprintf("%s.rserve.R", tools::file_path_sans_ext(f)),
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

  The file to write the deployment script to

- init:

  Names of objects (ts_functions) to make available to the
  initialisation function

- port:

  The port to deploy the app on

- run:

  Whether to run the deployment script, takes values "no", "here",
  "background"

- silent:

  Whether to print the deployment script

## Value

NULL, called to open an Rserve instance
