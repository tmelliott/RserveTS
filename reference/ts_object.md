# Typed object (internal use only)

This is the base type for all typed objects, and can be used to define
custom types.

## Usage

``` r
ts_object(
  input_type = "any",
  return_type = "any",
  default = NULL,
  check = function() stop("Not implemented"),
  generic = FALSE
)

is_ts_object(x)

get_type(x, which = c("input", "return"))

check_type(type, x)
```

## Arguments

- input_type:

  The type of the object that Typescript expect to send to R.

- return_type:

  The type of the object that Typescript expects to recieve from R.

- default:

  The default value of the object.

- check:

  A function that checks the object and returns it if it is valid. This
  operates on the R side and is mostly for development and debugging
  purposes. It is up to the developer to ensure that all functions
  return the correct type of object always.

- generic:

  logical, if `TRUE` then the object is a generic type.

- x:

  An object

- which:

  Which type to get, either "input" or "return"

- type:

  A ts object

## Functions

- `is_ts_object()`: Check if an object is a ts object

- `get_type()`: Get the input type of a ts object

- `check_type()`: Check if an object has the correct type
