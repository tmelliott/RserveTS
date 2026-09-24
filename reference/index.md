# Package index

## Building apps

Writing and compiling functions into Rserve apps.

- [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  : Define a typed function

- [`ts_app()`](https://tomelliott.co.nz/RserveTS/reference/ts_app.md) :

  Generate an 'Rserve' app from a
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)

- [`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
  : Compile R functions

- [`ts_deploy()`](https://tomelliott.co.nz/RserveTS/reference/ts_deploy.md)
  : Deploy a typed 'Rserve' app

- [`widgetActions()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  [`createWidget()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  [`observer()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  : Create 'TypeScript'-compatible widgets

## Types

Type helper functions for defining argument and return types.

- [`ts_union()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_optional()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_array()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_logical()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_integer()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_numeric()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_character()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_factor()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_list()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_record()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_dataframe()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_null()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_void()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  [`ts_undefined()`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)
  : Types in R and 'TypeScript'
- [`ts_object()`](https://tomelliott.co.nz/RserveTS/reference/ts_object.md)
  [`is_ts_object()`](https://tomelliott.co.nz/RserveTS/reference/ts_object.md)
  [`get_type()`](https://tomelliott.co.nz/RserveTS/reference/ts_object.md)
  [`check_type()`](https://tomelliott.co.nz/RserveTS/reference/ts_object.md)
  : Typed object
- [`ts_recursive_list()`](https://tomelliott.co.nz/RserveTS/reference/ts_recursive_list.md)
  [`ts_self()`](https://tomelliott.co.nz/RserveTS/reference/ts_recursive_list.md)
  : Recursive list
- [`js_function()`](https://tomelliott.co.nz/RserveTS/reference/js_function.md)
  : 'JavaScript' functions callable from R

## Debugging

Debugging utilities for RserveTS.

- [`rts_debug`](https://tomelliott.co.nz/RserveTS/reference/rts_debug.md)
  : Debug logging for 'RserveTS'
- [`rts_debug_enabled()`](https://tomelliott.co.nz/RserveTS/reference/rts_debug_enabled.md)
  : Check if debug logging is enabled for a tag
- [`rts_log()`](https://tomelliott.co.nz/RserveTS/reference/rts_log.md)
  : Log a debug message
