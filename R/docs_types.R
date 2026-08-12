#' Types in R and 'TypeScript'
#'
#' Constructors for typed values used in 'RserveTS' app contracts. Each
#' `ts_*()` helper returns a `ts_object` that describes the 'zod' / `Robj`
#' schema 'TypeScript' clients should expect for inputs and returns.
#'
#' @details
#' # TS objects
#' The basic object in 'RserveTS' is a `ts_object`. It carries an input type,
#' a return type, an optional default, and a `check()` helper used on the R
#' side during development.
#'
#' Input types describe the 'zod' schema of objects that 'TypeScript' can pass
#' to 'Rserve' functions. Return types describe the 'zod' schema of objects
#' that 'Rserve' functions return; most utilise the `Robj` helpers in the
#' 'rserve-ts' library (with `r_type` and `r_attributes`).
#'
#' # Scalar versus array ("vector") types
#' In R, almost all types are vectors. In the 'rserve-js' library, primitive
#' arrays of length one are converted into scalars, which leads to type
#' checking issues when a return value has unknown length (e.g. `which(x > 5)`).
#'
#' For vectors that support this distinction, pass `n`:
#' - `n = 1` -- scalar form
#' - `n != 1` (including `0`) -- array form
#' - default (`n = -1`) -- union of scalar and array forms
#'
#' This applies to logicals, integers, numerics, and characters.
#'
#' # Atomic types
#' - `ts_logical()`: logical / boolean. Array form: `Int8Array` /
#'   `Uint8Array`.
#' - `ts_integer()`: integer. Array form: `Int32Array`. 'JavaScript' has no
#'   native integer type, so scalars are numbers (same as `ts_numeric()`).
#' - `ts_numeric()`: numeric / double. Array form: `Float64Array`.
#' - `ts_character()`: character / string. Array form: `string[]`.
#' - `ts_factor()`: factor. Always a string array on the 'JavaScript' side
#'   (even for a single value); optional `levels` constrain the labels.
#'
#' # Structured types
#' - `ts_list()`: a list (named object or array in 'JavaScript'). Forms:
#'   1. Unknown list -- `ts_list()`
#'   2. Known named list -- `ts_list(x = ts_integer(), y = ts_character())`
#'      (object in 'JavaScript')
#'   3. Known unnamed list -- `ts_list(ts_integer(), ts_character())`
#'      (array in 'JavaScript')
#'   4. Named list of one value type with unknown keys -- use
#'      `ts_record(value_type)` (`Record<string, type>` in 'TypeScript')
#'   5. Unnamed list of one value type with unknown length --
#'      `ts_list(ts_integer())`-style homogeneous arrays (`Array<type>`)
#' - `ts_record()`: named list whose values share one type
#'   (`Record<string, value_type>`).
#' - `ts_dataframe()`: data frame; named columns of equal length.
#'
#' # Nullish and combinators
#' - `ts_null()`: only `NULL`.
#' - `ts_void()`: return type for functions that return nothing (prefer this
#'   over `ts_null()` for return types).
#' - `ts_undefined()`: 'JavaScript' `undefined`.
#' - `ts_union()`: union of several types.
#' - `ts_optional()`: `type` or undefined (wrapper around `ts_union()` /
#'   `ts_undefined()`).
#' - `ts_array()`: array of a typed element (or a 'zod' fragment string).
#'
#' See [ts_recursive_list()] for self-referential list schemas, and
#' [js_function()] for 'JavaScript' callbacks callable from R.
#'
#' @param n Length of the vector for atomic types. If `n = 1`, a single
#'   value is expected; if `n = 0`, any length; if `n > 1`, a vector of that
#'   length. The default (`-1`) accepts scalar or array form.
#' @param default Default value for the type (optional).
#' @param levels For `ts_factor()`: character vector of allowed levels
#'   (optional).
#' @param ... For `ts_list()` / `ts_dataframe()`: member types (named or
#'   unnamed for lists; named for data frames). For `ts_union()`: type
#'   objects to merge.
#' @param value_type For `ts_record()`: a single ts type for all values
#'   (e.g. `ts_character(1)`).
#' @param type For `ts_optional()` / `ts_array()`: the inner type. For
#'   `ts_array()`, may also be a 'zod'-style string such as `"z.number()"`.
#'
#' @return A `ts_object` describing the type (except `ts_array()` on a
#'   character 'Zod' fragment, which returns a character schema string).
#'
#' @examples
#' (x <- ts_numeric(1))
#' (person <- ts_list(name = ts_character(1), age = ts_integer(1)))
#' (df <- ts_dataframe(a = ts_integer(1), b = ts_character(1)))
#' (labels <- ts_record(ts_character(1)))
#' (ts_union(ts_numeric(1), ts_character(1)))
#'
#' @seealso [ts_object()], [ts_recursive_list()], [js_function()]
#' @family type documentation
#' @name type_objects
#' @md
NULL
