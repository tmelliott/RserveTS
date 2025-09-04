#' @title
#' Types in R and TypeScript
#'
#' @description
#' This document provides an overview of the main types available to
#' app developers, which includes the main types in R and their
#' TypeScript counterparts.
#'
#' @details
#' # TS objects
#' The basic object in `RserveTS` is a `ts_object` class. This is represented by
#' input and return types, an optional default value, and a checking
#' function.
#'
#' Input types describe the `zod` schema of objects that TypeScript can pass
#' to Rserve functions.
#'
#' Return types describe the `zod` schema of objects that Rserve functions,
#' and most of these utilise the `Robj` utility types in the `rserve-ts`
#' library. The return types have additional properties added,
#' namedly `r_type` and `r_attributes`, handled by the `Robj` utility types.
#'
#' **Scalar versus array ("vector") types**:
#' In R, almost all types are vectors. In the 'rserve-js' library,
#' primitive arrays of length one are converted into scalars, which
#' leads to some issues with type checking when, for example, a return value
#' has unknown length. `which(x > 5)` is one such example.
#'
#' To solve this, we add an `n` argument to the `ts_*` functions. When `n = 1`,
#' the type takes the *scalar* form of the alue. When `n != 1`, the type takes
#' the *array* form of the value (this includes 0). Otherwise, the type
#' is the union of the scalar and array forms.
#'
#' This is the case for numbers, strings, and booleans.
#'
#' # Available types
#'
#' - `ts_boolean`: A boolean value. The array type is `Int8Array`.
#' - `ts_integer`: An integer value. The array type is `Int32Array`.
#'                 Javascript does not have a native integer type,
#'                 so scalars are represented as a number
#'                 (the same as `ts_numeric`).
#' - `ts_numeric`: A numeric value. The array type is `Float64Array`.*
#' - `ts_string`: A string value. The array type is `string[]`.
#' - `ts_factor`: A factor value. The array type is `(level1 | level2 | ... | levelN)[]`, and this type does not have a scalar form.
#' - `ts_list`: A list value, represented by a named object or an array.
#' - `ts_dataframe`: A data frame value, represented by a named object.
#' - `ts_null`: A null value.
#' - `ts_void`: A void value, used for specifying return types of functions
#'              that do not return a value.
#'
#'
#'
#' @family type documentation
#'
#' @name type_objects
NULL
