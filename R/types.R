#' Typed object (internal use only)
#'
#' This is the base type for all typed objects, and can be used to define
#' custom types.
#'
#' @param input_type The type of the object that 'TypeScript' expects to send to R.
#' @param return_type The type of the object that 'TypeScript' expects to receive from R.
#' @param default The default value of the object.
#' @param check A function that checks the object and returns it if it is valid. This operates on the R side and is mostly for development and debugging purposes. It is up to the developer to ensure that all functions return the correct type of object always.
#' @param generic logical, if `TRUE` then the object is a generic type.
#' @return A `ts_object` environment with 'Zod' input/return schema strings and a
#'   `check()` helper. `is_ts_object()` returns a logical;
#'   `get_type()` returns a character schema string;
#'   `check_type()` returns `x` when valid (or errors).
#'
#' @md
#' @examples
#' x <- ts_numeric(1)
#' is_ts_object(x)
#' get_type(x, "input")
ts_object <- function(input_type = "any",
                      return_type = "any",
                      default = NULL,
                      check = function() stop("Not implemented"),
                      generic = FALSE) {
    e <- environment()

    e$attr <- function(name, value) {
        e$attributes[[name]] <- value
    }

    class(e) <- c("ts_object", class(e))
    e
}

#' @export
print.ts_object <- function(x, ...) {
    # name <- deparse(substitute(x))
    h3("Input type: ")
    if (nchar(x$input_type) > 50) {
        cat(format_js(x$input_type), "\n")
    } else {
        cat(x$input_type, "\n")
    }
    h3("Return type: ")
    if (nchar(x$return_type) > 50) {
        cat(format_js(x$return_type), "\n")
    } else {
        cat(x$return_type, "\n")
    }
    cat("\n")
}

#' @describeIn ts_object Check if an object is a ts object
#' @export
is_ts_object <- function(x) {
    inherits(x, "ts_object")
}

#' @describeIn ts_object Get the input type of a ts object
#' @param x A ts object
#' @param which Which type to get, either "input" or "return"
#' @export
get_type <- function(x, which = c("input", "return")) {
    UseMethod("get_type")
}

#' @export
get_type.ts_object <- function(x, which = c("input", "return")) {
    which <- match.arg(which)
    if (which == "input") {
        return(x$input_type)
    }
    x$return_type
}

#' @export
get_type.ts_ts_list <- function(x, which = c("input", "return")) {
    which <- match.arg(which)
    if (which == "input") {
        return(x$input_type)
    }
    vals <- x$.__ts_list_values
    if (!length(vals)) {
        return(x$return_type)
    }
    type_funs <- vapply(vals, get_type, character(1), which = "return")
    if (!is.null(names(vals))) {
        type_fn <- sprintf(
            "{ %s }",
            paste(names(vals), type_funs, sep = ": ", collapse = ", ")
        )
        return(if (identical(type_fn, "{ }")) "Robj.list()" else sprintf("Robj.list(%s)", type_fn))
    }
    if (length(vals) == 1L) {
        return(sprintf("Robj.list(%s)", type_funs[[1L]]))
    }
    sprintf("Robj.list([%s])", paste(type_funs, collapse = ", "))
}

#' @export
get_type.ts_function <- function(x, which = c("input", "return")) {
    which <- match.arg(which)
    if (which == "input") {
        return("z.function()")
    }

    compile_fn(x)
    # "Robj.ocap()"
}

#' @export
get_type.ts_widget <- function(x, which = c("input", "return")) {
    which <- match.arg(which)
    if (which == "input") {
        return("z.function()")
    }
    ref <- attr(x, ".__ts_schema_ref", exact = TRUE)
    if (is.character(ref) && length(ref) == 1L && nzchar(ref)) {
        return(ref)
    }
    compile_fn(x)
}

#' @export
get_type.default <- function(x, which) {
    stop("Invalid object")
}

#' @describeIn ts_object Check if an object has the correct type
#' @param type A ts object
#' @param x An object
#' @export
check_type <- function(type, x) UseMethod("check_type")

#' @export
check_type.default <- function(type, x) {
    stop("Invalid object")
}

#' @export
check_type.ts_object <- function(type, x) {
    type$check(x)
}

#' @export
check_type.ts_function <- function(type, x) {
    if ("ts_function" %in% class(x)) {
        return(x)
    }
    if (!is.function(x)) stop("Expected a function")
    do.call(
        ts_function,
        c(
            list(x), attr(type, "args"),
            list(result = attr(type, "result"))
        )
    )
}

#' Union type
#'
#' Create a union of types. Currently this only accepts schemas as strings.
#' @param ... Type objects to merge
#' @param default Default value for the type (optional).
#' @return A `ts_object` representing a 'Zod' union of the given types.
#' @export
#' @md
#' @examples
#' x <- ts_union(ts_numeric(1), ts_character(1))
ts_union <- function(..., default = NULL) {
    types <- list(...)

    ts_object(
        sprintf(
            "z.union([%s])",
            paste(sapply(types, \(x) x$input_type), collapse = ", ")
        ),
        sprintf(
            "z.union([%s])",
            paste(sapply(types, \(x) x$return_type), collapse = ", ")
        ),
        # paste(sapply(types, \(x) x$return_type), collapse = " | "),
        default = default,
        check = function(x = NULL) {
            any_pass <- FALSE
            for (t in types) {
                r <- try(t$check(x), silent = TRUE)
                if (!inherits(r, "try-error")) {
                    any_pass <- TRUE
                    break
                }
            }
            if (!any_pass) stop("No types match")
            x
        }
    )
}

#' Optional type
#'
#' A wrapper around union of a type and undefined
#' @param type Type that is optional
#' @return A `ts_object` that accepts either `type` or undefined.
#' @export
#' @md
#' @examples
#' x <- ts_optional(ts_numeric(1))
ts_optional <- function(type) {
    ts_union(type, ts_undefined())
}

#' Array type
#'
#' An array of typed objects. In 'zod', these are represented
#' by `z.array()`; returned objects must be R lists, `Robj.list()`.
#'
#' @param type The input type, either a 'zod'-style string ("z.number()") or a `ts_object`.
#' @return For a `ts_object` input, a `ts_object` wrapping `z.array(...)`.
#'   For a character 'Zod' fragment, a character string schema.
#' @md
#' @export
#' @examples
#' x <- ts_array(ts_numeric(1))
#' x$check(list(1, 2, 3))
ts_array <- function(type) {
    UseMethod("ts_array")
}

#' @export
ts_array.character <- function(type) {
    if (type == "z.number()") {
        return("z.instanceof(Float64Array)")
    }
    if (type == "z.boolean()") {
        return("z.instanceof(Uint8Array)")
    }
    sprintf("z.array(%s)", type)
}

#' @export
ts_array.ts_object <- function(type) {
    ts_object(
        sprintf("z.array(%s)", type$input_type),
        sprintf("z.array(%s)", type$return_type),
        check = function(x) {
            if (!is.list(x)) stop("Must be a list")
            lapply(x, type$check)
        }
    )
}

#' @export
ts_array.default <- function(type) stop("Invalid type")

n_type <- function(n, type, pl = ts_array(type)) {
    if (n == 1) {
        return(type)
    }
    if (n == -1) {
        return(sprintf("z.union([%s, %s])", type, pl))
    }
    pl
}
n_type_fun <- function(n, type) {
    sprintf("%s(%s)", type, ifelse(n < 0, "", n))
}

#' Logical or boolean type
#'
#' Booleans are represented in 'Zod' schema as either a boolean (`z.boolean()`),
#' or a typed Uint8Array (`z.instanceof(Uint8Array)`).
#'
#' @param n The length of the boolean vector. If `n = 1` then a single boolean is expected. If `n = 0` then any length is expected. If `n > 1` then a boolean vector of length `n` is expected.
#' @param default Default value for the type (optional).
#' @return A ts object that accepts logical scalars or vectors of length `n`.
#' @export
#' @md
#' @examples
#' x <- ts_logical(1)
#' x$check(TRUE)
#'
#' \dontrun{
#' # this will fail
#' x$check(5)
#' }
ts_logical <- function(n = -1L, default = NULL) {
    ts_object(
        n_type(n, "z.boolean()"),
        n_type_fun(n, "Robj.logical"),
        default = default,
        check = function(x) {
            if (!is.logical(x)) stop("Expected a boolean")
            if (n > 0 && length(x) != n) {
                stop("Expected a boolean of length ", n)
            }
            x
        }
    )
}

#' Integer type
#'
#' Integers are represented in 'Zod' schema as either a number (`z.number()`),
#' or a Int32Array (`z.instanceof(Int32Array)`).
#'
#' @param n The length of the integer vector. If `n = 1` then a single integer is expected. If `n = 0` then any length is expected. If `n > 1` then an integer vector of length `n` is expected.
#' @param default Default value for the type (optional).
#' @return A ts object that accepts integer scalars or vectors of length `n`.
#' @export
#' @md
#' @examples
#' x <- ts_integer(1)
#' x$check(1L)
#'
#' \dontrun{
#' # this will fail
#' x$check(1:10)
#' }
ts_integer <- function(n = -1L, default = NULL) {
    ts_object(
        n_type(n, "z.number()", "z.instanceof(Int32Array)"),
        n_type_fun(n, "Robj.integer"),
        default = default,
        check = function(x) {
            if (!is.numeric(x)) stop("Expected a number")
            if (!all.equal(x, as.integer(x))) {
                # javascript only has one number type
                stop("Expected an integer")
            }
            if (n > 0 && length(x) != n) {
                stop("Expected an integer of length ", n)
            }
            x
        }
    )
}

#' Numeric type
#'
#' Numbers are represented in 'Zod' schema as either a number (`z.number()`),
#' or a Float64Array (`z.instanceof(Float64Array)`).
#'
#' @param n The length of the numeric vector. If `n = 1` then a single number is expected. If `n = 0` then any length is expected. If `n > 1` then a numeric vector of length `n` is expected.
#' @param default Default value for the type (optional).
#' @return A ts object that accepts numeric scalars or vectors of length `n`.
#' @export
#' @md
#' @examples
#' x <- ts_numeric(1)
#' x$check(1)
#'
#' \dontrun{
#' # this will fail
#' x$check(c(1, 2, 3))
#' x$check("a")
#' }
ts_numeric <- function(n = -1L, default = NULL) {
    ts_object(
        n_type(n, "z.number()"),
        n_type_fun(n, "Robj.numeric"),
        default = default,
        check = function(x) {
            if (!is.numeric(x)) stop("Expected a number", call. = FALSE)
            if (n > 0 && length(x) != n) {
                stop("Expected a number of length ", n, call. = FALSE)
            }
            x
        }
    )
}

#' Character or string type
#'
#' Strings are represented in 'Zod' schema as either a string (`z.string()`),
#' or a string array (`z.array(z.string())`).
#' @param n The length of the string vector. If `n = 1` then a single string is expected. If `n = 0` then any length is expected. If `n > 1` then a string vector of length `n` is expected.
#' @param default Default value for the type (optional).
#' @return A ts object that accepts strings or string vectors of length `n`.
#' @export
#' @md
#' @examples
#' x <- ts_character(1)
#' x$check("a")
ts_character <- function(n = -1L, default = NULL) {
    ts_object(
        n_type(n, "z.string()"),
        n_type_fun(n, "Robj.character"),
        default = default,
        check = function(x) {
            if (!is.character(x)) stop("Expected a string")
            if (n > 0 && length(x) != n) stop("Expected a string of length ", n)
            x
        }
    )
}

vector_as_ts_array <- function(x) {
    paste("[\"", paste(x, collapse = "\", \""), "\"]", sep = "")
}

#' Typed factor
#'
#' Factors are integers with labels. On the JS side, these are *always* represented as a string array (even if only one value - yay!).
#'
#' @param levels A character vector of levels (optional).
#' @param default Default value for the type (optional).
#' @return A ts object that accepts factors with the specified levels.
#'
#' @export
#' @md
#' @examples
#' x <- ts_factor(levels = c("a", "b"))
#' x$check(factor("a", levels = c("a", "b")))
#'
#' \dontrun{
#' # this will fail
#' x$check("a")
#' x$check(factor("c", levels = c("a", "b", "c")))
#' }
ts_factor <- function(levels = NULL, default = NULL) {
    ts_object(
        ifelse(is.null(levels),
            ts_array("z.string()"),
            sprintf("z.enum([%s]).array()", paste(paste("\"", levels, "\"", sep = ""), collapse = ", "))
        ),
        if (is.null(levels)) {
            "Robj.factor()"
        } else {
            sprintf("Robj.factor(%s)", vector_as_ts_array(levels))
        },
        default = default,
        check = function(x) {
            if (!is.factor(x)) stop("Expected a factor")
            if (!is.null(levels) && !identical(levels, levels(x))) {
                stop(
                    "Expected a factor with levels: ",
                    paste(levels, collapse = ", ")
                )
            }
            x
        }
    )
}

# table?

#' Typed list
#'
#' A list is a vector of other robjects, which may or may not be named.
#'
#' There are five types of lists we can define:
#'
#' 1. Unknown list
#' 2. Known, named list (e.g., list(x = 1:5, y = 'hello world')). This is an object in 'JavaScript'.
#' 3. Known, unnamed list (e.g., list(1:5, 'hello world')). This is an array in 'JavaScript'.
#' 4. Named list of a single datatype (e.g., list(fit1 = lm(...), fit2 = lm(...), ...)), where the names and length are not known ahead of time. This is a `Record<string, type>` in 'JavaScript'. Use \code{\link{ts_record}(value_type)} for this case.
#' 5. Unnamed list of a single datatype (e.g., list(lm(...), lm(...), ...)), where the length is unknown ahead of time. This is an `Array<type>` in 'JavaScript'.
#'
#' @param ... A list of types, named or unnamed.
#' @param default Default value for the type (optional).
#' @return A ts object that accepts lists with the specified types.
#'
#' @export
#' @md
#'
#' @examples
#' x <- ts_list(a = ts_integer(1), b = ts_character(1))
#' x$check(list(a = 1L, b = "a"))
ts_list <- function(..., default = NULL) {
    values <- list(...)

    type <- "z.union([z.object({}), z.array(z.any())])"
    type_fn <- ""
    if (length(values)) {
        types <- sapply(values, get_type, which = "input")
        type_funs <- sapply(values, get_type, which = "return")
        if (!is.null(names(values))) {
            type <- sprintf(
                "z.object({ %s })",
                paste(names(values), types, sep = ": ", collapse = ", ")
            )
            type_fn <- sprintf(
                "{ %s }",
                paste(names(values), type_funs, sep = ": ", collapse = ", ")
            )
        } else {
            type <- sprintf(
                ifelse(length(values) == 1, "z.array(%s)", "z.tuple([%s])"),
                paste(types, collapse = ", ")
            )
            type_fn <- sprintf(
                ifelse(length(values) == 1, "%s", "[%s]"),
                paste(type_funs, collapse = ", ")
            )
        }
    }

    out <- ts_object(
        type,
        ifelse(type_fn == "", "Robj.list()",
            sprintf("Robj.list(%s)", type_fn)
        ),
        default = default,
        check = function(x) {
            if (!is.list(x)) stop("Expected a list")
            if (!is.null(values)) {
                if (!is.null(names(values))) {
                    if (!identical(names(x), names(values))) {
                        stop(
                            "Expected a list with names: ",
                            paste(names(values), collapse = ", ")
                        )
                    }
                }
                for (i in seq_along(values)) {
                    x[[i]] <- check_type(values[[i]], x[[i]])
                }
            }
            x
        }
    )
    if (length(values)) {
        out$.__ts_list_values <- values
        class(out) <- c("ts_ts_list", class(out))
    }
    out
}


#' Record type (named list of a single value type)
#'
#' A list whose element names are unknown at compile time but whose values
#' all have the same type. In 'TypeScript' this is `Record<string, value_type>`.
#' Use this for e.g. `getAvailablePlotTypes()` returning list(default = "default", scatter = "scatter").
#' For "named list of a single datatype" (case 4 in \code{ts_list()}), use \code{ts_record(value_type)}.
#'
#' @param value_type A single ts type (e.g. ts_character(1), ts_integer(1)).
#' @param default Default value for the type (optional).
#' @return A ts object that accepts named lists with values of the given type.
#' @export
#' @md
#' @examples
#' x <- ts_record(ts_character(1))
#' x$check(list(a = "x", b = "y"))
ts_record <- function(value_type, default = NULL) {
    stopifnot(is_ts_object(value_type))

    input_type <- sprintf("z.record(z.string(), %s)", value_type$input_type)
    return_type <- sprintf("Robj.list(z.record(z.string(), %s))", get_type(value_type, "return"))

    ts_object(
        input_type,
        return_type,
        default = default,
        check = function(x) {
            if (!is.list(x)) stop("Expected a list")
            if (is.null(names(x))) stop("Expected a named list")
            for (i in seq_along(x)) {
                x[[i]] <- check_type(value_type, x[[i]])
            }
            x
        }
    )
}


#' Typed dataframe
#'
#' This is essentially a list, but the elements must have names and are all the same length.
#'
#' @param ... Named types.
#' @param default Default value for the type (optional).
#' @return A ts object that accepts data frames with the specified types.
#'
#' @export
#' @md
#'
#' @examples
#' x <- ts_dataframe(a = ts_integer(1), b = ts_character(1))
#' x$check(data.frame(a = 1L, b = "a"))
ts_dataframe <- function(..., default = NULL) {
    values <- list(...)
    type <- "z.record(z.string(), z.any())"
    type_fn <- ""
    if (length(values)) {
        types <- sapply(values, get_type, which = "input")
        type_funs <- sapply(values, get_type, which = "return")
        if (is.null(names(values))) stop("Expected named elements")

        type <- sprintf(
            "z.object({\n  %s\n})",
            paste(names(values), types, sep = ": ", collapse = ",\n  ")
        )
        type_fn <- sprintf(
            "{\n  %s\n}",
            paste(names(values), type_funs, sep = ": ", collapse = ",\n  ")
        )
    }

    ts_object(
        type,
        sprintf("Robj.dataframe(%s)", type_fn),
        default = default,
        check = function(x) {
            if (!is.data.frame(x)) stop("Expected a data frame")
            x
        }
    )
}

#' Null type
#'
#' This is a type that only accepts `NULL`. For function return types, use `ts_void()`.
#'
#' @return A ts object that only accepts `NULL`.
#' @export
#'
#' @md
#' @examples
#' x <- ts_null()
#' x$check(NULL)
ts_null <- function() {
    ts_object(
        "z.null()",
        "Robj.null()",
        check = function(x) {
            if (!is.null(x)) stop("Expected NULL")
            x
        }
    )
}

#' Void type
#'
#' This is a type that accepts null values (this would typically be used for
#' functions that return nothing).
#' @return A ts object that accepts `NULL`.
#' @export
#' @md
#' @seealso \code{\link{ts_null}()}
ts_void <- function() {
    ts_object(
        "z.void()",
        "Robj.null()",
        check = function(x) {
            return(NULL)
        }
    )
}

#' Undefined type
#'
#' For the undefined type.
#' @return A ts object that accepts 'undefined'.
#' @export
#' @md
ts_undefined <- function() {
    ts_object(
        "z.undefined()",
        "Robj.null()",
        check = function(x = NULL) {
            if (missing(x) || is.null(x)) {
                return()
            }
            stop("Expecting nothing.")
        }
    )
}

#' Recursive list
#'
#' For complex recursive lists — objects that can contain subcomponents of the
#' same (parent) type. For example, a Person with `name` and optional
#' `children` that are themselves Person objects.
#'
#' Use [ts_self()] inside `recur` to mark those self-referential fields.
#' By default `ts_self()` means an array of the parent type; use
#' `ts_self(1)` for a single nested object.
#'
#' Defining this type in 'Zod' is currently complicated, as the type has to be
#' pre-defined, and then extended after manually defining the Type. In an
#' upcoming version of 'zod' 4, this should be simplified. For now, it's tricky.
#'
#' @param values properties that define the base schema of the list;
#'               must be a named list.
#' @param recur a named list of properties that are added.
#'              These can use [ts_self()].
#' @param n For `ts_self()`: number of elements — `n = 1` for a single nested
#'   object, or `n != 1` (default `-1`) for an array of the parent type.
#' @return `ts_recursive_list()` returns a ts object that accepts recursive
#'   lists. `ts_self()` returns a marker used in `recur`.
#' @export
#' @md
#' @examples
#' person <- ts_recursive_list(
#'     list(name = ts_character(1)),
#'     list(children = ts_self())
#' )
#' echo_person <- ts_function(function() person, result = person)
#' ts_compile(echo_person, name = "echo_person")
ts_recursive_list <- function(values, recur) {
    if (length(values) == 0) stop("Must specify values")
    if (is.null(names(values))) stop("Input must be named")
    if (!is.list(values)) stop("values must be a list")

    # first define the base schema (*must* be a Zod object)
    types <- sapply(values, get_type, which = "input")
    type_funs <- sapply(values, get_type, which = "return")

    base_type <- sprintf(
        "const baseObjectSchema = z.object({\n  %s \n});",
        paste(names(values), types, sep = ": ", collapse = ",\n  ")
    )
    # base_type_fn <- sprintf(
    #     "Robj.list({\n  %s \n})",
    #     paste(names(values), type_funs, sep = ": ", collapse = ",\n  ")
    # )
    # cat("\n\n")
    # cat(base_type_fn)

    # next create the Type with recursive properties
    recur_types <- gsub("__self", "self",
        gsub("__self[]", "__self.array()",
            sapply(recur, get_type, which = "input"),
            fixed = TRUE
        ),
        fixed = TRUE
    )
    recur_zod <- gsub("__self", "ObjectType",
        sapply(recur, get_type, which = "input"),
        fixed = TRUE
    )
    # recur_type_funs <- sapply(values, get_type, which = "return")

    obj_type <- sprintf(
        "type ObjectType = z.infer<typeof baseObjectSchema> & {\n  %s\n};",
        paste(names(recur_zod), recur_zod, sep = ": ", collapse = ",\n  ")
    )

    # then create the lazy zod definition
    zod_type <- sprintf(
        paste(
            sep = "\n",
            "const listType = Robj.recursive_list<ObjectType>(",
            "  baseObjectSchema,",
            "  (self) => ({",
            "    %s",
            "  })",
            ");"
        ),
        paste(
            names(recur_zod),
            paste(recur_types, "optional()", sep = "."),
            sep = ": ", collapse = ",\n    "
        )
    )

    # finally put together the IFFE
    result <- sprintf(
        "(function () {\n  %s\n  %s\n  %s\n  return listType;\n})()",
        gsub("\n", "\n  ", base_type, fixed = TRUE),
        gsub("\n", "\n  ", obj_type, fixed = TRUE),
        gsub("\n", "\n  ", zod_type, fixed = TRUE)
    )

    # recur <- lapply(recur, \(x) {
    #     if (grepl("[]", x, fixed = TRUE)) {
    #         ts_list(ts_self(1))
    #     } else {
    #         x
    #     }
    # })

    SELF <- ts_object(
        "undefined",
        result,
        check = function(x) {
            if (!is.list(x)) stop("Expected a list")
            if (!is.null(values)) {
                if (!is.null(names(values))) {
                    # if (!identical(names(x), names(c(values, recur)))) {
                    if (!all(names(values) %in% names(x))) {
                        stop(
                            "Expected a list with names: ",
                            paste(names(values), collapse = ", ")
                        )
                    }
                }
                for (i in seq_along(values)) {
                    n <- names(values)[i]
                    x[[n]] <- check_type(values[[n]], x[[n]])
                }

                # check recurring types
                for (i in seq_along(recur)) {
                    n <- names(recur)[i]

                    if ("ts_self" %in% class(recur[[n]])) {
                        if (grepl("[]", recur[[n]], fixed = TRUE)) {
                            x[[n]] <- lapply(x[[n]], \(z) {
                                check_type(SELF, z)
                            })
                        } else {
                            x[[n]] <- check_type(SELF, x[[n]])
                        }
                    } else {
                        x[[n]] <- check_type(recur[[n]], x[[n]])
                    }
                }
            }
            x
        }
    )
    SELF
}

#' @rdname ts_recursive_list
#' @export
ts_self <- function(n = -1L) {
    structure(
        ifelse(n == 1, "__self", "__self[]"),
        class = "ts_self"
    )
}

#' @export
get_type.ts_self <- function(x, which) unclass(x)

#' @export
check_type.ts_self <- function(type, x) x

#' 'JavaScript' functions callable from R
#'
#' If `result` is `NULL`, it will be an oobSend (R process will continue);
#' otherwise the R process will wait for a response (oobMessage).
#'
#' @param ... arguments passed to the function
#' @param result the type of value returned from 'JavaScript' to R
#' @return A ts object that accepts 'JavaScript' functions as input.
#'   Using 'JavaScript' functions as output (R to 'JavaScript') is not supported yet.
#' @export
#' @examples
#' # Fire-and-forget callback from 'JavaScript' (oobSend)
#' cb <- js_function(ts_character(1))
#'
#' # Callback that returns a value to R (oobMessage)
#' ask <- js_function(ts_integer(1), result = ts_logical(1))
js_function <- function(..., result = NULL) {
    input <- list(...)
    types <- sapply(input, get_type, which = "input")

    ts_object(
        sprintf(
            "Robj.js_function([%s]%s)",
            paste(types, collapse = ", "),
            ifelse(is.null(result), "",
                paste(",", get_type(result, which = "input"))
            )
        ),
        NULL
    )
}
