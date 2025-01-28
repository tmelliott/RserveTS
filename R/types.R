#' Typed object
#'
#' This is the base type for all typed objects. It is not meant to be used directly.
#'
#' @param input_type The type of the object that Typescript expect to send to R.
#' @param return_type The type of the object that Typescript expects to recieve from R.
#' @param default The default value of the object.
#' @param check A function that checks the object and returns it if it is valid. This operates on the R side and is mostly for development and debugging purposes. It is up to the developer to ensure that all functions return the correct type of object always.
#' @param generic logical, if `TRUE` then the object is a generic type.
#'
#' @md
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
        print(x$return_type)
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
get_type.ts_function <- function(x, which = c("input", "return")) {
    which <- match.arg(which)
    if (which == "input") {
        return("z.function()")
    }
    "Robj.ocap()"
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

ts_union <- function(...) sprintf("z.union([%s])", paste(..., sep = ", "))
ts_array <- function(type = c("z.number()", "z.boolean()", "z.string()")) {
    if (type == "z.number()") {
        return("z.instanceof(Float64Array)")
    }
    if (type == "z.boolean()") {
        return("z.instanceof(Uint8Array)")
    }
    return("z.array(z.string())")
}

n_type <- function(n, type, pl = ts_array(type)) {
    if (n == 1) {
        return(type)
    }
    if (n == -1) {
        return(ts_union(type, pl))
    }
    pl
}
n_type_fun <- function(n, type) {
    sprintf("%s(%s)", type, ifelse(n < 0, "", n))
}

#' Logical or boolean type
#'
#' Booleans are represented in Zod schema as either a boolean (`z.boolean()`),
#' or a typed Uint8Array (`z.instanceof(Uint8Array)`).
#'
#' @param n The length of the boolean vector. If `n = 1` then a single boolean is expected. If `n = 0` then any length is expected. If `n > 1` then a boolean vector of length `n` is expected.
#' @return A ts object that accepts logical scalars or vectors of length `n`.
#' @export
#' @md
ts_logical <- function(n = -1L) {
    ts_object(
        n_type(n, "z.boolean()"),
        n_type_fun(n, "Robj.logical"),
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
#' Integers are represented in Zod schema as either a number (`z.number()`),
#' or a Int32Array (`z.instanceof(Int32Array)`).
#'
#' @param n The length of the integer vector. If `n = 1` then a single integer is expected. If `n = 0` then any length is expected. If `n > 1` then an integer vector of length `n` is expected.
#' @return A ts object that accepts integer scalars or vectors of length `n`.
#' @export
#' @md
ts_integer <- function(n = -1L) {
    ts_object(
        n_type(n, "z.number()", "z.instanceof(Int32Array)"),
        n_type_fun(n, "Robj.integer"),
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
#' Numbers are represented in Zod schema as either a number (`z.number()`),
#' or a Float64Array (`z.instanceof(Float64Array)`).
#'
#' @param n The length of the numeric vector. If `n = 1` then a single number is expected. If `n = 0` then any length is expected. If `n > 1` then a numeric vector of length `n` is expected.
#' @return A ts object that accepts numeric scalars or vectors of length `n`.
#' @export
#' @md
ts_numeric <- function(n = -1L) {
    ts_object(
        n_type(n, "z.number()"),
        n_type_fun(n, "Robj.numeric"),
        check = function(x) {
            if (!is.numeric(x)) stop("Expected a number", call. = FALSE)
            if (n > 0 && length(x) != n) {
                stop("Expected a number of length ", n, , call. = FALSE)
            }
            x
        }
    )
}

#' Character or string type
#'
#' Strings are represented in Zod schema as either a string (`z.string()`),
#' or a string array (`z.array(z.string())`).
#' @param n The length of the string vector. If `n = 1` then a single string is expected. If `n = 0` then any length is expected. If `n > 1` then a string vector of length `n` is expected.
#' @return A ts object that accepts strings or string vectors of length `n`.
#' @export
#' @md
ts_character <- function(n = -1L) {
    ts_object(
        n_type(n, "z.string()"),
        n_type_fun(n, "Robj.character"),
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
#' @return A ts object that accepts factors with the specified levels.
#'
#' @export
#' @md
ts_factor <- function(levels = NULL) {
    ts_object(
        ifelse(is.null(levels),
            ts_array("z.string()"),
            sprintf("(%s)[]", paste(levels, collapse = " | "))
        ),
        if (is.null(levels)) {
            "Robj.factor()"
        } else {
            sprintf("Robj.factor(%s)", vector_as_ts_array(levels))
        },
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
#' @param ... A list of types, named or unnamed.
#' @return A ts object that accepts lists with the specified types.
#'
#' @export
#' @md
ts_list <- function(...) {
    values <- list(...)

    type <- "z.union([z.object({}), z.array(z.any())])"
    type_fn <- ""
    if (length(values)) {
        types <- sapply(values, get_type, which = "input")
        type_funs <- sapply(values, get_type, which = "return")
        if (!is.null(names(values))) {
            type <- sprintf(
                "{ %s }",
                paste(names(values), types, sep = ": ", collapse = ", ")
            )
            type_fn <- sprintf(
                "{ %s }",
                paste(names(values), type_funs, sep = ": ", collapse = ", ")
            )
        } else {
            type <- sprintf("[%s]", paste(types, collapse = ", "))
            type_fn <- sprintf("[%s]", paste(type_funs, collapse = ", "))
        }
    }

    ts_object(
        type,
        ifelse(type_fn == "", "Robj.list()",
            sprintf("Robj.list(%s)", type_fn)
        ),
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
}


#' Typed dataframe
#'
#' This is essentially a list, but the elements must have names and are all the same length.
#'
#' @param ... Named types.
#' @return A ts object that accepts data frames with the specified types.
#'
#' @export
#' @md
ts_dataframe <- function(...) {
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
        check = function(x) {
            if (!is.data.frame(x)) stop("Expected a data frame")
            x
        }
    )
}

#' Null type
#'
#' This is a type that only accepts `NULL`.
#'
#' @return A ts object that only accepts `NULL`.
#' @export
#'
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
ts_void <- function() {
    ts_object(
        "z.void()",
        "null",
        check = function(x) {
            return(NULL)
        }
    )
}
