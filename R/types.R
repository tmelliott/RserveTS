#' Typed object
#'
#' This is the base type for all typed objects. It is not meant to be used directly.
#'
#' @param type The type of the object that Typescript expect to send to R.
#' @param type_fn The type of the object that Typescript expects to recieve from R.
#' @param default The default value of the object.
#' @param check A function that checks the object and returns it if it is valid. This operates on the R side and is mostly for development and debugging purposes. It is up to the developer to ensure that all functions return the correct type of object always.
#' @param generic logical, if `TRUE` then the object is a generic type.
#'
#' @md
object <- function(type = "any",
                   type_fn = "any",
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
    cat(sprintf("Object: %s\n", x$type))
    cat(sprintf("rserve-ts type: %s\n", x$type_fn))
}

is_object <- function(x) {
    inherits(x, "ts_object")
}

ts_union <- function(...) paste(..., sep = " | ")
ts_array <- function(type) paste(type, "[]", sep = "")

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
    if (n < 0) {
        return(type)
    }
    sprintf("%s<%s>)", type, n)
}

#' @export
ts_logical <- function(n = -1L) {
    object(
        n_type(n, "boolean"),
        n_type_fun(n, "RTYPE.Logical"),
        check = function(x) {
            if (!is.logical(x)) stop("Expected a boolean")
            if (n > 0 && length(x) != n) stop("Expected a boolean of length ", n)
            x
        }
    )
}

#' @export
ts_integer <- function(n = -1L) {
    object(
        n_type(n, "number"),
        n_type_fun(n, "RTYPE.Integer"),
        check = function(x) {
            if (!is.integer(x)) stop("Expected an integer")
            if (n > 0 && length(x) != n) stop("Expected an integer of length ", n)
            x
        }
    )
}

#' @export
ts_numeric <- function(n = -1L) {
    object(
        n_type(n, "number"),
        n_type_fun(n, "RTYPE.Numeric"),
        check = function(x) {
            if (!is.numeric(x)) stop("Expected a number", call. = FALSE)
            if (n > 0 && length(x) != n) {
                stop("Expected a number of length ", n, , call. = FALSE)
            }
            x
        }
    )
}

#' @export
ts_character <- function(n = -1L) {
    object(
        n_type(n, "string"),
        n_type_fun(n, "RTYPE.Character"),
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
#'
#' @export
#' @md
ts_factor <- function(levels = NULL) {
    object(
        sprintf("(%s)[]", paste(levels, collapse = " | ")),
        if (is.null(levels)) {
            "Factor"
        } else {
            sprintf("Factor<%s>", vector_as_ts_array(levels))
        },
        check = function(x) {
            if (!is.factor(x)) stop("Expected a factor")
            if (!is.null(levels) && !identical(levels, levels(x))) {
                stop("Expected a factor with levels ", levels)
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
#' @export
#' @md
ts_list <- function(values = NULL) {
    type <- "[]"
    type_fn <- ""
    if (!is.null(values)) {
        types <- sapply(values, function(x) x$type)
        type_funs <- sapply(values, function(x) x$type_fn)
        if (!is.null(names(values))) {
            type <- sprintf(
                "{%s}",
                paste(names(values), types, sep = ": ", collapse = ", ")
            )
            type_fn <- sprintf(
                "{%s}",
                paste(names(values), type_funs, sep = ": ", collapse = ", ")
            )
        } else {
            type <- sprintf("[%s]", paste(values, collapse = ", "))
            type_fn <- sprintf("[%s]", paste(type_funs, collapse = ", "))
        }
    }

    object(
        type,
        sprintf("List<%s>", type_fn),
        check = function(x) {
            if (!is.list(x)) stop("Expected a list")
            x
        }
    )
}


#' Typed dataframe
#'
#' This is essentially a list, but the elements must have names and are all the same length.
#'
#' @export
#' @md
ts_dataframe <- function(...) {
    values <- list(...)
    type <- "{}"
    type_fn <- ""
    if (length(values)) {
        types <- sapply(values, function(x) x$type)
        type_funs <- sapply(values, function(x) x$type_fn)
        if (is.null(names(values))) stop("Expected named elements")

        type <- sprintf(
            "{\n  %s\n}",
            paste(names(values), types, sep = ": ", collapse = ",\n  ")
        )
        type_fn <- sprintf(
            "{\n  %s\n}",
            paste(names(values), type_funs, sep = ": ", collapse = ",\n  ")
        )
    }

    object(
        type,
        sprintf("List<%s>", type),
        check = function(x) {
            if (!is.data.frame(x)) stop("Expected a data frame")
            x
        }
    )
}
