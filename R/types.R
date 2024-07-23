object <- function(type = "any",
                   type_fn = "any",
                   default = NULL,
                   check = function() stop("Not implemented")) {
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
        n_type_fun(n, "Logical"),
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
        n_type_fun(n, "Integer"),
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
        n_type_fun(n, "Numeric"),
        check = function(x) {
            if (!is.numeric(x)) stop("Expected a number")
            if (n > 0 && length(x) != n) stop("Expected a number of length ", n)
            x
        }
    )
}

#' @export
ts_character <- function(n = -1L) {
    object(
        n_type(n, "string"),
        n_type_fun(n, "Character"),
        check = function(x) {
            if (!is.character(x)) stop("Expected a string")
            if (n > 0 && length(x) != n) stop("Expected a string of length ", n)
            x
        }
    )
}
