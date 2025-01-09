parse_args <- function(x, mc) {
    fmls <- lapply(x, eval)
    mc <- mc[-1]
    if (!all(names(mc) %in% names(fmls))) {
        stop(
            "Invalid argument(s): ",
            paste(setdiff(names(mc), names(fmls)), collapse = ", ")
        )
    }
    args <- lapply(names(fmls), function(n) {
        tryCatch(
            {
                fmls[[n]]$check(eval(mc[[n]]))
            },
            error = function(e) {
                stop("Invalid argument '", n, "': ", e$message, call. = FALSE)
            }
        )
    })
    names(args) <- names(fmls)
    args
}

check_args <- function(args, fmls) {
    args <- as.list(args)[-1]
    fmls <- lapply(fmls, eval)
    lapply(names(fmls), function(n) {
        tryCatch(
            {
                fmls[[n]]$check(args[[n]])
            },
            error = function(e) {
                stop("Invalid argument '", n, "': ", e$message, call. = FALSE)
            }
        )
    })
}

ts_result <- function(type, value) {
    if (type$check(value)) {
        return(value)
    }
    stop("Expected a value of type ", type$type)
}

#' TS function definition
#'
#' @param f an R function
#' @param ... argument definitions (only required if f does not specify these in its formals)
#' @param result return type (ignored if overloads are provided)
#' @export
ts_function <- function(f, ..., result = NULL) {
    args <- list(...)
    if (!is.null(result) && !is_ts_object(result)) {
        stop("Invalid return type")
    }

    if (length(args) == 0) {
        args <- lapply(formals(f), eval)
    }

    fn <- function(...) {
        mc <- match.call(f)
        x <- parse_args(args, mc)
        result$check(do.call(f, x))
    }
    attr(fn, "args") <- args
    attr(fn, "result") <- result
    class(fn) <- c("ts_function", class(f))
    fn
}
