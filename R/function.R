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
        fmls[[n]]$check(eval(mc[[n]]))
    })
    names(args) <- names(fmls)
    args
}

#' TS function definition
#'
#' @param f an R function
#' @param ... argument definitions, OR function overloads
#' @param result return type (ignored if overloads are provided)
#' @export
ts_function <- function(f, ..., result = NULL) {
    args <- list(...)
    if (!is.null(result) && !is_object(result)) {
        stop("Invalid return type")
    }
    if (any(is_overload(args))) {
        if (!all(is_overload(args))) {
            stop("Cannot mix overloads with standard arguments")
        }
        z <- lapply(args, function(x) {
            do.call(ts_function, c(list(f), x$args, list(result = x$result)))
        })
        class(z) <- "ts_overload"
        return(z)
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

#' @export
is_overload <- function(x) {
    sapply(x, inherits, what = "ts_overload")
}

#' @export
ts_overload <- function(..., result = NULL) {
    structure(list(args = list(...), result = result),
        class = "ts_overload"
    )
}
