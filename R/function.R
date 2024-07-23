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

# TS function
#' @export
ts_function <- function(f, ..., result = NULL) {
    args <- list(...)
    if (!is_object(result)) {
        stop("Invalid return type")
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
