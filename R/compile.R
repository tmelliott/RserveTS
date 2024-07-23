#' @export
ts_compile <- function(f) {
    name <- deparse(substitute(f))
    inputs <- attr(f, "args")
    result <- attr(f, "result")

    inputs <- sapply(inputs, \(x) x$type)
    fn_args <- paste(names(inputs), inputs, sep = ": ") |>
        paste(collapse = ", ")
    cat(sprintf("const %s = (%s) => Promise<%s>;", name, fn_args, result$type_fn), "\n")
}
