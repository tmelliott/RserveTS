#' @export
ts_compile <- function(f, ..., file = NULL) {
    o <- UseMethod("ts_compile")
}

#' @export
ts_compile.ts_function <- function(f, name = deparse(substitute(f)), ...) {
    inputs <- attr(f, "args")
    result <- attr(f, "result")

    inputs <- sapply(inputs, \(x) x$zod_type)
    fn_args <- paste(inputs) |>
        paste(collapse = ", ")
    sprintf("const %s = R.ocap([%s], %s]);", name, fn_args, result$r_type)
}

# #' @export
# ts_compile.ts_overload <- function(f, file = NULL, name = deparse(substitute(f))) {
#     cmt <- sprintf("\n// %s overloads", name)
#     oloads <- sapply(f, ts_compile, name = name)
#     paste(cmt, paste(oloads, collapse = "\n"), sep = "\n")
# }

#' @export
ts_compile.character <- function(
    f,
    file = sprintf("%s.d.ts", tools::file_path_sans_ext(f))) {
    if (length(f) > 1) {
        return(sapply(f, ts_compile))
    }

    if (!file.exists(f)) {
        warning(sprintf("File not found: %s", f))
        return()
    }
    e <- new.env()
    source(f, local = e)

    x <- sapply(ls(e), \(x) ts_compile(e[[x]], file = file, name = x))

    # find any RTYPE.[type] and grab types
    types <- unique(
        gsub(
            "RTYPE\\.(\\w+)", "\\1",
            unlist(regmatches(x, gregexpr("RTYPE\\.\\w+", x)))
        )
    )
    x <- gsub("RTYPE\\.", "", x)

    cat(
        sprintf(
            "import { %s } from 'rserve-ts';\n\n",
            paste(types, collapse = ", ")
        ),
        file = file
    )
    cat(x, sep = "\n", file = file, append = TRUE)

    invisible()
}

#' @export
ts_compile.default <- function(f, ...) {
    warning("Not supported")
}
