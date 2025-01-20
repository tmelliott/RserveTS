#' Compile R functions to TypeScript schemas
#' @param f A function or file path
#' @param name The name of the function
#' @param ... Additional arguments
#' @param file The file path to write the TypeScript schema (optional). If `""`, the output is printed to the standard output console (see `cat`).
#' @return Character vector of TypeScript schema, or NULL if writing to file
#' @md
#' @export
ts_compile <- function(f, ..., name, file) {
    o <- UseMethod("ts_compile")
}

#' @export
ts_compile.ts_function <- function(f, ..., name = deparse(substitute(f))) {
    inputs <- f$args
    result <- f$result

    inputs <- sapply(inputs, \(x) x$input_type)
    fn_args <- paste(paste(inputs), collapse = ", ")

    sprintf(
        "const %s = Robj.ocap([%s], %s);", name, fn_args,
        result$return_type
    )
}

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
    # types <- unique(
    #     gsub(
    #         "RTYPE\\.(\\w+)", "\\1",
    #         unlist(regmatches(x, gregexpr("RTYPE\\.\\w+", x)))
    #     )
    # )
    # x <- gsub("RTYPE\\.", "", x)

    src <- c(
        "import { Robj } from 'rserve-ts';",
        "import { z } from 'zod';",
        "\n",
        x
    )

    writeLines(src, file)

    invisible()
}

#' @export
ts_compile.default <- function(f, ...) {
    warning("Not supported")
}
