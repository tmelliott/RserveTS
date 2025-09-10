#' Compile R functions
#'
#' Generates TypeScript schema for the given R function or file path. If a path, the R app is also generated.
#'
#' @param f A function or file path
#' @param name The name of the function
#' @param ... Additional arguments (passed to ts_deploy)
#' @param filename The base file path to write the TypeScript schema and R app to (optional, uses `[path of f].rserve` by default). `.R` and `.ts` file extensions are appended automatically. If `""`, the output is printed to the standard output console (see `cat`).
#' @return Character vector of TypeScript schema, or NULL if writing to file
#' @md
#' @export
ts_compile <- function(f, ..., name, filename) {
    o <- UseMethod("ts_compile")
}

compile_fn <- function(f) {
    inputs <- f$args
    result <- f$result

    inputs <- sapply(inputs, \(x) x$input_type)
    fn_args <- paste(paste(inputs), collapse = ", ")
    sprintf(
        "Robj.ocap([%s], %s)",
        fn_args,
        result$return_type
    )
}

#' @export
ts_compile.ts_function <- function(f, ..., name = deparse(substitute(f))) {
    ocap_str <- compile_fn(f)

    sprintf(
        "const %s = %s;", name, ocap_str
    )
}

#' @export
ts_compile.character <- function(
    f,
    ...,
    filename = sprintf("%s.rserve", tools::file_path_sans_ext(f))) {
    if (length(f) > 1) {
        return(sapply(f, ts_compile))
    }

    if (!file.exists(f)) {
        warning(sprintf("File not found: %s", f))
        return()
    }
    e <- new.env()
    source(f, local = e)

    fns <- ls(e)
    exports <- fns[sapply(
        fns,
        \(z) inherits(e[[z]], "ts_function") && isTRUE(e[[z]]$export)
    )]

    exportFns <- sapply(
        exports,
        \(z) ts_compile(e[[z]], filename = "", name = z)
    )

    src <- c(
        "import { Robj } from 'rserve-ts';",
        "import { z } from 'zod';",
        "\n",
        exportFns,
        "\n",
        sprintf(
            "export default {\n  %s\n};",
            paste(exports, collapse = ",\n  ")
        )
    )

    cat(src, file = sprintf("%s.ts", filename), sep = "\n")

    # R file
    ts_deploy(f, file = sprintf("%s.R", filename), silent = TRUE, ...)

    invisible()
}

#' @export
ts_compile.default <- function(f, ...) {
    warning("Not supported")
}
