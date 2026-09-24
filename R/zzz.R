# Suppress R CMD check warnings for variables used in closures
utils::globalVariables("widget")

.onLoad <- function(libname, pkgname) {
    op <- options()
    toset <- !(c(
        "RserveTS.format",
        "RserveTS.prettier_cmd",
        "RserveTS.compile_dir"
    ) %in% names(op))
    if (any(toset)) {
        options(list(
            RserveTS.format = FALSE,
            RserveTS.prettier_cmd = NULL,
            RserveTS.compile_dir = NULL
        )[toset])
    }
    invisible()
}
