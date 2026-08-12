# Suppress R CMD check warnings for variables used in closures
utils::globalVariables("widget")

.onLoad <- function(libname, pkgname) {
    op <- options()
    toset <- !(c("RserveTS.format", "RserveTS.prettier_cmd") %in% names(op))
    if (any(toset)) {
        options(list(
            RserveTS.format = FALSE,
            RserveTS.prettier_cmd = NULL
        )[toset])
    }
    invisible()
}
