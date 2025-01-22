h3 <- function(x) {
    cat("\n---", x, "\n")
}



format_js <- function(x) {
    if (!requireNamespace("js", quietly = TRUE)) {
        return(x)
    }
    js::uglify_reformat(x, beautify = TRUE)
}
