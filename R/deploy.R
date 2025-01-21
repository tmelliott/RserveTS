#' Deploy a ts Rserve app
#'
#' @param f The path to the application files
#' @param file The file to write the deployment script to
#' @param init Names of objects (ts_functions) to make available to
#'             the initialisation function
#' @param port The port to deploy the app on
#' @param run Whether to run the deployment script,
#'            takes values "no", "here", "background"
#' @return NULL, called to open an Rserve instance
#' @export
#' @md
ts_deploy <- function(f,
                      file = sprintf("%s.rserve.R", tools::file_path_sans_ext(f)),
                      init = NULL,
                      port = 6311,
                      run = c("no", "here", "background")) {
    if (length(f) != 1) stop("Expected a single path")
    if (!file.exists(f)) stop("File not found")

    x <- readLines(f)

    if (is.null(init)) init <- ls_ocaps(f)
    init <- sprintf(
        "list(\n   %s\n)",
        paste(sapply(init, \(z) sprintf("%s = %s", z, z)), collapse = ",\n   ")
    )

    src <- c(
        "library(Rserve)",
        "library(ts)",
        "",
        x,
        sprintf("first.fns <- function() ts_app(%s)", init),
        "",
        sprintf("oc.init <- function() Rserve:::ocap(first.fns)"),
        "",
        sprintf(
            paste(
                "Rserve::run.Rserve(",
                "  websockets.port = %s,",
                "  websockets = TRUE,",
                "  oob = TRUE,",
                "  qap = FALSE,",
                "  websockets.qap.oc = TRUE",
                ")",
                sep = "\n"
            ),
            port
        )
    )

    writeLines(src, file)

    run <- match.arg(run)
    switch(run,
        "no" = {
            cat("Run the following command to deploy the app:\n")
            cat(sprintf("Rscript %s", file), "\n")
        },
        "here" = source(file),
        "background" = system(sprintf("Rscript %s", file))
    )
}

ls_ocaps <- function(f) {
    e <- new.env()
    source(f, local = e)
    x <- ls(e)
    x[sapply(x, \(z) class(e[[z]]) == "ts_function")]
}
