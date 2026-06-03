h3 <- function(x) {
    cat("\n---", x, "\n")
}


format_js <- function(x) {
    x
}

#' Split env var into argv (whitespace-separated tokens; no embedded spaces in paths).
#' @noRd
parse_prettier_cmd_env <- function(x) {
    x <- trimws(x)
    if (!nzchar(x)) {
        return(character())
    }
    strsplit(x, "\\s+")[[1L]]
}

#' Resolve argv for Prettier (or compatible formatter): executable first, no file path.
#' The caller appends a temporary `.ts` path as the last argument.
#' @noRd
resolve_prettier_argv <- function(prettier_cmd) {
    if (length(prettier_cmd) >= 1L && nzchar(prettier_cmd[[1L]])) {
        return(as.character(prettier_cmd))
    }
    opt <- getOption("RserveTS.prettier_cmd")
    if (is.character(opt) && length(opt) >= 1L && nzchar(opt[[1L]])) {
        return(as.character(opt))
    }
    env <- Sys.getenv("RserveTS_PRETTIER_CMD", "")
    if (nzchar(env)) {
        return(parse_prettier_cmd_env(env))
    }
    parser_flags <- c("--parser", "typescript")
    p <- Sys.which("prettier")
    if (nzchar(p)) {
        return(c(p, parser_flags))
    }
    npx <- Sys.which("npx")
    if (nzchar(npx)) {
        return(c(npx, "--yes", "prettier", parser_flags))
    }
    stop(
        "Could not find Prettier or npx. Install Prettier with npm (see README) ",
        "or set option 'RserveTS.prettier_cmd' or env var RserveTS_PRETTIER_CMD ",
        "to a character vector / space-separated argv (executable first).",
        call. = FALSE
    )
}

#' Format generated TypeScript using an external CLI (writes a temp file, then runs
#' `executable ...flags path.ts` like `prettier --parser typescript path.ts`).
#'
#' @param text Character vector of source lines.
#' @param prettier_cmd Optional character vector argv (executable first); if
#'   `NULL`, uses option `RserveTS.prettier_cmd`, then env `RserveTS_PRETTIER_CMD`,
#'   then `Sys.which("prettier")`, then `npx prettier`.
#' @return Character vector of formatted lines.
#' @noRd
format_ts_source <- function(text, prettier_cmd = NULL) {
    if (length(text) == 0L) {
        return(character())
    }
    tf <- tempfile(fileext = ".ts")
    on.exit(unlink(tf), add = TRUE)
    writeLines(text, tf, useBytes = TRUE)

    argv <- resolve_prettier_argv(prettier_cmd)
    argv_tf <- c(argv, tf)
    cmd <- paste(vapply(argv_tf, shQuote, character(1)), collapse = " ")
    lines <- suppressWarnings(system(cmd, intern = TRUE))
    st <- attr(lines, "status", exact = TRUE)
    if (!is.null(st) && !identical(st, 0L)) {
        msg <- if (length(lines) && any(nzchar(lines))) {
            paste(lines, collapse = "\n")
        } else {
            ""
        }
        stop(
            "Prettier exited with status ", st,
            if (nzchar(msg)) paste0(":\n", msg) else "",
            call. = FALSE
        )
    }
    if (length(lines) == 1L && grepl("\n", lines[[1L]], fixed = TRUE)) {
        return(strsplit(lines[[1L]], "\n", fixed = TRUE)[[1L]])
    }
    lines
}
