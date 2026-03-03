#' Debug Logging for RserveTS
#'
#' Controlled via the `RSERVETS_DEBUG` environment variable.
#' Set to `*` for all tags, or a comma-separated list of tags:
#' `widget`, `ocap`, `init`, `state`, `child`.
#'
#' @examples
#' \dontrun{
#' # Enable all debug logging
#' Sys.setenv(RSERVETS_DEBUG = "*")
#'
#' # Enable specific tags
#' Sys.setenv(RSERVETS_DEBUG = "widget,child,init")
#'
#' # Disable
#' Sys.setenv(RSERVETS_DEBUG = "")
#' }
#'
#' @keywords internal
#' @name debug
NULL

#' Check if debug logging is enabled for a tag
#' @param tag Character tag to check
#' @return Logical
#' @keywords internal
#' @export
rts_debug_enabled <- function(tag = "general") {
    debug_val <- Sys.getenv("RSERVETS_DEBUG", "")
    if (debug_val == "") return(FALSE)
    if (debug_val == "*") return(TRUE)
    tag %in% strsplit(debug_val, ",", fixed = TRUE)[[1]]
}

#' Log a debug message
#' @param ... Message parts (passed to paste0)
#' @param tag Character tag for filtering
#' @keywords internal
#' @export
rts_log <- function(..., tag = "general") {
    if (!rts_debug_enabled(tag)) return(invisible())
    msg <- paste0("[RserveTS:", tag, "] ", paste(...))
    message(msg)
}
