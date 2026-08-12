#' Debug logging for RserveTS
#'
#' Controlled via the `RSERVETS_DEBUG` environment variable.
#' Set to `*` for all tags, or a comma-separated list of tags:
#' `widget`, `ocap`, `init`, `state`, `child`.
#'
#' @name rts_debug
#' @keywords internal
#' @seealso [rts_debug_enabled()], [rts_log()]
#' @examples
#' withr::with_envvar(
#'     c(RSERVETS_DEBUG = "*"),
#'     {
#'         rts_debug_enabled("widget")
#'     }
#' )
#' withr::with_envvar(
#'     c(RSERVETS_DEBUG = "widget,child,init"),
#'     {
#'         rts_debug_enabled("child")
#'     }
#' )
#' withr::with_envvar(
#'     c(RSERVETS_DEBUG = ""),
#'     {
#'         rts_debug_enabled()
#'     }
#' )
NULL

#' Check if debug logging is enabled for a tag
#' @param tag Character tag to check
#' @return Logical scalar; `TRUE` when `RSERVETS_DEBUG` enables `tag`.
#' @keywords internal
#' @export
rts_debug_enabled <- function(tag = "general") {
    debug_val <- Sys.getenv("RSERVETS_DEBUG", "")
    if (debug_val == "") {
        return(FALSE)
    }
    if (debug_val == "*") {
        return(TRUE)
    }
    tag %in% strsplit(debug_val, ",", fixed = TRUE)[[1]]
}

#' Log a debug message
#' @param ... Message parts (passed to paste0)
#' @param tag Character tag for filtering
#' @return `invisible(NULL)`. Emits a [message()] when the tag is enabled.
#' @keywords internal
#' @export
rts_log <- function(..., tag = "general") {
    if (!rts_debug_enabled(tag)) {
        return(invisible())
    }
    msg <- paste0("[RserveTS:", tag, "] ", paste(...))
    message(msg)
}
