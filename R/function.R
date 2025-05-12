parse_args <- function(x, mc) {
    fmls <- lapply(x, eval)
    if (!all(names(mc) %in% names(fmls))) {
        stop(
            "Invalid argument(s): ",
            paste(setdiff(names(mc), names(fmls)), collapse = ", ")
        )
    }
    args <- lapply(names(fmls), function(n) {
        cat("\n---- Checking args ...\n")
        print(n)
        print(mc[[n]])
        tryCatch(
            {
                fmls[[n]]$check(eval(mc[[n]]))
            },
            error = function(e) {
                stop("Invalid argument '", n, "': ", e$message, call. = FALSE)
            }
        )
    })
    names(args) <- names(fmls)
    args
}

check_args <- function(args, fmls) {
    args <- as.list(args)[-1]
    fmls <- lapply(fmls, eval)
    lapply(names(fmls), function(n) {
        tryCatch(
            {
                fmls[[n]]$check(args[[n]])
            },
            error = function(e) {
                stop("Invalid argument '", n, "': ", e$message, call. = FALSE)
            }
        )
    })
}

ts_result <- function(type, value) {
    if (type$check(value)) {
        return(value)
    }
    stop("Expected a value of type ", type$type)
}

#' TS function definition
#'
#' @details
#' Defining functions is the core of writing Rserve apps.
#' Functions are referred to as *object capabilities* (ocaps),
#' as they are 'objects' that allow Javascript to access capabilities
#' of R with a restricted interface. Only arguments can be adjusted.
#'
#' TS functions can be defined using existing (named) or anonymous functions.
#' Anonymous functions are useful in that the arguments to the functions
#' can explicitly be defined with their types as formal arguments:
#'
#' ```
#' ts_function(function(x = ts_integer(), y = ts_string()) { ... })
#' ```
#'
#' @param f an R function
#' @param ... argument definitions (only required if f does not specify these in its formals)
#' @param result return type (ignored if overloads are provided)
#' @export
#' @md
#'
#' @return a ts function object which has a `call` method that will call the function with the given arguments, which will be checked for type correctness.
#'
#' @examples
#' f <- ts_function(function(x = ts_integer(1), y = ts_character(1)) {
#'     x + nchar(y)
#' }, result = ts_integer(1))
#' f$call(1, "hello")
ts_function <- function(f, ..., result = ts_void()) {
    args <- list(...)
    if (!is.null(result) && !is_ts_object(result)) {
        stop("Invalid return type")
    }

    if (length(args) == 0) {
        args <- lapply(formals(f), eval)
    }

    e <- new.env()
    e$f <- f
    # e$env <- env
    e$args <- args
    e$result <- result

    e$call <- function(...) {
        # mc <- match.call(e$f)
        # .args <- parse_args(args, mc[-1])
        # .res <- do.call(e$f, .args)

        # We don't need to do match.call or anything,
        # because Rserve can only pass args as an unnamed array,
        # anyway ... and we don't need to check input types
        # as they will be valid on the TypeScript side ...
        .res <- e$f(...)

        # so, we only need to check the result before sending it to
        # TypeScript (and even that is probably overkill because
        # Zod will check the types for us, anyway ...)
        check_type(result, .res)
        .res
    }

    e$copy <- function(env = parent.frame()) {
        e2 <- e
        environment(e2$f) <- rlang::env_clone(environment(e$f), env)
        e2
    }

    class(e) <- "ts_function"
    e
}


#' @export
print.ts_function <- function(x, ...) {
    h3("Ocap function")

    cat("Arguments:\n")
    args <- lapply(x$args, \(z) z$input_type)
    lapply(names(args), \(n) {
        cat("- ", n, ": ", args[[n]], "\n", sep = "")
    })
    cat("\n\n")

    cat("Return type:\n")
    cat(x$result$return_type)
}

#' Generate an Rserve app from a ts function
#'
#' Anything that is not a function simply returns itself.
#' However, functions are wrapped with `Rserve::ocap()`,
#' and the result is subsequently wrapped with `ts_app()`.
#'
#' @param x A ts function object (`ts_function()`)
#' @export
#' @md
#'
#' @returns An object of class 'OCref', see [Rserve::ocap()]
#'
#' @examples
#' f <- ts_function(function(x = ts_integer(1), y = ts_character(1)) {
#'     x + nchar(y)
#' }, result = ts_integer(1))
#' app <- ts_app(f) # class of 'OCref'
#' # this can now be used in an Rserve application, for example
ts_app <- function(x) UseMethod("ts_app")

#' @export
ts_app.default <- function(x) {
    x
}

#' @export
ts_app.list <- function(x) {
    lapply(x, ts_app)
}

#' @export
ts_app.ts_function <- function(x) {
    Rserve::ocap(function(...) ts_app(x$call(...)))
}
