#' Create a TypeScript-Compatible Widget
#'
#' Creates a reference class-based widget that can interact with TypeScript code.
#' The widget supports reactive properties that can be observed from both R and
#' TypeScript, with automatic state synchronization.
#'
#' Note that the object constructed takes a Javascript setter function as argument, so calling `obj$call()` will fail.
#'
#' @param name Character string specifying the name of the widget class
#' @param properties Named list of typed properties for the widget. Each property
#'   should be a TypeScript type object that defines the property's type
#' @param initialize Optional initialization function that receives the widget
#'   instance and sets up initial state
#' @param methods Named list of methods to add to the widget class. Each method
#'   should be a `ts_function` object
#' @param ... Additional arguments passed to the TypeScript function constructor
#'
#' @return A TypeScript function constructor that creates widget instances with
#'   reactive properties and methods for TypeScript interoperability
#'
#' @details
#' The created widget includes built-in methods:
#' \itemize{
#'   \item \code{set(prop, value)}: Set a property value and mark it as changed
#'   \item \code{get(prop)}: Get a property value
#'   \item \code{addPropHandler(prop, fn)}: Register a handler for property changes
#'   \item \code{updateState(all = FALSE)}: Synchronize changed properties to TypeScript
#' }
#'
#' Each property automatically gets TypeScript-accessible methods:
#' \itemize{
#'   \item \code{register(fn)}: Register a callback for property changes
#'   \item \code{get()}: Get the current property value
#'   \item \code{set(x)}: Set the property value
#' }
#'
#' @export
#'
#' @examples
#' # Create a simple counter widget
#'
#' createWidget(
#'     name = "Counter",
#'     properties = list(count = ts_integer(1)),
#'     initialize = function(widget) {
#'         widget$set("count", 0)
#'     }
#' )
createWidget <- function(
    name,
    properties = list(),
    initialize = NULL,
    methods = list(),
    ...) {
    rc <- setRefClass(name,
        properties(
            fields = c(
                widgetProperties(properties),
                list(
                    changed = "character",
                    setState = "function"
                )
            )
        ),
        methods = c(
            widgetMethods(substitute(methods)),
            list(
                initialize = function(setState) {
                    .self$setState <- setState
                },
                set = function(prop, value) {
                    if (!any(.self$changed == prop)) {
                        .self$changed <- c(.self$changed, prop)
                    }
                    .self[[prop]] <- value
                },
                get = function(prop) .self[[prop]],
                addPropHandler = function(prop, fn) {
                    .self[[sprintf("%sChanged", prop)]]$connect(fn)
                },
                updateState = function(all = FALSE) {
                    if (all) chg <- names(properties) else chg <- .self$changed
                    if (length(chg) == 0) {
                        return()
                    }

                    x <- lapply(chg, \(p) .self$get(p))
                    names(x) <- chg

                    .self$setState(x)
                    .self$changed <- character()
                }
            )
        ),
        where = parent.frame()
    )

    props <- lapply(names(properties), \(name) {
        prop <- properties[[name]]
        list(
            register = ts_function(
                function(fn, id) {
                    id <- widget$addPropHandler(
                        name, function() {
                            # cat("-- calling JS fun\n")
                            jsfun(fn)(widget$get(name))
                            # cat("-- jsfun complete ...\n")
                            invisible(NULL)
                        }
                    )
                    cat("- REGISTER", name, "| id =", id, "\n")
                    id
                },
                fn = js_function(
                    x = prop,
                    result = ts_null()
                ),
                id = ts_character(1),
                result = ts_character(1L)
            ),
            get = ts_function(
                function() {
                    # cat(sprintf("- get {%s}\n", name))
                    widget$get(name)
                },
                result = prop
            ),
            set = ts_function(
                function(x) {
                    cat(sprintf(
                        "- setting {%s} to %s\n",
                        name, x
                    ))
                    widget$set(name, x)
                    invisible()
                },
                x = prop
            )
        )
    })
    names(props) <- names(properties)

    w_type <- ts_list(
        properties = do.call(
            ts_list,
            lapply(props, \(prop) do.call(ts_list, prop))
        )
    )

    setStateType <- do.call(
        ts_list,
        lapply(properties, \(prop) ts_optional(prop))
    )

    w_ctor <- ts_function(
        # pass in 'init' args here ...
        function(fn) {
            widget <- rc$new(
                setState = if (is.null(fn)) function() {} else jsfun(fn)
            )

            if (!is.null(initialize)) {
                initialize(widget)
            }
            widget$updateState()

            list(
                properties = lapply(props, \(prop) lapply(prop, \(method) method$copy()))
            )
        },
        fn = ts_optional(js_function(state = setStateType, result = ts_null())),
        result = w_type,
        ...
    )

    w_ctor
}

# convert typed properties to ref class fields
widgetProperties <- function(properties) {
    fields <- lapply(properties, \(x) {
        if (!inherits(x, "ts_object")) stop("Invalid property")
        t <- gsub("\\(.+", "", gsub("Robj.", "", x$return_type, fixed = TRUE))
        if (t == "dataframe") t <- "data.frame"
        t
    })
    fields
}

# create list of methods
widgetMethods <- function(methods) {
    methods <- as.list(methods)[-1]


    lapply(methods, \(x) {
        # TODO: gotta make sure these *are* ts_function
        # print(x)
        eval(x[[2]])
    })
}
