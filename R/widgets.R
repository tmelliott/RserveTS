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
#' @param .env Environment where the ref class should be created. Defaults to
#'   `parent.frame()` which is the caller's environment (typically unlocked).
#'   Can be overridden (e.g., to `.GlobalEnv`) if needed.
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
#' @importFrom methods setRefClass new
#' @importFrom objectProperties properties
#' @export
#'
#' @examples
#' # Create a simple counter widget
#' \dontrun{
#' createWidget(
#'     name = "Counter",
#'     properties = list(count = ts_integer(1)),
#'     initialize = function(widget) {
#'         widget$set("count", 0)
#'     }
#' )
#' }
createWidget <- function(
    name,
    properties = list(),
    initialize = NULL,
    methods = list(),
    auto_flush = TRUE,
    .env = parent.frame(),
    ...) {
    # setRefClass with contains tries to register class metadata in the base class's
    # namespace. To work around locked namespace issues, we need to ensure the
    # class definition can be registered. The .env parameter allows specifying
    # where the new class should be created, but inheritance metadata may still
    # need to be registered in the package namespace.
    method_info <- widgetMethods(substitute(methods), auto_flush = auto_flush)

    rc <- setRefClass(name,
        properties(
            fields = widgetProperties(properties)
        ),
        contains = "tsWidget",
        methods = c(
            method_info$methods,
            list(
                initialize = function(setState) {
                    .self$setState <- setState
                    .self$.child_connectors <- NULL
                    .self$.childrenVersion <- 0L
                    .self$.call_depth <- 0L
                }
            )
        ),
        where = .env
    )

    ts_props <- tsProps(properties)
    widget_props <- widgetProps(properties)

    props <- lapply(names(ts_props), \(name) {
        prop <- ts_props[[name]]
        if (!inherits(prop, "ts_object")) {
            return(NULL)
        }
        list(
            register = ts_function(
                function(fn, id) {
                    id <- widget$addPropHandler(
                        name, function() {
                            jsfun(fn)(widget$get(name))
                            invisible(NULL)
                        }
                    )
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
                    widget$get(name)
                },
                result = prop
            ),
            set = ts_function(
                function(x) {
                    widget$set(name, x)
                    invisible()
                },
                x = prop
            )
        )
    })
    names(props) <- names(ts_props)

    method_defs <- method_info$exported_defs

    w_type <- ts_list(
        properties = do.call(
            ts_list,
            lapply(props, \(prop) do.call(ts_list, prop))
        ),
        children = do.call(ts_list, widget_props),
        methods = if (length(method_defs) > 0)
            do.call(ts_list, method_defs)
        else
            ts_list()
    )

    setStateType <- do.call(
        ts_list,
        lapply(ts_props, \(prop) ts_optional(prop))
    )

    w_ctor <- ts_function(
        # pass in 'init' args here ...
        function(fn) {
            widget <- rc$new(
                setState = if (is.null(fn)) NULL else jsfun(fn)
            )
            # Store ts property names so updateState(all=TRUE) knows what to send
            widget$.property_names <- names(ts_props)
            # Wire change-tracking: direct assignment auto-adds to changed list
            for (prop_name in names(ts_props)) {
                local({
                    pn <- prop_name
                    widget[[paste0(pn, "Changed")]]$connect(function() {
                        if (!any(widget$changed == pn)) {
                            widget$changed <- c(widget$changed, pn)
                        }
                    })
                })
            }
            # Apply property defaults before initialize
            for (prop_name in names(ts_props)) {
                default_val <- ts_props[[prop_name]]$default
                if (!is.null(default_val)) {
                    widget$set(prop_name, default_val)
                }
            }
            # Wire observer methods to property signals
            for (nm in names(method_info$observers)) {
                local({
                    # Build function() widget$<method>() using substitute
                    # so $ dispatches to the ref class method (not field via [[)
                    handler_fn <- eval(substitute(
                        function() widget$METHOD(),
                        list(METHOD = as.name(nm))
                    ))
                    for (prop in method_info$observers[[nm]]) {
                        widget$addPropHandler(prop, handler_fn,
                            auto_flush = FALSE)
                    }
                })
            }
            if (!is.null(initialize)) {
                initialize(widget)
            }
            widget$updateState(all = TRUE)

            lapply(
                names(widget_props),
                \(prop) widget$add_child(prop, widget_props[[prop]])
            )

            list(
                properties =
                    lapply(props, \(prop) lapply(prop, \(method) method$copy())),
                children = widget$.child_connectors,
                methods = build_method_ocaps(widget, method_defs)
            )
        },
        fn = ts_optional(js_function(state = setStateType, result = ts_null())),
        result = w_type,
        ...
    )

    class(w_ctor) <- c("ts_widget", class(w_ctor))
    attr(w_ctor, ".__refclass") <- rc
    attr(w_ctor, ".__props") <- list(
        ts = props,
        widgets = widget_props,
        ts_raw = ts_props
    )
    attr(w_ctor, ".__init") <- initialize
    attr(w_ctor, ".__methods") <- list(
        exported = method_info$exported,
        observers = method_info$observers,
        exported_defs = method_info$exported_defs
    )

    w_ctor
}

# convert typed properties to ref class fields
widgetProperties <- function(properties) {
    fields <- lapply(properties, \(x) {
        if (inherits(x, "ts_widget")) {
            return("tsWidget")
        }
        if (!inherits(x, "ts_object")) {
            return(x)
        }
        t <- gsub("\\(.+", "", gsub("Robj.", "", x$return_type, fixed = TRUE))
        if (t == "dataframe") t <- "data.frame"
        t
    })
    fields
}

tsProps <- function(properties) {
    properties[sapply(properties, \(prop) {
        inherits(prop, "ts_object")
    })]
}
widgetProps <- function(properties) {
    properties[sapply(properties, \(prop) {
        inherits(prop, "ts_widget")
    })]
}

#' Create an Observer for Reactive Methods
#'
#' Wraps a method so it reacts to property changes. Used inside the
#' \code{methods} list of \code{createWidget()} to declare which properties
#' trigger the method.
#'
#' @param props Character vector of property names to observe.
#' @param fn The method body: a plain \code{function} (internal) or a
#'   \code{ts_function} (exported to JS).
#' @return A \code{ts_observer} object used by \code{createWidget()}.
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' createWidget("Example",
#'     properties = list(x = ts_integer(1L, default = 0L)),
#'     methods = list(
#'         on_x = observer("x", function() {
#'             cat("x changed to", .self$x, "\n")
#'         })
#'     )
#' )
#' }
observer <- function(props, fn) {
    structure(
        list(props = props, fn = fn),
        class = "ts_observer"
    )
}

# Wrap a function body with depth-counting auto-flush.
# Only flushes updateState() when the outermost method/handler returns.
wrap_auto_flush <- function(fn) {
    original_body <- body(fn)
    body(fn) <- bquote({
        .self$.call_depth <- .self$.call_depth + 1L
        on.exit({
            .self$.call_depth <- .self$.call_depth - 1L
            if (.self$.call_depth == 0L) .self$updateState()
        })
        .(original_body)
    })
    fn
}

# Process method definitions into ref class methods + observer metadata.
# Returns list with:
#   $methods   - named list of functions for setRefClass
#   $observers - named list: method_name -> character vector of watched props
#   $exported  - character vector of method names wrapped in ts_function
widgetMethods <- function(methods, auto_flush = TRUE) {
    methods <- as.list(methods)[-1]

    method_fns <- list()
    observers <- list()
    exported <- character()
    exported_defs <- list()

    for (nm in names(methods)) {
        x <- methods[[nm]]
        obs_props <- NULL

        # Unwrap observer() to get inner fn + watched props
        if (is.call(x) && identical(x[[1]], quote(observer))) {
            obs_props <- eval(x[[2]])
            x <- x[[3]] # inner function or ts_function call
        }

        # Extract raw function and determine if exported
        if (is.call(x) && identical(x[[1]], quote(ts_function))) {
            fn <- eval(x[[2]])
            exported <- c(exported, nm)
            exported_defs[[nm]] <- eval(x)
        } else {
            fn <- eval(x)
        }

        method_fns[[nm]] <- if (auto_flush) wrap_auto_flush(fn) else fn

        if (!is.null(obs_props)) {
            observers[[nm]] <- obs_props
        }
    }

    list(methods = method_fns, observers = observers, exported = exported,
        exported_defs = exported_defs)
}

# Build ocap wrappers for exported methods on a widget instance.
# Each wrapper calls instance$method(args...) with proper type signatures.
build_method_ocaps <- function(instance, method_defs) {
    if (length(method_defs) == 0) return(list())
    method_ocaps <- list()
    for (nm in names(method_defs)) {
        local({
            mn <- nm
            def <- method_defs[[mn]]
            arg_names <- names(def$args)

            # Build: function(arg1, ...) instance$method(arg1, ...)
            method_call <- as.call(c(
                list(call("$", quote(instance), as.name(mn))),
                lapply(arg_names, as.name)
            ))
            wrapper <- function() NULL
            if (length(arg_names) > 0) {
                formals(wrapper) <- as.pairlist(setNames(
                    rep(list(quote(expr = )), length(arg_names)),
                    arg_names
                ))
            }
            body(wrapper) <- method_call

            method_ocaps[[mn]] <<- do.call(
                ts_function,
                c(list(wrapper), def$args, list(result = def$result))
            )
        })
    }
    method_ocaps
}

#' Create Child Widget Connector
#'
#' Internal helper function to create connector functions for child widgets.
#' Used by the \code{add_child} method of \code{tsWidget}.
#'
#' @param child_instance The child widget instance
#' @param parent_instance The parent widget instance
#' @param property_name Name of the property containing the child
#' @param type_info Type information from the widget definition
#' @param widget_def The widget definition object
#' @return A TypeScript function constructor for the child widget
#' @keywords internal
#' @export
create_child_connector <- function(child_instance, parent_instance, property_name, type_info, widget_def) {
    # Use raw TypeScript type definitions
    ts_raw <- type_info$ts_raw
    child_method_meta <- attr(widget_def, ".__methods")
    child_method_defs <- if (!is.null(child_method_meta$exported_defs))
        child_method_meta$exported_defs
    else
        list()

    setStateType <- do.call(
        ts_list,
        lapply(ts_raw, \(prop) ts_optional(prop))
    )

    # Generate property methods for THIS child instance (not copied)
    child_props <- lapply(names(ts_raw), \(name) {
        prop <- ts_raw[[name]]
        if (!inherits(prop, "ts_object")) {
            return(NULL)
        }
        list(
            register = ts_function(
                function(fn, id) {
                    id <- child_instance$addPropHandler(
                        name, function() {
                            jsfun(fn)(child_instance$get(name))
                            invisible(NULL)
                        }
                    )
                    id
                },
                fn = js_function(x = prop, result = ts_null()),
                id = ts_character(1),
                result = ts_character(1L)
            ),
            get = ts_function(
                function() child_instance$get(name),
                result = prop
            ),
            set = ts_function(
                function(x) {
                    child_instance$set(name, x)
                    invisible()
                },
                x = prop
            )
        )
    })
    names(child_props) <- names(ts_raw)

    w_type <- ts_list(
        properties = do.call(
            ts_list,
            lapply(child_props, \(prop) do.call(ts_list, prop))
        ),
        children = ts_null(),
        methods = if (length(child_method_defs) > 0)
            do.call(ts_list, child_method_defs)
        else
            ts_list()
    )

    ts_function(
        function(fn) {
            child_instance$register(fn = if (is.null(fn)) NULL else fn)

            # Wire change-tracking: direct assignment auto-adds to changed list
            for (prop_name in names(ts_raw)) {
                local({
                    pn <- prop_name
                    child_instance[[paste0(pn, "Changed")]]$connect(function() {
                        if (!any(child_instance$changed == pn)) {
                            child_instance$changed <- c(child_instance$changed, pn)
                        }
                    })
                })
            }
            # Apply property defaults before initialize
            for (prop_name in names(ts_raw)) {
                default_val <- ts_raw[[prop_name]]$default
                if (!is.null(default_val)) {
                    child_instance$set(prop_name, default_val)
                }
            }

            # Wire observer methods to property signals
            method_meta <- attr(widget_def, ".__methods")
            if (!is.null(method_meta$observers)) {
                for (nm in names(method_meta$observers)) {
                    local({
                        handler_fn <- eval(substitute(
                            function() child_instance$METHOD(),
                            list(METHOD = as.name(nm))
                        ))
                        for (prop in method_meta$observers[[nm]]) {
                            child_instance$addPropHandler(prop, handler_fn,
                                auto_flush = FALSE)
                        }
                    })
                }
            }

            child_init <- attr(widget_def, ".__init")
            if (!is.null(child_init)) {
                has_parent_param <- "parent" %in% names(formals(child_init))
                if (has_parent_param) {
                    child_init(child_instance, parent_instance)
                } else {
                    child_init(child_instance)
                }
            }
            child_instance$updateState(all = TRUE)

            list(
                properties = child_props,
                children = NULL,
                methods = build_method_ocaps(child_instance, child_method_defs)
            )
        },
        fn = ts_optional(js_function(state = setStateType, result = ts_null())),
        result = w_type
    )
}

#' Convert JavaScript Function to R Function
#'
#' Converts a JavaScript function object to an R function that can be called
#' to send messages via Rserve's out-of-band messaging.
#'
#' @param x A JavaScript function object
#' @return An R function that sends messages via Rserve
#' @keywords internal
#' @export
jsfun <- function(x) {
    if (!inherits(x, "javascript_function")) {
        stop("Not a function")
    }
    function(...) Rserve::self.oobMessage(list(x, ...))
}

#' Base Widget Class
#'
#' Base reference class for all widgets created with \code{createWidget}.
#' This class provides the core functionality for reactive properties and
#' state synchronization with TypeScript.
#'
#' @export
tsWidget <- setRefClass("tsWidget",
    properties(
        fields = list(
            changed = "character",
            setState = "ANY",
            .child_connectors = "ANY",
            .childrenVersion = "integer",
            .property_names = "character",
            .call_depth = "integer"
        )
    ),
    methods = list(
        set = function(prop, value) {
            if (!any(.self$changed == prop)) {
                .self$changed <- c(.self$changed, prop)
            }
            .self[[prop]] <- value
        },
        get = function(prop) .self[[prop]],
        addPropHandler = function(prop, fn, auto_flush = TRUE) {
            if (auto_flush) {
                self <- .self
                wrapped <- function() {
                    self$.call_depth <- self$.call_depth + 1L
                    on.exit({
                        self$.call_depth <- self$.call_depth - 1L
                        if (self$.call_depth == 0L) self$updateState()
                    })
                    fn()
                }
                .self[[sprintf("%sChanged", prop)]]$connect(wrapped)
            } else {
                .self[[sprintf("%sChanged", prop)]]$connect(fn)
            }
        },
        updateState = function(all = FALSE) {
            if (is.null(.self$setState)) {
                return()
            }
            if (all) {
                # Use stored property names if available (for child widgets)
                if (!is.null(.self$.property_names)) {
                    chg <- .self$.property_names
                } else {
                    # Fallback for main widgets - get all fields except internal ones
                    all_fields <- names(.self$getRefClass()$fields())
                    chg <- all_fields[!all_fields %in% c("changed", "setState", ".child_connectors", ".childrenVersion", ".call_depth")]
                }
            } else {
                chg <- .self$changed
            }
            if (length(chg) == 0) {
                return()
            }

            x <- lapply(chg, \(p) .self$get(p))
            names(x) <- chg

            .self$setState(x)
            .self$changed <- character()
        },
        register = function(fn) {
            if (!is.null(.self$setState)) {
                warning("Already registered")
            } else {
                .self$setState <- jsfun(fn)
            }
        },
        batch = function(props, expr) {
            for (p in props) {
                .self[[paste0(p, "Changed")]]$block()
            }
            on.exit({
                for (p in props) {
                    .self[[paste0(p, "Changed")]]$unblock()
                    if (!any(.self$changed == p)) {
                        .self$changed <- c(.self$changed, p)
                    }
                }
                .self$updateState()
            })
            eval(substitute(expr), envir = parent.frame())
        },
        add_child = function(property, widget_def, parent_as = ".parent") {
            child_rc <- attr(widget_def, ".__refclass")
            child <- child_rc$new(NULL)

            # Set parent reference
            if (!is.null(parent_as)) {
                child[[parent_as]] <- .self
            }

            # Store child in parent
            .self[[property]] <- child

            # Get cached type info from widget definition
            props <- attr(widget_def, ".__props")

            # Store property names in child so updateState knows what to send
            child$.property_names <- names(props$ts_raw)

            # Create connector function that JS will call
            child_connector <- create_child_connector(
                child_instance = child,
                parent_instance = .self,
                property_name = property,
                type_info = props,
                widget_def = widget_def
            )

            if (is.null(.self$.child_connectors)) {
                .self$.child_connectors <- list()
            }

            # Store connector for JS access
            .self$.child_connectors[[property]] <- child_connector

            # Mark parent state as changed (for runtime additions)
            if (!is.null(.self$.childrenVersion)) {
                .self$set(".childrenVersion", .self$.childrenVersion + 1L)
            }

            invisible(child) # Return child instance for chaining
        }
    )
)