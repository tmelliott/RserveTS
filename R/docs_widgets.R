#' Create 'TypeScript'-compatible widgets
#'
#' Widgets are stateful reference classes shared between R and 'TypeScript'.
#' Use `createWidget()` to define one, wrap reactive methods with `observer()`,
#' and optionally declare typed actions with `widgetActions()`. Instances
#' inherit from the `tsWidget` reference class.
#'
#' @details
#' `createWidget()` returns a `ts_function()`-like constructor (class
#' `ts_widget`) that 'JavaScript' calls with a state setter. Locally you can
#' inspect or compile it with [ts_compile()] without a live 'Rserve' session;
#' calling `$call()` needs an out-of-band 'JavaScript' setter.
#'
#' ## Properties and methods
#' Each entry in `properties` is a `ts_*()` type (optionally with `default`).
#' Child widgets can be nested by passing another `createWidget()` result as a
#' property value.
#'
#' Each entry in `methods` is usually a `ts_function()` exported to
#' 'JavaScript'. Use `observer()` to run a method when properties change
#' (internal-only if the body is a plain `function`).
#'
#' ## Actions
#' Pass `actions = widgetActions(...)` to enable typed, named actions
#' dispatched from 'JavaScript'. Each action must be a named `ts_function()`
#' with exactly one payload argument. `strict` controls unknown action handling
#' (`"off"`, `"warn"`, or `"strict"`).
#'
#' ## `tsWidget` reference class
#' All widget instances inherit `tsWidget` and include:
#' - `set(prop, value)`, `get(prop)` -- field access with change tracking
#' - `updateState(all = FALSE)` -- push changed properties to 'TypeScript'
#' - `addPropHandler(prop, fn)` -- react to property changes
#' - `batch(props, expr)` -- batch updates into one state flush
#' - `add_child(property, widget_def)` -- attach a nested widget
#' - `create_dynamic_child(widget_def)` -- runtime child widget
#' - `destroy()` -- tear down the widget
#'
#' ## Client apps ('rserve-ts' / React)
#' Compile widgets with [ts_compile()] and import the generated schema into a
#' client app that connects to 'Rserve' via the ['rserve-ts'](https://www.npmjs.com/package/rserve-ts)
#' library. Obtain widget Ocaps from the compiled app schema (for example
#' `app.histogram` after connecting with `useRserve()` in React).
#'
#' In React, [`useWidget()`](https://www.npmjs.com/package/@tmelliott/react-rserve)
#' from `@tmelliott/react-rserve` wraps a compiled widget constructor and keeps
#' 'JavaScript' state in sync with R:
#' - `state` -- current property values (from R `updateState()`)
#' - `set` -- update properties from the client
#' - `methods` -- call exported `ts_function()` methods on the widget
#' - `children` -- nested widget connectors when properties include child widgets
#'
#' Action-enabled widgets (`actions = widgetActions(...)`) also expose
#' `capabilities`, `dispatchAction`, `undo`, and `redo` on the hook return value.
#'
#' @param name Widget class name (character).
#' @param properties Named list of typed properties (`ts_*()` objects, or nested
#'   widget constructors).
#' @param initialize Optional function run after defaults are applied; receives
#'   the widget instance when it has a parameter.
#' @param methods Named list of `ts_function()` methods and/or `observer()`
#'   reactive methods.
#' @param actions `FALSE`/`TRUE`, a list with `enabled`/`types`/`strict`, or a
#'   `widgetActions()` object.
#' @param auto_flush If `TRUE` (default), methods flush state to 'TypeScript'
#'   after they return; if `FALSE`, call `updateState()` manually.
#' @param .env Environment for the ref class definition (default
#'   `parent.frame()`).
#' @param ... For `createWidget()`: passed to the underlying `ts_function()`
#'   constructor. For `widgetActions()`: named `ts_function()` action definitions.
#' @param strict For `widgetActions()`: unknown action handling (`"off"`,
#'   `"warn"`, or `"strict"`).
#' @param enabled For `widgetActions()`: whether action support is enabled.
#' @param props For `observer()`: property names that trigger the method.
#' @param fn For `observer()`: method body (`function` or `ts_function()`).
#'
#' @return `createWidget()` returns a `ts_widget` constructor.
#'   `widgetActions()` returns a `ts_widget_actions` object.
#'   `observer()` returns a `ts_observer` object.
#'   `tsWidget` is the base reference class generator.
#'
#' @examples
#' Counter <- createWidget(
#'     name = "Counter",
#'     properties = list(count = ts_integer(1L, default = 0L)),
#'     methods = list(
#'         increment = ts_function(function(by = ts_integer(1L)) {
#'             .self$count <- as.integer(.self$count + by)
#'             .self$count
#'         }, result = ts_integer(1)),
#'         on_count = observer("count", function() NULL)
#'     )
#' )
#' inherits(Counter, "ts_widget")
#' ts_compile(Counter)
#'
#' @seealso [ts_function()], [ts_compile()], [type_objects],
#'   [Package '@tmelliott/react-rserve'](https://www.npmjs.com/package/@tmelliott/react-rserve)
#' @name createWidget
#' @md
NULL
