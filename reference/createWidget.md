# Create 'TypeScript'-compatible widgets

Widgets are stateful reference classes shared between R and
'TypeScript'. Use `createWidget()` to define one, wrap reactive methods
with `observer()`, and optionally declare typed actions with
`widgetActions()`. Instances inherit from the `tsWidget` reference
class.

## Usage

``` r
widgetActions(..., strict = "warn", enabled = TRUE)

createWidget(
  name,
  properties = list(),
  initialize = NULL,
  methods = list(),
  actions = FALSE,
  auto_flush = TRUE,
  .env = parent.frame(),
  ...
)

observer(props, fn)
```

## Arguments

- ...:

  For `createWidget()`: passed to the underlying
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  constructor. For `widgetActions()`: named
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  action definitions.

- strict:

  For `widgetActions()`: unknown action handling (`"off"`, `"warn"`, or
  `"strict"`).

- enabled:

  For `widgetActions()`: whether action support is enabled.

- name:

  Widget class name (character).

- properties:

  Named list of typed properties (`ts_*()` objects, or nested widget
  constructors).

- initialize:

  Optional function run after defaults are applied; receives the widget
  instance when it has a parameter.

- methods:

  Named list of
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  methods and/or `observer()` reactive methods.

- actions:

  `FALSE`/`TRUE`, a list with `enabled`/`types`/`strict`, or a
  `widgetActions()` object.

- auto_flush:

  If `TRUE` (default), methods flush state to 'TypeScript' after they
  return; if `FALSE`, call `updateState()` manually.

- .env:

  Environment for the ref class definition (default
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)).

- props:

  For `observer()`: property names that trigger the method.

- fn:

  For `observer()`: method body (`function` or
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)).

## Value

`createWidget()` returns a `ts_widget` constructor. `widgetActions()`
returns a `ts_widget_actions` object. `observer()` returns a
`ts_observer` object. `tsWidget` is the base reference class generator.

## Details

`createWidget()` returns a
[`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)-like
constructor (class `ts_widget`) that 'JavaScript' calls with a state
setter. Locally you can inspect or compile it with
[`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
without a live 'Rserve' session; calling `$call()` needs an out-of-band
'JavaScript' setter.

### Properties and methods

Each entry in `properties` is a `ts_*()` type (optionally with
`default`). Child widgets can be nested by passing another
`createWidget()` result as a property value.

Each entry in `methods` is usually a
[`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
exported to 'JavaScript'. Use `observer()` to run a method when
properties change (internal-only if the body is a plain `function`).

### Actions

Pass `actions = widgetActions(...)` to enable typed, named actions
dispatched from 'JavaScript'. Each action must be a named
[`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
with exactly one payload argument. `strict` controls unknown action
handling (`"off"`, `"warn"`, or `"strict"`).

### `tsWidget` reference class

All widget instances inherit `tsWidget` and include:

- `set(prop, value)`, `get(prop)` – field access with change tracking

- `updateState(all = FALSE)` – push changed properties to 'TypeScript'

- `addPropHandler(prop, fn)` – react to property changes

- `batch(props, expr)` – batch updates into one state flush

- `add_child(property, widget_def)` – attach a nested widget

- `create_dynamic_child(widget_def)` – runtime child widget

- `destroy()` – tear down the widget

### Client apps ('rserve-ts' / React)

Compile widgets with
[`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
and import the generated schema into a client app that connects to
'Rserve' via the ['rserve-ts'](https://www.npmjs.com/package/rserve-ts)
library. Obtain widget Ocaps from the compiled app schema (for example
`app.histogram` after connecting with `useRserve()` in React).

In React,
[`useWidget()`](https://www.npmjs.com/package/@tmelliott/react-rserve)
from `@tmelliott/react-rserve` wraps a compiled widget constructor and
keeps 'JavaScript' state in sync with R:

- `state` – current property values (from R `updateState()`)

- `set` – update properties from the client

- `methods` – call exported
  [`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md)
  methods on the widget

- `children` – nested widget connectors when properties include child
  widgets

Action-enabled widgets (`actions = widgetActions(...)`) also expose
`capabilities`, `dispatchAction`, `undo`, and `redo` on the hook return
value.

## See also

[`ts_function()`](https://tomelliott.co.nz/RserveTS/reference/ts_function.md),
[`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md),
[type_objects](https://tomelliott.co.nz/RserveTS/reference/type_objects.md),
[Package
'@tmelliott/react-rserve'](https://www.npmjs.com/package/@tmelliott/react-rserve)

## Examples

``` r
Counter <- createWidget(
    name = "Counter",
    properties = list(count = ts_integer(1L, default = 0L)),
    methods = list(
        increment = ts_function(function(by = ts_integer(1L)) {
            .self$count <- as.integer(.self$count + by)
            .self$count
        }, result = ts_integer(1)),
        on_count = observer("count", function() NULL)
    )
)
inherits(Counter, "ts_widget")
#> [1] TRUE
ts_compile(Counter)
#> export const Counter = Robj.ocap(
#>   [
#>     z.union([
#>       Robj.js_function(
#>         [z.object({ count: z.union([z.number(), z.undefined()]) })],
#>         z.null(),
#>       ),
#>       z.undefined(),
#>     ]),
#>   ],
#>   Robj.list({
#>     properties: Robj.list({
#>       count: Robj.list({
#>         register: Robj.ocap(
#>           [Robj.js_function([z.number()], z.null()), z.string()],
#>           Robj.character(1),
#>         ),
#>         get: Robj.ocap([], Robj.integer(1)),
#>         set: Robj.ocap([z.number()], Robj.null()),
#>       }),
#>     }),
#>     children: Robj.list(),
#>     capabilities: Robj.list({
#>       actions: Robj.list({
#>         enabled: Robj.logical(1),
#>         types: Robj.character(0),
#>         strict: Robj.character(1),
#>       }),
#>     }),
#>     methods: Robj.list({ increment: Robj.ocap([z.number()], Robj.integer(1)) }),
#>   }),
#> );
```
