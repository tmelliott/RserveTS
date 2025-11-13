# Create a TypeScript-Compatible Widget

Creates a reference class-based widget that can interact with TypeScript
code. The widget supports reactive properties that can be observed from
both R and TypeScript, with automatic state synchronization.

## Usage

``` r
createWidget(
  name,
  properties = list(),
  initialize = NULL,
  methods = list(),
  .env = parent.frame(),
  ...
)
```

## Arguments

- name:

  Character string specifying the name of the widget class

- properties:

  Named list of typed properties for the widget. Each property should be
  a TypeScript type object that defines the property's type

- initialize:

  Optional initialization function that receives the widget instance and
  sets up initial state

- methods:

  Named list of methods to add to the widget class. Each method should
  be a `ts_function` object

- .env:

  Environment where the ref class should be created. Defaults to
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) which is
  the caller's environment (typically unlocked). Can be overridden
  (e.g., to `.GlobalEnv`) if needed.

- ...:

  Additional arguments passed to the TypeScript function constructor

## Value

A TypeScript function constructor that creates widget instances with
reactive properties and methods for TypeScript interoperability

## Details

Note that the object constructed takes a Javascript setter function as
argument, so calling `obj$call()` will fail.

The created widget includes built-in methods:

- `set(prop, value)`: Set a property value and mark it as changed

- `get(prop)`: Get a property value

- `addPropHandler(prop, fn)`: Register a handler for property changes

- `updateState(all = FALSE)`: Synchronize changed properties to
  TypeScript

Each property automatically gets TypeScript-accessible methods:

- `register(fn)`: Register a callback for property changes

- [`get()`](https://rdrr.io/r/base/get.html): Get the current property
  value

- `set(x)`: Set the property value

## Examples

``` r
# Create a simple counter widget
if (FALSE) { # \dontrun{
createWidget(
    name = "Counter",
    properties = list(count = ts_integer(1)),
    initialize = function(widget) {
        widget$set("count", 0)
    }
)
} # }
```
