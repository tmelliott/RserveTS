# Create an Observer for Reactive Methods

Wraps a method so it reacts to property changes. Used inside the
`methods` list of
[`createWidget()`](http://tomelliott.co.nz/RserveTS/reference/createWidget.md)
to declare which properties trigger the method.

## Usage

``` r
observer(props, fn)
```

## Arguments

- props:

  Character vector of property names to observe.

- fn:

  The method body: a plain `function` (internal) or a `ts_function`
  (exported to JS).

## Value

A `ts_observer` object used by
[`createWidget()`](http://tomelliott.co.nz/RserveTS/reference/createWidget.md).

## Examples

``` r
if (FALSE) { # \dontrun{
createWidget("Example",
    properties = list(x = ts_integer(1L, default = 0L)),
    methods = list(
        on_x = observer("x", function() {
            cat("x changed to", .self$x, "\n")
        })
    )
)
} # }
```
