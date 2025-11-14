# Create Child Widget Connector

Internal helper function to create connector functions for child
widgets. Used by the `add_child` method of `tsWidget`.

## Usage

``` r
create_child_connector(
  child_instance,
  parent_instance,
  property_name,
  type_info,
  widget_def
)
```

## Arguments

- child_instance:

  The child widget instance

- parent_instance:

  The parent widget instance

- property_name:

  Name of the property containing the child

- type_info:

  Type information from the widget definition

- widget_def:

  The widget definition object

## Value

A TypeScript function constructor for the child widget
