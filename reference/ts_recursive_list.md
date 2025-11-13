# Recursive list

For complex recursive lists. These are objects that can contain
subcomponents of the same (parent) type. e.g., Person can have name,
dob, properties, and 'children' which is an (optional) array of Person
objects.

## Usage

``` r
ts_recursive_list(values, recur)
```

## Arguments

- values:

  properties that define the base schema of the list; must be a named
  list.

- recur:

  a named list of properties that are added. These can use the
  'ts_self()' helper.

## Value

A ts object that accepts recursive lists.

## Details

Defining this type in Zod is currently complicated, as the type has to
be pre-defined, and then extended after manually defining the Type. In
an upcoming version of zod 4, this should be simplified. For now, it's
tricky.

## Examples

``` r
r_list <- ts_recursive_list(
    list(name = ts_character(1)),
    list(children = ts_self())
)
```
