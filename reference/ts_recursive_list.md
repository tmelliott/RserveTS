# Recursive list

For complex recursive lists — objects that can contain subcomponents of
the same (parent) type. For example, a Person with `name` and optional
`children` that are themselves Person objects.

## Usage

``` r
ts_recursive_list(values, recur)

ts_self(n = -1L)
```

## Arguments

- values:

  properties that define the base schema of the list; must be a named
  list.

- recur:

  a named list of properties that are added. These can use `ts_self()`.

- n:

  For `ts_self()`: number of elements — `n = 1` for a single nested
  object, or `n != 1` (default `-1`) for an array of the parent type.

## Value

`ts_recursive_list()` returns a ts object that accepts recursive lists.
`ts_self()` returns a marker used in `recur`.

## Details

Use `ts_self()` inside `recur` to mark those self-referential fields. By
default `ts_self()` means an array of the parent type; use `ts_self(1)`
for a single nested object.

Defining this type in 'Zod' is currently complicated, as the type has to
be pre-defined, and then extended after manually defining the Type. In
an upcoming version of 'zod' 4, this should be simplified. For now, it's
tricky.

## See also

Other type documentation:
[`type_objects`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md)

## Examples

``` r
person <- ts_recursive_list(
    list(name = ts_character(1)),
    list(children = ts_self())
)
echo_person <- ts_function(function() person, result = person)
ts_compile(echo_person, name = "echo_person")
#> export const echo_person = Robj.ocap(
#>   [],
#>   (function () {
#>     const baseObjectSchema = z.object({
#>       name: z.string(),
#>     });
#>     type ObjectType = z.infer<typeof baseObjectSchema> & {
#>       children: ObjectType[];
#>     };
#>     const listType = Robj.recursive_list<ObjectType>(
#>       baseObjectSchema,
#>       (self) => ({
#>         children: self.array().optional(),
#>       }),
#>     );
#>     return listType;
#>   })(),
#> );
```
