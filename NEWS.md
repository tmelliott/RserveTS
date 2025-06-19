- added initial support for js functions with `js_function()`

# ts 0.2.0

- Added support for recursive lists with `ts_recursive_list()`
- Added union types with `ts_union()`
- Exported array type `ts_array()` that now works with ts_objects
- Added optional types with `ts_optional()`
- Exported `ts_optional()` function
- Updated documentation

# ts 0.1.0 - Initial release

The initial developmental release of `ts`.

API expected to change in future releases as it is tested in various example
applications, so if you happen to use this in a project I suggest you
pin the version:

```r
# install.packages("remotes")
remotes::install_github("ws1/ts@0.1.0")
```

To get started, check the docs site at https://tomelliott.co.nz/ts/.
