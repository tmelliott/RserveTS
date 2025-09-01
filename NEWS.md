# ts 0.4.0

- (breaking) add `export = ` argument ts `ts_function()` (`FALSE` by default) to control which functions are exposed with the first connection

# ts 0.3.0

- added initial support for js functions with `js_function()`
- fix return type for ts_undefined(), ts_void(), ts_union() (#6)
- add unnamed array-type list
- add basic error handling (try() around all functions)

# ts 0.2.0

- Added support for recursive lists with `ts_recursive_list()`
- Added support for passing in Javascript functions with `js_function()`
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
