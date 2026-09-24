# Changelog

## RserveTS 0.8.3

CRAN release: 2026-09-24

- `ts_compile.character()` and
  [`ts_deploy()`](https://tomelliott.co.nz/RserveTS/reference/ts_deploy.md)
  default output to under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) (or
  `options(RserveTS.compile_dir = ...)` / env `RSERVETS_COMPILE_DIR`)
  instead of writing beside the source file; pass `filename` / `file` to
  choose an explicit path.
- Consolidate `ts_*()` type constructors onto a single `type_objects`
  help page
  ([`?ts_numeric`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md),
  [`?ts_dataframe`](https://tomelliott.co.nz/RserveTS/reference/type_objects.md),
  etc.).
- Consolidate widget docs onto a single `createWidget` help page
  ([`?widgetActions`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md),
  [`?observer`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md),
  [`?tsWidget`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)).
- [`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
  hoists nested widget connectors to named top-level TypeScript exports
  (`export const` + `T*` aliases), ordered by child-widget dependencies.
- `ts_compile.character()` gains `format` and `prettier_cmd` to run
  Prettier (or a compatible CLI) on generated `.ts` output;
  `prettier_cmd` defaults to `getOption("RserveTS.prettier_cmd")`
  (`NULL` until set), with fallback to `RserveTS_PRETTIER_CMD` / `PATH`.
- `ts_compile.ts_function()` also honours `format` / `prettier_cmd`;
  both methods default `format` to
  `getOption("RserveTS.format", FALSE)`.
- [`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
  roxygen documents `format` / `prettier_cmd` and clarifies file-path vs
  `ts_function` arguments.
- add
  [`widgetActions()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  for typed widget action definitions used by
  `createWidget(actions = ...)`.
- restore separate
  [`createWidget()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  and
  [`widgetActions()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  reference docs/exports and update pkgdown reference indexing.
- stabilize widget observer tests by replacing parent-environment
  assignments with explicit test state environments.

## RserveTS 0.8.2

- fix
  [`ts_compile()`](https://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
  app schema output by emitting a named `*AppSchema` object with typed
  default export.
- add compile regression test coverage for named app schema and default
  export shape.

## RserveTS 0.8.1

- ts_compile() enhancements
- add debug logging utilities

## RserveTS 0.8.0

- Simplify widget creation with
  [`createWidget()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  function. Now takes default values for properties, and automatically
  flushes state changes to TypeScript after execution.

## RserveTS 0.7.2

- Fix bug in JS type for factors.

## RserveTS 0.7.0

- Enhanced
  [`createWidget()`](https://tomelliott.co.nz/RserveTS/reference/createWidget.md)
  function with support for child widgets. Widgets can now contain other
  widgets as properties, enabling hierarchical widget structures. The
  function now properly handles child widget initialization and state
  management.
- Add `objectSignals` as dependency
- Export `tsWidget` base class for proper class lookup

## RserveTS 0.6.0

- Add new widget functionality to simplify creating widget objects that
  store state that can be modified by the JS application. The node
  package
  [`@tmelliott/react-rserve`](https://www.npmjs.com/package/@tmelliott/react-rserve)
  contains hooks for working with these widgets.

- fix bug (use inherits() instead of class() == ““)

## RserveTS 0.5.0

- rename package to ‘RserveTS’ to make it more obvious what the package
  does, and to avoid any potential confusion with “time series”
