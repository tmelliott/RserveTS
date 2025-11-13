# Changelog

## RserveTS 0.7.0

- Enhanced
  [`createWidget()`](http://tomelliott.co.nz/RserveTS/reference/createWidget.md)
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
