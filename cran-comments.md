## Test environments

* local Arch Linux, R 4.5.2
* GitHub Actions:
  * macos-latest (R release)
  * windows-latest (R release)
  * ubuntu-latest (R devel, release, oldrel-1)
* win-builder (R-release, R-devel)

## R CMD check results

Local (Arch Linux, R 4.5.2): 0 errors | 0 warnings | 0 notes

GitHub Actions (all five matrix jobs): 0 errors | 0 warnings | 0 notes

win-builder (R-release, R-devel): 0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

This is a new submission (first release to CRAN).

`objectProperties` and `objectSignals` are listed in Depends (not Imports)
because `createWidget()` defines reference classes via `objectProperties`
whose generated fields call `Signal()` unbound; those packages must be
attached for widgets defined in user environments (e.g. `globalenv()`).

URLs pointing at <https://www.npmjs.com/> (companion `rserve-ts` package)
may be reported as inaccessible by automated checkers (HTTP 403) while
remaining valid in a normal browser.
