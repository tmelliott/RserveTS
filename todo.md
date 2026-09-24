# RserveTS backlog (not shipped)

Dev-only notes. This file is listed in `.Rbuildignore` and must not appear in the CRAN tarball.

## JS functions (`js_function()`)

- When compiling, automatically wrap in `self.oobMessage()` or `self.oobSend()` as necessary.
- Support “naked” JS functions (pass a function back to JavaScript from R).
- Support JS functions as **output** (R → JS), not only as input. `js_function()` currently emits `NULL` for the return type slot.

## Types / compile

- Pass objects from TypeScript to R for recursive list / object schemas (`ts_recursive_list` / related compile path — commented stub near the Zod `baseObjectSchema` generation).
- Tests: implement coverage for functions that accept JS functions (`tests/testthat/test-compile.R` currently `skip()`s).

## API / README

- Clarify / document which functions are exported in the entry-point OCAP (former README `# ts_export(app)` idea).
