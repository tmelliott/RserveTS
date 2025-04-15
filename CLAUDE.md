# CLAUDE.md - Assistant Guidelines

## Build/Test Commands
- Install package: `make install` or `R CMD INSTALL .`
- Check package: `make check` or `Rscript -e "devtools::check()"`
- Run all tests: `make test` or `Rscript -e "devtools::test()"`
- Run single test: `Rscript -e "devtools::test_file('tests/testthat/test-file-name.R')"`
- Document package: `make document` or `Rscript -e "devtools::document()"`
- TypeScript compile: `Rscript -e 'ts::ts_compile("app.R"); ts::ts_deploy("app.R")'`
- Start Rserve: `Rscript app.rserve.R` (in appropriate directory)

## Code Style Guidelines
- R functions use snake_case (e.g., `ts_function`, `ts_compile`)
- TypeScript uses strict typing and ES module imports
- R type constructors: `ts_numeric()`, `ts_character()`, `ts_list()`, etc.
- File naming convention: `.rserve.R` (R server), `.rserve.ts` (TypeScript definitions)
- Error handling: Prefer strong typing over try/catch where possible
- Tests use testthat with `test_that()` blocks and `expect_*` assertions

This package bridges R and TypeScript, enabling type-safe R functions to be called from TypeScript.