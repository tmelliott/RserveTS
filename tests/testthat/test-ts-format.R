has_prettier_or_npx <- function() {
    nzchar(Sys.which("prettier")) || nzchar(Sys.which("npx"))
}

test_that("format=TRUE produces more lines than unformatted output when Prettier available", {
    skip_if_not(has_prettier_or_npx())

    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)

    src <- file.path(dir, "Fmt.R")
    writeLines(
        c(
            "f <- ts_function(function(x = ts_integer(1)) x, result = ts_integer(1))",
            "f$export <- TRUE"
        ),
        src
    )
    base_plain <- file.path(dir, "Plain.rserve")
    base_fmt <- file.path(dir, "Pretty.rserve")
    ts_compile(src, filename = base_plain, format = FALSE)
    ts_compile(src, filename = base_fmt, format = TRUE)

    plain_lines <- readLines(paste0(base_plain, ".ts"), warn = FALSE)
    fmt_lines <- readLines(paste0(base_fmt, ".ts"), warn = FALSE)
    expect_false(identical(paste(plain_lines, collapse = "\n"), paste(fmt_lines, collapse = "\n")))

    txt <- paste(fmt_lines, collapse = "\n")
    expect_match(txt, "export const f")
})

test_that("format=TRUE errors when formatter exits non-zero", {
    skip_if_not(.Platform$OS.type == "unix")
    false_bin <- Sys.which("false")
    skip_if_not(nzchar(false_bin))

    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)

    src <- file.path(dir, "Fmt2.R")
    writeLines(
        c(
            "g <- ts_function(function(x = ts_integer(1)) x, result = ts_integer(1))",
            "g$export <- TRUE"
        ),
        src
    )
    base <- file.path(dir, "Fmt2.rserve")

    expect_error(
        ts_compile(src, filename = base, format = TRUE, prettier_cmd = false_bin),
        "Prettier exited with status 1"
    )
})
