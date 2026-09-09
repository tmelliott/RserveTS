test_that("anonomous functions", {
    add <- ts_function(
        function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
        result = ts_numeric(1)
    )

    add_c <- ts_compile(add)
    expect_equal(
        as.character(add_c),
        "export const add = Robj.ocap([z.number(), z.number()], Robj.numeric(1));"
    )
})


test_that("Compile files", {
    f <- tempfile(fileext = ".rserve.ts")
    on.exit(unlink(f))
    res <- ts_compile("sampler/app.R", filename = tools::file_path_sans_ext(f))
    expect_true(file.exists(f))
})

test_that("Compile files default to tempdir / RSERVETS_COMPILE_DIR", {
    src <- tempfile(fileext = ".R")
    writeLines(
        "add <- ts_function(function(x = ts_integer(1)) x + 1L, result = ts_integer(1), export = TRUE)",
        src
    )
    on.exit(unlink(src), add = TRUE)

    withr::local_options(RserveTS.compile_dir = NULL)
    withr::local_envvar(RSERVETS_COMPILE_DIR = "")

    out <- ts_compile(src)
    expect_equal(
        normalizePath(dirname(out), winslash = "/", mustWork = FALSE),
        normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    )
    expect_true(file.exists(paste0(out, ".ts")))
    expect_true(file.exists(paste0(out, ".R")))
    on.exit(unlink(paste0(out, c(".ts", ".R"))), add = TRUE)

    compile_dir <- tempfile("compile-dir-")
    dir.create(compile_dir)
    on.exit(unlink(compile_dir, recursive = TRUE), add = TRUE)
    withr::local_envvar(RSERVETS_COMPILE_DIR = compile_dir)
    out2 <- ts_compile(src)
    expect_equal(
        normalizePath(dirname(out2), winslash = "/", mustWork = TRUE),
        normalizePath(compile_dir, winslash = "/", mustWork = TRUE)
    )
    expect_true(file.exists(paste0(out2, ".ts")))

    withr::local_options(RserveTS.compile_dir = compile_dir)
    withr::local_envvar(RSERVETS_COMPILE_DIR = tempfile("ignored-"))
    out3 <- ts_compile(src)
    expect_equal(
        normalizePath(dirname(out3), winslash = "/", mustWork = TRUE),
        normalizePath(compile_dir, winslash = "/", mustWork = TRUE)
    )
})

test_that("functions that return new ocaps", {
    f1 <- ts_function(function() print("x1"), return = ts_void())
    f2 <- ts_function(function(x = ts_numeric(1)) x + 1,
        result = ts_numeric()
    )
    more_funs <- ts_function(
        function() list(f1 = f1, f2 = f2),
        result = ts_list(
            f1 = f1,
            f2 = f2
        )
    )

    ts_compile(more_funs)
})

test_that("new ocaps have the correct scope", {
    f2 <- ts_function(function() x_value, result = ts_numeric(1))
    f1 <- ts_function(
        function(x_value = ts_numeric(1)) {
            list(
                f2 = f2$copy()
            )
        },
        result = ts_list(
            f2 = f2
        )
    )
    expect_equal(f1$call(5)$f2$call(), 5)
})

test_that("functions that accept JS functions", {
    # See todo.md — JS function compile coverage not implemented yet
    skip("JS function compile coverage not implemented yet")
    update <- NULL
    logger <- ts_function(
        function(f = ts_function(function() NULL, result = ts_void())) {
            update <<- f
        },
        result = ts_void()
    )

    ts_compile(logger)
})

test_that("compiled widget schema includes action capabilities metadata", {
    src <- tempfile(fileext = ".R")
    base <- tempfile(fileext = ".rserve")
    ts_out <- sprintf("%s.ts", base)

    on.exit({
        if (file.exists(src)) unlink(src)
        if (file.exists(ts_out)) unlink(ts_out)
        r_out <- sprintf("%s.R", base)
        if (file.exists(r_out)) unlink(r_out)
    })

    writeLines(c(
        "ActionCompileWidget <- createWidget(",
        "  \"ActionCompileWidget\",",
        "  properties = list(value = ts_character(1L, default = \"\")),",
        "  actions = list(enabled = TRUE, types = c(\"SetValue\"), strict = \"warn\")",
        ")",
        "ActionCompileWidget$export <- TRUE"
    ), src)

    ts_compile(src, filename = base)
    expect_true(file.exists(ts_out))

    compiled <- paste(readLines(ts_out, warn = FALSE), collapse = "\n")
    expect_match(compiled, "capabilities")
    expect_match(compiled, "actions")
    expect_match(compiled, "strict")
})

test_that("compiled widgetActions emits object-style dispatchAction type", {
    src <- tempfile(fileext = ".R")
    base <- tempfile(fileext = ".rserve")
    ts_out <- sprintf("%s.ts", base)

    on.exit({
        if (file.exists(src)) unlink(src)
        if (file.exists(ts_out)) unlink(ts_out)
        r_out <- sprintf("%s.R", base)
        if (file.exists(r_out)) unlink(r_out)
    })

    writeLines(c(
        "ActionCompileWidget2 <- createWidget(",
        "  \"ActionCompileWidget2\",",
        "  properties = list(value = ts_character(1L, default = \"\")),",
        "  actions = widgetActions(",
        "    SetValue = ts_function(function(payload = ts_list(value = ts_character(1L))) { .self$value <- payload$value }),",
        "    strict = \"warn\"",
        "  )",
        ")",
        "ActionCompileWidget2$export <- TRUE"
    ), src)

    ts_compile(src, filename = base)
    expect_true(file.exists(ts_out))
    compiled <- paste(readLines(ts_out, warn = FALSE), collapse = "\n")
    expect_match(compiled, "dispatchAction")
    expect_match(compiled, "z\\.object\\(\\{ type: z\\.literal\\(\"SetValue\"\\)")
    expect_match(compiled, "payload")
})

test_that("compiled file emits named app schema and typed default export", {
    dir <- tempfile()
    dir.create(dir, recursive = TRUE)
    src <- file.path(dir, "KnownName.R")
    base <- file.path(dir, "KnownName.rserve")
    ts_out <- sprintf("%s.ts", base)

    on.exit({
        unlink(dir, recursive = TRUE)
    })

    writeLines(c(
        "SchemaCompileWidget <- createWidget(",
        "  \"SchemaCompileWidget\",",
        "  properties = list(value = ts_character(1L, default = \"\"))",
        ")",
        "SchemaCompileWidget$export <- TRUE"
    ), src)

    ts_compile(src, filename = base)
    expect_true(file.exists(ts_out))
    compiled <- readLines(ts_out, warn = FALSE)
    compiled_text <- paste(compiled, collapse = "\n")

    expect_match(compiled_text, "export const KnownNameAppSchema = \\{")
    expect_match(compiled_text, "SchemaCompileWidget")
    expect_match(compiled_text, "satisfies z\\.ZodRawShape")
    expect_match(compiled_text, "export type TKnownNameApp = z\\.infer<z\\.ZodObject<typeof KnownNameAppSchema, \"strip\">>;")
    expect_match(compiled_text, "export default KnownNameAppSchema;")
    expect_false(any(grepl("^export default \\{", compiled)))
})

test_that("compiled file falls back to app schema name for invalid basename", {
    dir <- tempfile()
    dir.create(dir, recursive = TRUE)
    src <- file.path(dir, "1-bad-name.R")
    base <- file.path(dir, "1-bad-name.rserve")
    ts_out <- sprintf("%s.ts", base)

    on.exit({
        unlink(dir, recursive = TRUE)
    })

    writeLines(c(
        "FallbackNameWidget <- createWidget(",
        "  \"FallbackNameWidget\",",
        "  properties = list(value = ts_character(1L, default = \"\"))",
        ")",
        "FallbackNameWidget$export <- TRUE"
    ), src)

    ts_compile(src, filename = base)
    expect_true(file.exists(ts_out))
    compiled_text <- paste(readLines(ts_out, warn = FALSE), collapse = "\n")
    expect_match(compiled_text, "export const appAppSchema = \\{")
    expect_match(compiled_text, "FallbackNameWidget")
    expect_match(compiled_text, "export type TAppApp = z\\.infer<z\\.ZodObject<typeof appAppSchema, \"strip\">>;")
    expect_match(compiled_text, "export default appAppSchema;")
})
