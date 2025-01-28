test_that("anonomous functions", {
    add <- ts_function(
        function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
        result = ts_numeric(1)
    )

    add_c <- ts_compile(add)
    expect_equal(add_c, "const add = Robj.ocap([z.number(), z.number()], Robj.numeric(1));")
})


test_that("Compile files", {
    f <- tempfile(fileext = ".rserve.ts")
    on.exit(unlink(f))
    res <- ts_compile("sampler/app.R", filename = tools::file_path_sans_ext(f))
    expect_true(file.exists(f))
})
