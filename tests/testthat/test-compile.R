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
    # TODO: implement this
    skip()
    update <- NULL
    logger <- ts_function(
        function(f = ts_function(function() NULL, result = ts_void())) {
            update <<- f
        },
        result = ts_void()
    )

    ts_compile(logger)
})
