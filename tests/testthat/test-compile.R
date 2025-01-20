test_that("anonomous functions", {
    add <- ts_function(
        function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
        result = ts_numeric(1)
    )

    ts_compile(add)

    # expect_equal(add$call(1, 2), 3)
    # expect_error(add$call("a", 2))
})

test_that("Complex functions", {
    get_sample <- ts_function(
        function(n = ts_numeric(1)) {
            sample(values, n)
        },
        result = ts_numeric()
    )

    sampler <- ts_function(
        function(values = ts_numeric()) {
            list(
                get = get_sample$copy(),
                set = ts_function(
                    function(value = ts_numeric()) {
                        values <<- value
                    }
                )
            )
        },
        result = ts_list(
            get = get_sample,
            set = ts_function(NULL, value = ts_numeric())
        )
    )

    ts_compile(sampler)
})

test_that("Compile files", {
    on.exit(if (file.exists("app.d.ts")) unlink("app.d.ts"))
    res <- ts_compile("app.R")
})
