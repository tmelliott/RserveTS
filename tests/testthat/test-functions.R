test_that("anonomous function definitions", {
    add <- ts_function(
        function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
        result = ts_numeric(1)
    )

    expect_equal(add$call(1, 2), 3)
    expect_s3_class(add$call("a", 2), "try-error")
})

test_that("named function definitions", {
    sample_num <- ts_function(
        sample,
        x = ts_numeric(),
        result = ts_numeric()
    )

    x <- sample_num$call(1:10)
    expect_true(all(x %in% 1:10))
    expect_error(sample_num("a"))
})

test_that("void return types", {
    print_x <- ts_function(
        function(x = ts_numeric()) {
            print(x)
            return(NULL)
        }
    )

    expect_output(z <- print_x$call(1:10))
    expect_null(z)
})

test_that("function with complex return types", {
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

    s <- sampler$call(1:10)
    expect_type(s$get$call(2), "integer")

    expect_silent(s$set$call(100:200))
    expect_gte(s$get$call(1), 100)
})
