# # optional, check arguments - useful for debugging/development
# check_args(match.call(), formals())

test_that("anonomous function definitions", {
    add <- ts_function(
        function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
        result = ts_numeric(1)
    )

    expect_equal(add(1, 2), 3)
    expect_error(add("a", 2))
})

test_that("named function definitions", {
    sample_num <- ts_function(
        sample,
        x = ts_numeric(),
        result = ts_numeric()
    )

    x <- sample_num(1:10)
    expect_true(all(x %in% 1:10))
    expect_error(sample_num("a"))
})

test_that("function with complex return types", {
    sampler <- ts_function(
        function(x = ts_numeric()) {
            list(
                get = function(n) sample(x, n)
            )
        },
        result = ts_list(
            get = ts_function(
                NULL,
                n = ts_integer(1),
                result = ts_numeric(1)
            )
        )
    )

    s <- sampler(1:10)
})
