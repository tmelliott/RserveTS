test_that("Deploy converts ts functions into valid ocap lists", {
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
})
