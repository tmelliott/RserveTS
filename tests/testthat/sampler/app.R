library(ts)

fn_mean <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
fn_first <- ts_function(function(x = ts_character()) x[1],
    result = ts_character(1)
)

sample_num <- ts_function(
    sample,
    x = ts_numeric(0),
    size = ts_integer(1),
    result = ts_numeric()
)

get_later <- ts_function(function() stored_value, result = ts_numeric(1))
save_for_later <- ts_function(
    function(stored_value = ts_numeric(1)) {
        print(paste("Storing value", stored_value))
        list(
            get = get_later$copy()
        )
    },
    result = ts_list(get = get_later)
)

optional_fn <- ts_function(
    # function(x = ts_optional(ts_numeric(1))) {
    function(x = ts_undefined()) {
        print(x)
        !is.null(x)
    },
    result = ts_logical(1)
)
