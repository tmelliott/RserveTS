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
