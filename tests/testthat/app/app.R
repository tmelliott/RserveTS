library(ts)

fn_mean <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
fn_first <- ts_function(function(x) x[1],
    x = ts_character(-1), result = ts_character(1)
)
