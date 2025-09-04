library(RserveTS)

get_hist <- ts_function(
    function(bins = ts_integer(1)) {
        h <- hist(faithful$waiting, breaks = bins, plot = FALSE)
        data.frame(x = h$mids, y = h$density)
    },
    result = ts_dataframe(x = ts_numeric(0), y = ts_numeric(0))
)
get_smoother <- ts_function(
    function(bandwidth = ts_numeric(1)) {
        d <- density(faithful$waiting, bw = bandwidth)
        data.frame(x = d$x, y = d$y)
    },
    result = ts_dataframe(x = ts_numeric(0), y = ts_numeric(0))
)
