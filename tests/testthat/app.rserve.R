library(Rserve)
library(ts)

library(ts)

fn_mean <- ts_function(mean, x = ts_numeric(), result = ts_numeric(1))
fn_first <- ts_function(function(x = ts_character(-1)) x[1],
    result = ts_character(1)
)

sample_num <- ts_function(
    sample,
    x = ts_numeric(0),
    result = ts_numeric(1)
)
first.fns <- function() ts_app(list(
   fn_first = fn_first,
   fn_mean = fn_mean,
   sample_num = sample_num
))

oc.init <- function() Rserve:::ocap(first.fns)

Rserve::run.Rserve(
  websockets.port = 6311,
  websockets = TRUE,
  oob = TRUE,
  qap = FALSE,
  websockets.qap.oc = TRUE
)
