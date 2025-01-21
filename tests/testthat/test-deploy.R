test_that("Deploy converts ts functions into valid ocap lists", {
    on.exit(if (file.exists("app.rserve.R")) unlink("app.rserve.R"))
    ts_deploy("app.R")
    expect_true(file.exists("app.rserve.R"))
})
