test_that("Deploy converts ts functions into valid ocap lists", {
    skip()
    f <- tempfile(fileext = ".rserve.R")
    on.exit(unlink(f))
    ts_deploy("sampler/app.R", file = f)
    expect_true(file.exists(f))
})
