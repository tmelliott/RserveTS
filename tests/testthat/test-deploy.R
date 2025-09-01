f <- tempfile(fileext = ".rserve.R")
on.exit(unlink(f))

test_that("Deploy converts ts functions into valid ocap lists", {
    ts_deploy("sampler/app.R", file = f)

    # remove Rserve call so we can inspect namespace
    out <- readLines(f)
    out <- out[1:(grep("run.Rserve", out) - 1)]
    writeLines(out, f)

    expect_true(file.exists(f))

    appe <- new.env()
    source(f, local = appe)

    x <- appe$oc.init()
    r <- resolve.ocap(x)()
    expect_s3_class(r$fn_first, "OCref")
})

test_that("Deploy exported functions only", {
    appe <- new.env()
    source(f, local = appe)

    x <- appe$oc.init()
    r <- resolve.ocap(x)()
    expect_null(r$hidden_function)
})
