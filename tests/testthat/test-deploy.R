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

test_that("Deploy defaults to tempdir / RSERVETS_COMPILE_DIR", {
    src <- tempfile(fileext = ".R")
    writeLines(
        "add <- ts_function(function(x = ts_integer(1)) x + 1L, result = ts_integer(1), export = TRUE)",
        src
    )
    on.exit(unlink(src), add = TRUE)

    withr::local_options(RserveTS.compile_dir = NULL)
    withr::local_envvar(RSERVETS_COMPILE_DIR = "")

    out <- ts_deploy(src, silent = TRUE, run = "no")
    expect_equal(
        normalizePath(dirname(out), winslash = "/", mustWork = FALSE),
        normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    )
    expect_true(file.exists(out))
    on.exit(unlink(out), add = TRUE)

    deploy_dir <- tempfile("deploy-dir-")
    dir.create(deploy_dir)
    on.exit(unlink(deploy_dir, recursive = TRUE), add = TRUE)
    withr::local_envvar(RSERVETS_COMPILE_DIR = deploy_dir)
    out2 <- ts_deploy(src, silent = TRUE, run = "no")
    expect_equal(
        normalizePath(dirname(out2), winslash = "/", mustWork = TRUE),
        normalizePath(deploy_dir, winslash = "/", mustWork = TRUE)
    )
})
