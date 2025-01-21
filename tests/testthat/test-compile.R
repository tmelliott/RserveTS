test_that("anonomous functions", {
    add <- ts_function(
        function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
        result = ts_numeric(1)
    )

    add_c <- ts_compile(add)
    expect_equal(add_c, "const add = Robj.ocap([z.number(), z.number()], Robj.numeric(1));")
})

# test_that("Complex functions", {
#     get_sample <- ts_function(
#         function(n = ts_numeric(1)) {
#             sample(values, n)
#         },
#         result = ts_numeric()
#     )

#     sampler <- ts_function(
#         function(values = ts_numeric()) {
#             list(
#                 get = get_sample$copy(),
#                 set = ts_function(
#                     function(value = ts_numeric()) {
#                         values <<- value
#                     }
#                 )
#             )
#         },
#         result = ts_list(
#             get = get_sample,
#             set = ts_function(NULL, value = ts_numeric())
#         )
#     )

#     sampler_c <- ts_compile(sampler)
#     s <- sampler_c(1:10)
#     expect_equal()
# })

test_that("Compile files", {
    f <- tempfile(fileext = ".rserve.ts")
    on.exit(unlink(f))
    res <- ts_compile("app.R", file = f)
    expect_true(file.exists(f))
})
