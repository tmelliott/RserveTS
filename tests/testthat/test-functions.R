# # optional, check arguments - useful for debugging/development
# check_args(match.call(), formals())

# test_that("function definitions", {
#     add <- ts_fun(
#         function(a = ts_numeric(1), b = ts_numeric(1)) a + b,
#         result = ts_numeric(1)
#     )

#     expect_equal(add(1, 2), 3)
#     expect_error(add("a", 2))
# })
