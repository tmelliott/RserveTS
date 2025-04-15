test_that("Union types", {
    number_or_null <- ts_union(ts_numeric(1), ts_null())
    expect_equal(number_or_null$input_type, "z.union([z.number(), z.null()])")

    expect_equal(number_or_null$check(5), 5)
    expect_equal(number_or_null$check(NULL), NULL)

    expect_error(number_or_null$check("not a number"))
})

test_that("Optional arguments", {
    optional <- ts_optional(ts_numeric())
    expect_equal(optional$check(5), 5)
    expect_equal(optional$check(1:5), 1:5)

    expect_equal(optional$check(), NULL)
    expect_equal(optional$check(NULL), NULL)

    expect_error(optional$check("hello"))
})
