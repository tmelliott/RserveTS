test_that("boolean type", {
  x <- ts_logical()
  expect_equal(x$check(TRUE), TRUE)
  expect_error(x$check(1))

  x1 <- ts_logical(1)
  expect_equal(x1$check(TRUE), TRUE)
  expect_error(x1$check(c(TRUE, FALSE)))

  x2 <- ts_logical(3)
  expect_equal(x2$check(c(TRUE, FALSE, TRUE)), c(TRUE, FALSE, TRUE))
  expect_error(x2$check(FALSE))
})

test_that("integer type", {
  x <- ts_integer()
  expect_equal(x$check(1L), 1L)
  expect_equal(x$check(1:10), 1:10)
  expect_error(x$check("a"))
  expect_error(x$check(1.5))

  x1 <- ts_integer(1)
  expect_equal(x1$check(1L), 1L)
  expect_error(x1$check(c(1L, 2L)))

  x2 <- ts_integer(3)
  expect_equal(x2$check(c(1:3)), c(1:3))
  expect_error(x2$check(1L))
})

test_that("numeric type", {
  x <- ts_numeric()
  expect_equal(x$check(1), 1)
  expect_equal(x$check(1:10 + 0.5), 1:10 + 0.5)
  expect_error(x$check("a"))

  x1 <- ts_numeric(1)
  expect_equal(x1$check(1), 1)
  expect_error(x1$check(c(1, 2)))

  x2 <- ts_numeric(3)
  expect_equal(x2$check(c(1, 2, 3)), c(1, 2, 3))
  expect_error(x2$check(1))
})

test_that("character type", {
  x <- ts_character()
  expect_equal(x$check("a"), "a")
  expect_equal(x$check(c("a", "b")), c("a", "b"))
  expect_error(x$check(1))

  x1 <- ts_character(1)
  expect_equal(x1$check("a"), "a")
  expect_error(x1$check(c("a", "b")))

  x2 <- ts_character(3)
  expect_equal(x2$check(c("a", "b", "c")), c("a", "b", "c"))
  expect_error(x2$check("a"))
})

test_that("factor type (no levels)", {
  x <- ts_factor()
  expect_equal(x$check(factor("a")), factor("a"))
  expect_error(x$check("a"))
  expect_error(x$check(1))
})

test_that("factor type (with levels)", {
  x <- ts_factor(levels = c("a", "b"))
  expect_equal(
    x$check(factor("a", levels = c("a", "b"))),
    factor("a", levels = c("a", "b"))
  )
  expect_error(x$check(factor("a", levels = c("a"))))
  expect_error(x$check("a"))
  expect_error(x$check(1))
})

test_that("list type - default", {
  x <- ts_list()
  expect_equal(x$check(list()), list())
  expect_equal(x$check(list(a = 1, b = 2)), list(a = 1, b = 2))
  expect_error(x$check(1))
})

test_that("list type - named", {
  x <- ts_list(a = ts_integer(1), b = ts_numeric(1))
  expect_equal(x$check(list(a = 1L, b = 2)), list(a = 1L, b = 2))
  expect_error(x$check(1))
  expect_error(x$check(list()))
})

test_that("list type - unnamed", {
  x <- ts_list(
    ts_integer(1), ts_character(1),
    ts_list(a = ts_integer(1))
  )
  expect_equal(x$check(list(
    1L, "a",
    list(a = 1L)
  )), list(
    1L, "a",
    list(a = 1L)
  ))
  expect_error(x$check(1))
  expect_error(x$check(list()))
})
