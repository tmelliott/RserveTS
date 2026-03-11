test_that("ts_record has correct input_type and return_type", {
  x <- ts_record(ts_character(1))
  expect_match(x$input_type, "z\\.record\\(z\\.string\\(\\), .+\\)", perl = TRUE)
  expect_match(x$return_type, "Robj\\.list\\(z\\.record\\(z\\.string\\(\\), .+\\)\\)", perl = TRUE)
  expect_match(x$input_type, "z\\.string\\(\\)", fixed = FALSE)
  expect_true(grepl("Robj.character(1)", x$return_type, fixed = TRUE))
})

test_that("ts_record(ts_character(1)) check accepts named list of strings", {
  x <- ts_record(ts_character(1))
  expect_equal(x$check(list(a = "x", b = "y")), list(a = "x", b = "y"))
  expect_equal(x$check(list(default = "default", scatter = "scatter")),
    list(default = "default", scatter = "scatter"))
})

test_that("ts_record check rejects non-list", {
  x <- ts_record(ts_character(1))
  expect_error(x$check(1), "Expected a list")
  expect_error(x$check("a"), "Expected a list")
  expect_error(x$check(TRUE), "Expected a list")
})

test_that("ts_record check rejects unnamed list", {
  x <- ts_record(ts_character(1))
  expect_error(x$check(list("x", "y")), "Expected a named list")
})

test_that("ts_record check rejects list with wrong value type", {
  x <- ts_record(ts_character(1))
  expect_error(x$check(list(a = 1L, b = "y")), "Expected a string")
  expect_error(x$check(list(a = "x", b = 2)), "Expected a string")
})

test_that("ts_record(ts_integer(1)) check accepts named list of integers", {
  x <- ts_record(ts_integer(1))
  expect_equal(x$check(list(a = 1L, b = 2L)), list(a = 1L, b = 2L))
  expect_error(x$check(list(a = "x", b = 2L)), "Expected")
})

test_that("ts_compile with ts_record generates Robj.list(z.record(...))", {
  f <- ts_function(
    function() list(a = "1", b = "2"),
    result = ts_record(ts_character(1))
  )
  out <- ts_compile(f)
  expect_match(out, "Robj\\.list\\(z\\.record\\(z\\.string\\(\\)", perl = TRUE)
  expect_true(grepl("Robj.character(1)", out, fixed = TRUE))
})
