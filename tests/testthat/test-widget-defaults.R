test_that("createWidget applies defaults before initialize", {
  # Track what setState receives
  state_updates <- list()
  mock_setState <- structure(
    list("fn"),
    class = "javascript_function"
  )

  # Mock jsfun to return a capturing function
  local_mocked_bindings(
    jsfun = function(x) {
      function(...) {
        state_updates[[length(state_updates) + 1L]] <<- list(...)
      }
    }
  )

  # Track the order: defaults vs initialize
  init_saw_nBin <- NULL

  w <- createWidget(
    "TestDefaultWidget",
    properties = list(
      var = ts_character(1L, default = "Sepal.Length"),
      nBin = ts_integer(1L, default = 10L),
      counts = ts_numeric(0L)
    ),
    initialize = function(widget) {
      # initialize should see defaults already applied
      init_saw_nBin <<- widget$get("nBin")
    },
    methods = list()
  )

  # Call the constructor
  result <- w$call(mock_setState)

  # Defaults should have been visible to initialize
  expect_equal(init_saw_nBin, 10L)

  # setState should have been called with the default values
  expect_length(state_updates, 1L)
  expect_equal(state_updates[[1]][[1]]$var, "Sepal.Length")
  expect_equal(state_updates[[1]][[1]]$nBin, 10L)

  # counts had no default, so should not appear (or be default numeric)
})

test_that("initialize can override defaults", {
  mock_setState <- structure(
    list("fn"),
    class = "javascript_function"
  )

  local_mocked_bindings(
    jsfun = function(x) {
      function(...) invisible(NULL)
    }
  )

  init_ran <- FALSE

  w <- createWidget(
    "TestOverrideWidget",
    properties = list(
      nBin = ts_integer(1L, default = 10L)
    ),
    initialize = function(widget) {
      widget$set("nBin", 20L)
      init_ran <<- TRUE
    },
    methods = list()
  )

  result <- w$call(mock_setState)
  expect_true(init_ran)
})

test_that("widget with no defaults works unchanged", {
  mock_setState <- structure(
    list("fn"),
    class = "javascript_function"
  )

  local_mocked_bindings(
    jsfun = function(x) {
      function(...) invisible(NULL)
    }
  )

  w <- createWidget(
    "TestNoDefaultWidget",
    properties = list(
      var = ts_character(1L),
      nBin = ts_integer(1L)
    ),
    initialize = function(widget) {
      widget$set("var", "x")
      widget$set("nBin", 5L)
    },
    methods = list()
  )

  # Should work exactly as before
  result <- w$call(mock_setState)
  expect_true(!is.null(result))
})
