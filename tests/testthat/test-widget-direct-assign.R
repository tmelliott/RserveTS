# Helper: mock JS function
mock_js_fn <- function() {
  structure(list("fn"), class = "javascript_function")
}

capture_state_updates <- function() {
  updates <- list()
  list(
    mock = function(x) {
      function(...) {
        updates[[length(updates) + 1L]] <<- list(...)
      }
    },
    get = function() updates
  )
}

test_that("direct assignment tracks changes for updateState", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  widget_ref <- NULL

  w <- createWidget(
    "DirectAssignWidget",
    properties = list(
      x = ts_integer(1L, default = 0L),
      y = ts_character(1L, default = "")
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    }
  )

  result <- w$call(mock_js_fn())
  updates_before <- length(tracker$get())

  # Direct assignment should track change
  widget_ref$x <- 42L
  widget_ref$updateState()

  updates_after <- length(tracker$get())
  expect_equal(updates_after - updates_before, 1L)

  # Check that the update included x
  last_update <- tracker$get()[[updates_after]]
  expect_true("x" %in% names(last_update[[1]]))
  expect_equal(last_update[[1]]$x, 42L)
})

test_that("direct assignment and set() are equivalent", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  widget_ref <- NULL

  w <- createWidget(
    "EquivAssignWidget",
    properties = list(
      a = ts_integer(1L, default = 0L),
      b = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    }
  )

  result <- w$call(mock_js_fn())

  # set() and direct assignment should both track
  widget_ref$set("a", 10L)
  widget_ref$b <- 20L
  expect_true("a" %in% widget_ref$changed)
  expect_true("b" %in% widget_ref$changed)
})

test_that("direct assignment triggers observer handlers", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_called <- FALSE
  widget_ref <- NULL

  w <- createWidget(
    "DirectObsWidget",
    properties = list(
      x = ts_integer(1L, default = 0L),
      y = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    methods = list(
      on_x = observer("x", function() {
        handler_called <<- TRUE
        .self$set("y", .self$x * 2L)
      })
    )
  )

  result <- w$call(mock_js_fn())
  handler_called <- FALSE

  # Direct assignment should trigger the observer
  widget_ref$x <- 5L

  expect_true(handler_called)
  expect_equal(widget_ref$y, 10L)
})

test_that("change tracking does not duplicate entries", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  widget_ref <- NULL

  w <- createWidget(
    "NoDupWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    }
  )

  result <- w$call(mock_js_fn())
  widget_ref$changed <- character()

  # Set the same property multiple times
  widget_ref$x <- 1L
  widget_ref$x <- 2L
  widget_ref$x <- 3L

  # Should only appear once in changed list
  expect_equal(sum(widget_ref$changed == "x"), 1L)
})
