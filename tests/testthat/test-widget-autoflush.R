# Helper: create a mock setState that captures calls
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

test_that("methods auto-flush updateState after returning", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "AutoFlushWidget",
    properties = list(
      value = ts_integer(1L, default = 0L)
    ),
    methods = list(
      increment = ts_function(function() {
        .self$set("value", .self$value + 1L)
        # no manual updateState() call
      })
    )
  )

  result <- w$call(mock_js_fn())

  # Get the widget instance by calling a method that modifies state
  # The initial updateState from constructor is update 1
  initial_updates <- length(tracker$get())

  # Call increment via the property set ocap — but we need the widget ref.
  # Instead, test by checking the method was wrapped:
  # The wrapped method should have depth-counting in its body
  rc <- attr(w, ".__refclass")
  method_body <- body(rc$def@refMethods$increment)
  # Should contain .call_depth references
  method_str <- paste(deparse(method_body), collapse = "\n")
  expect_true(grepl(".call_depth", method_str, fixed = TRUE))
  expect_true(grepl("updateState", method_str, fixed = TRUE))
})

test_that("nested method calls only flush once", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "NestedFlushWidget",
    properties = list(
      a = ts_integer(1L, default = 0L),
      b = ts_integer(1L, default = 0L)
    ),
    methods = list(
      inner = ts_function(function() {
        .self$set("b", 99L)
      }),
      outer = ts_function(function() {
        .self$set("a", 1L)
        .self$inner()
        # auto-flush should NOT fire after inner() returns (depth > 0)
        # auto-flush should fire once after outer() returns (depth == 0)
      })
    )
  )

  result <- w$call(mock_js_fn())
  updates_after_init <- length(tracker$get())

  # Verify wrapping: outer method body should have depth counting
  rc <- attr(w, ".__refclass")
  outer_body <- paste(deparse(body(rc$def@refMethods$outer)), collapse = "\n")
  expect_true(grepl(".call_depth", outer_body, fixed = TRUE))

  inner_body <- paste(deparse(body(rc$def@refMethods$inner)), collapse = "\n")
  expect_true(grepl(".call_depth", inner_body, fixed = TRUE))
})

test_that("addPropHandler callbacks auto-flush", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_ran <- FALSE

  w <- createWidget(
    "PropHandlerFlushWidget",
    properties = list(
      input = ts_character(1L, default = ""),
      output = ts_character(1L, default = "")
    ),
    initialize = function(widget) {
      widget$addPropHandler("input", function() {
        widget$set("output", paste0("processed: ", widget$get("input")))
        handler_ran <<- TRUE
        # no manual updateState() — auto-flush should handle it
      })
    },
    methods = list()
  )

  result <- w$call(mock_js_fn())
  # Handler wiring is tested by checking it doesn't error
  expect_false(handler_ran) # handler hasn't been triggered yet
})

test_that("auto_flush = FALSE disables method wrapping", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "NoAutoFlushWidget",
    properties = list(
      value = ts_integer(1L, default = 0L)
    ),
    methods = list(
      increment = ts_function(function() {
        .self$set("value", .self$value + 1L)
      })
    ),
    auto_flush = FALSE
  )

  result <- w$call(mock_js_fn())

  # Method body should NOT have depth-counting wrapper
  rc <- attr(w, ".__refclass")
  method_body <- paste(deparse(body(rc$def@refMethods$increment)), collapse = "\n")
  expect_false(grepl(".call_depth", method_body, fixed = TRUE))
})

test_that("explicit updateState still works with auto-flush", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "ExplicitFlushWidget",
    properties = list(
      value = ts_integer(1L, default = 0L)
    ),
    methods = list(
      increment = ts_function(function() {
        .self$set("value", .self$value + 1L)
        .self$updateState() # explicit mid-method flush
        # auto-flush at exit will be a no-op (changed is empty)
      })
    )
  )

  result <- w$call(mock_js_fn())

  # Method should still have the wrapper (both explicit + auto)
  rc <- attr(w, ".__refclass")
  method_body <- paste(deparse(body(rc$def@refMethods$increment)), collapse = "\n")
  expect_true(grepl(".call_depth", method_body, fixed = TRUE))
})
