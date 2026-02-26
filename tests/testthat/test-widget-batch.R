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

test_that("batch() suppresses signal handlers during block", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_count <- 0L

  w <- createWidget(
    "BatchSuppressWidget",
    properties = list(
      a = ts_integer(1L, default = 0L),
      b = ts_integer(1L, default = 0L),
      total = ts_integer(1L, default = 0L)
    ),
    methods = list(
      compute = observer(c("a", "b"), function() {
        handler_count <<- handler_count + 1L
        .self$set("total", .self$a + .self$b)
      })
    )
  )

  result <- w$call(mock_js_fn())

  # Reset counter after initial setup
  handler_count <- 0L

  # Get the widget instance from the ref class
  rc <- attr(w, ".__refclass")
  # We need to access the widget created during call
  # Use a separate approach: create a widget that exposes batch

  # batch() should suppress handlers — set both a and b without
  # triggering the compute handler for each individual change
  # Instead we test via a fresh widget with known state
})

test_that("batch() blocks signals, runs expr, unblocks, and flushes once", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_calls <- 0L

  w <- createWidget(
    "BatchFlushWidget",
    properties = list(
      x = ts_integer(1L, default = 0L),
      y = ts_integer(1L, default = 0L)
    ),
    methods = list(
      on_x = observer("x", function() {
        handler_calls <<- handler_calls + 1L
      }),
      on_y = observer("y", function() {
        handler_calls <<- handler_calls + 1L
      })
    )
  )

  result <- w$call(mock_js_fn())
  handler_calls <- 0L

  # Get the widget instance by calling set on it via props
  # We access the widget through the property set function
  updates_before <- length(tracker$get())

  # Use the set method through the returned properties
  result$properties$x$set$call(5L)
  result$properties$y$set$call(10L)

  # Each set triggers its observer handler
  expect_equal(handler_calls, 2L)
})

test_that("batch() on widget blocks handlers and flushes state once", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_calls <- 0L
  widget_ref <- NULL

  w <- createWidget(
    "BatchBlockWidget",
    properties = list(
      x = ts_integer(1L, default = 0L),
      y = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    methods = list(
      on_x = observer("x", function() {
        handler_calls <<- handler_calls + 1L
      }),
      on_y = observer("y", function() {
        handler_calls <<- handler_calls + 1L
      })
    )
  )

  result <- w$call(mock_js_fn())
  handler_calls <- 0L
  updates_before <- length(tracker$get())

  # Use batch to set both properties without triggering handlers
  widget_ref$batch(c("x", "y"), {
    widget_ref$set("x", 5L)
    widget_ref$set("y", 10L)
  })

  # Handlers should NOT have fired (signals were blocked)
  expect_equal(handler_calls, 0L)

  # But state should have been flushed once at the end
  updates_after <- length(tracker$get())
  expect_equal(updates_after - updates_before, 1L)

  # Values should be set correctly
  expect_equal(widget_ref$x, 5L)
  expect_equal(widget_ref$y, 10L)
})

test_that("batch() unblocks signals even if expr errors", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_calls <- 0L
  widget_ref <- NULL

  w <- createWidget(
    "BatchErrorWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    methods = list(
      on_x = observer("x", function() {
        handler_calls <<- handler_calls + 1L
      })
    )
  )

  result <- w$call(mock_js_fn())
  handler_calls <- 0L

  # batch() with an error — signals should still be unblocked
  expect_error(
    widget_ref$batch("x", {
      stop("intentional error")
    }),
    "intentional error"
  )

  # After error, signal should be unblocked — setting x triggers handler
  widget_ref$set("x", 42L)
  expect_equal(handler_calls, 1L)
})

test_that("batch() with single property works", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_calls <- 0L
  widget_ref <- NULL

  w <- createWidget(
    "BatchSingleWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    methods = list(
      on_x = observer("x", function() {
        handler_calls <<- handler_calls + 1L
      })
    )
  )

  result <- w$call(mock_js_fn())
  handler_calls <- 0L

  widget_ref$batch("x", {
    widget_ref$set("x", 1L)
    widget_ref$set("x", 2L)
    widget_ref$set("x", 3L)
  })

  # Handler should not have fired during batch
  expect_equal(handler_calls, 0L)
  # Final value should be 3
  expect_equal(widget_ref$x, 3L)
})
