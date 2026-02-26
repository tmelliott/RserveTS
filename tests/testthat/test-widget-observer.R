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

test_that("observer() creates ts_observer structure", {
  obs <- observer(c("x", "y"), function() NULL)
  expect_s3_class(obs, "ts_observer")
  expect_equal(obs$props, c("x", "y"))
  expect_true(is.function(obs$fn))
})

test_that("observer() works with ts_function", {
  obs <- observer("x", ts_function(function() 42, result = ts_numeric(1L)))
  expect_s3_class(obs, "ts_observer")
  expect_equal(obs$props, "x")
  expect_s3_class(obs$fn, "ts_function")
})

test_that("observer(props, function) wires prop handlers", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_called <- FALSE

  w <- createWidget(
    "ObsPlainWidget",
    properties = list(
      input = ts_integer(1L, default = 0L),
      output = ts_integer(1L, default = 0L)
    ),
    methods = list(
      on_input = observer("input", function() {
        handler_called <<- TRUE
        .self$set("output", .self$input * 2L)
      })
    )
  )

  result <- w$call(mock_js_fn())

  # Handler shouldn't have fired yet (input hasn't changed since init)
  expect_false(handler_called)

  # Method should be on the ref class
  rc <- attr(w, ".__refclass")
  expect_true("on_input" %in% names(rc$def@refMethods))

  # Observer metadata should be stored
  method_meta <- attr(w, ".__methods")
  expect_equal(method_meta$observers$on_input, "input")

  # Should NOT be exported (plain function, not ts_function)
  expect_false("on_input" %in% method_meta$exported)
})

test_that("observer(props, ts_function) wires handlers and marks exported", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "ObsTsFnWidget",
    properties = list(
      type = ts_character(1L, default = "normal"),
      result = ts_numeric(0L, default = numeric(0))
    ),
    methods = list(
      generate = observer("type", ts_function(
        function() {
          .self$set("result", rnorm(5))
        },
        result = ts_void()
      ))
    )
  )

  result <- w$call(mock_js_fn())

  method_meta <- attr(w, ".__methods")
  expect_equal(method_meta$observers$generate, "type")
  expect_true("generate" %in% method_meta$exported)
})

test_that("ts_function without observer is exported but not reactive", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "TsFnOnlyWidget",
    properties = list(
      value = ts_integer(1L, default = 0L)
    ),
    methods = list(
      reset = ts_function(function() {
        .self$set("value", 0L)
      })
    )
  )

  result <- w$call(mock_js_fn())

  method_meta <- attr(w, ".__methods")
  expect_true("reset" %in% method_meta$exported)
  expect_null(method_meta$observers$reset)
})

test_that("plain function is internal and not reactive", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "PlainFnWidget",
    properties = list(
      value = ts_integer(1L, default = 0L)
    ),
    methods = list(
      helper = function() {
        .self$value + 1L
      }
    )
  )

  result <- w$call(mock_js_fn())

  method_meta <- attr(w, ".__methods")
  expect_false("helper" %in% method_meta$exported)
  expect_null(method_meta$observers$helper)
})

test_that("observer with multiple props wires all handlers", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "MultiObsWidget",
    properties = list(
      a = ts_integer(1L, default = 0L),
      b = ts_integer(1L, default = 0L),
      total = ts_integer(1L, default = 0L)
    ),
    methods = list(
      compute = observer(c("a", "b"), function() {
        .self$set("total", .self$a + .self$b)
      })
    )
  )

  result <- w$call(mock_js_fn())

  method_meta <- attr(w, ".__methods")
  expect_equal(method_meta$observers$compute, c("a", "b"))
})

test_that("observer methods get auto-flush wrapping", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "ObsAutoFlushWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    methods = list(
      on_x = observer("x", function() {
        .self$set("x", .self$x + 1L)
      })
    )
  )

  result <- w$call(mock_js_fn())

  # Method body should have depth-counting wrapper
  rc <- attr(w, ".__refclass")
  method_body <- paste(deparse(body(rc$def@refMethods$on_x)), collapse = "\n")
  expect_true(grepl(".call_depth", method_body, fixed = TRUE))
})

test_that("all four method types coexist in one widget", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "AllTypesWidget",
    properties = list(
      x = ts_integer(1L, default = 0L),
      y = ts_integer(1L, default = 0L)
    ),
    methods = list(
      # internal reactive
      on_x = observer("x", function() {
        .self$set("y", .self$x * 2L)
      }),
      # exported reactive
      on_y = observer("y", ts_function(
        function() { .self$y },
        result = ts_integer(1L)
      )),
      # exported non-reactive
      reset = ts_function(function() {
        .self$set("x", 0L)
      }),
      # internal non-reactive
      helper = function() {
        .self$x + .self$y
      }
    )
  )

  result <- w$call(mock_js_fn())

  method_meta <- attr(w, ".__methods")

  # Check exported
  expect_true("on_y" %in% method_meta$exported)
  expect_true("reset" %in% method_meta$exported)
  expect_false("on_x" %in% method_meta$exported)
  expect_false("helper" %in% method_meta$exported)

  # Check observers
  expect_equal(method_meta$observers$on_x, "x")
  expect_equal(method_meta$observers$on_y, "y")
  expect_null(method_meta$observers$reset)
  expect_null(method_meta$observers$helper)
})

test_that("backward compat: initialize with addPropHandler still works", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  handler_ran <- FALSE

  w <- createWidget(
    "BackwardCompatWidget",
    properties = list(
      input = ts_character(1L, default = ""),
      output = ts_character(1L, default = "")
    ),
    initialize = function(widget) {
      widget$addPropHandler("input", function() {
        widget$set("output", paste0("got: ", widget$get("input")))
        handler_ran <<- TRUE
      })
    },
    methods = list()
  )

  result <- w$call(mock_js_fn())
  expect_false(handler_ran)
})
