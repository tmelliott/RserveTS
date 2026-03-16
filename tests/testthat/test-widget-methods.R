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

test_that("exported methods are included in widget return", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "MethodExportWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    methods = list(
      compute = ts_function(function() {
        .self$x * 2L
      }, result = ts_integer(1L)),
      internal_update = observer("x", function() {
        # internal — not exported
      })
    )
  )

  result <- w$call(mock_js_fn())

  expect_true("methods" %in% names(result))
  expect_true("compute" %in% names(result$methods))
  # Internal methods should NOT be exported
  expect_false("internal_update" %in% names(result$methods))
  # Method should be a ts_function
  expect_true(inherits(result$methods$compute, "ts_function"))
})

test_that("exported method ocap calls widget method", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  widget_ref <- NULL

  w <- createWidget(
    "MethodCallWidget",
    properties = list(
      x = ts_integer(1L, default = 5L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    methods = list(
      double_x = ts_function(function() {
        .self$x * 2L
      }, result = ts_integer(1L))
    )
  )

  result <- w$call(mock_js_fn())

  # Call the exported method via its ocap
  ret <- result$methods$double_x$call()
  expect_equal(ret, 10L)
})

test_that("exported method with args works", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  widget_ref <- NULL

  w <- createWidget(
    "MethodArgsWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    methods = list(
      add = ts_function(function(n) {
        .self$x <- .self$x + n
      }, n = ts_integer(1L))
    )
  )

  result <- w$call(mock_js_fn())

  result$methods$add$call(7L)
  expect_equal(widget_ref$x, 7L)
})

test_that("observer-wrapped exported method is included", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "ObsExportWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    methods = list(
      compute = observer("x", ts_function(function() {
        .self$x * 3L
      }, result = ts_integer(1L)))
    )
  )

  result <- w$call(mock_js_fn())

  # observer-wrapped ts_function should still be exported
  expect_true("compute" %in% names(result$methods))
  expect_true(inherits(result$methods$compute, "ts_function"))
})

test_that("widget with no exported methods has empty methods list", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "NoMethodsWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    methods = list(
      internal = observer("x", function() {
        # internal only
      })
    )
  )

  result <- w$call(mock_js_fn())

  expect_true("methods" %in% names(result))
  expect_equal(length(result$methods), 0L)
})

test_that("exported_defs are stored in widget attributes", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "AttrDefsWidget",
    properties = list(
      x = ts_integer(1L, default = 0L)
    ),
    methods = list(
      compute = ts_function(function() {
        .self$x * 2L
      }, result = ts_integer(1L)),
      reset = ts_function(function() {
        .self$x <- 0L
      })
    )
  )

  method_meta <- attr(w, ".__methods")
  expect_true("exported_defs" %in% names(method_meta))
  expect_equal(sort(names(method_meta$exported_defs)), c("compute", "reset"))
  expect_true(inherits(method_meta$exported_defs$compute, "ts_function"))
})

test_that("createWidget stores normalized action metadata", {
  w <- createWidget(
    "ActionMetaWidget",
    properties = list(x = ts_integer(1L, default = 0L)),
    actions = list(
      enabled = TRUE,
      types = c("SetX", "ResetX"),
      strict = "warn"
    )
  )

  capabilities <- attr(w, ".__capabilities")
  expect_true(is.list(capabilities))
  expect_true(is.list(capabilities$actions))
  expect_true(isTRUE(capabilities$actions$enabled))
  expect_equal(capabilities$actions$types, c("SetX", "ResetX"))
  expect_equal(capabilities$actions$strict, "warn")
})

test_that("root widget connector exposes capabilities.actions payload", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  w <- createWidget(
    "ActionRootCapsWidget",
    properties = list(x = ts_integer(1L, default = 0L)),
    actions = list(
      enabled = TRUE,
      types = c("SetX"),
      strict = "warn"
    )
  )

  result <- w$call(mock_js_fn())
  expect_true("capabilities" %in% names(result))
  expect_true("actions" %in% names(result$capabilities))
  expect_true(isTRUE(result$capabilities$actions$enabled))
  expect_equal(result$capabilities$actions$types, "SetX")
  expect_equal(result$capabilities$actions$strict, "warn")
})

test_that("widgetActions creates typed dispatchAction and metadata", {
  tracker <- capture_state_updates()
  local_mocked_bindings(jsfun = tracker$mock)

  widget_ref <- NULL
  w <- createWidget(
    "ActionFactoryWidget",
    properties = list(x = ts_integer(1L, default = 0L)),
    initialize = function(widget) {
      widget_ref <<- widget
    },
    actions = widgetActions(
      SetX = ts_function(function(payload = ts_list(x = ts_integer(1L))) {
        .self$x <- payload$x
      }),
      strict = "warn"
    )
  )

  result <- w$call(mock_js_fn())
  expect_true("dispatchAction" %in% names(result$methods))
  expect_equal(result$capabilities$actions$types, "SetX")
  expect_equal(result$capabilities$actions$strict, "warn")

  result$methods$dispatchAction$call(list(
    type = "SetX",
    payload = list(x = 5L)
  ))
  expect_equal(widget_ref$x, 5L)
})

test_that("widgetActions validates action definitions", {
  expect_error(
    widgetActions(ts_function(function(payload = ts_list(x = ts_integer(1L))) {
      invisible(NULL)
    })),
    "must be named"
  )

  expect_error(
    widgetActions(
      SetX = function() {},
      strict = "warn"
    ),
    "must be a ts_function"
  )

  expect_error(
    widgetActions(
      SetX = ts_function(function(a = ts_integer(1L), b = ts_integer(1L)) {
        invisible(NULL)
      }),
      strict = "warn"
    ),
    "exactly one payload argument"
  )

  expect_error(
    widgetActions(
      "Set-X" = ts_function(function(payload = ts_list(x = ts_integer(1L))) {
        invisible(NULL)
      }),
      strict = "warn"
    ),
    "must match"
  )
})
