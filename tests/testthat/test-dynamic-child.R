# --- Shared fixtures ---
mock_setState <- structure(list("fn"), class = "javascript_function")

# Widget definitions must be in globalenv so widgetMethods' eval()
# can find cross-references (same as real apps using source()).
DynChildWidget <- createWidget(
  "DynChildWidget",
  properties = list(
    value = ts_integer(1L, default = 0L),
    label = ts_character(1L, default = "")
  ),
  initialize = function(widget, parent = NULL) {
    if (!is.null(parent)) {
      widget$set("label", "child_of_parent")
    }
  },
  methods = list(
    increment = ts_function(
      function() {
        .self$value <- .self$value + 1L
      }
    )
  ),
  .env = globalenv()
)
assign("DynChildWidget", DynChildWidget, envir = globalenv())

DynParentWidget <- createWidget(
  "DynParentWidget",
  properties = list(
    name = ts_character(1L, default = "parent"),
    .children = "ANY"
  ),
  initialize = function(widget) {
    widget$.children <- list()
  },
  methods = list(
    addChild = ts_function(
      function() {
        child <- .self$create_dynamic_child(DynChildWidget)
        idx <- length(.self$.children) + 1L
        .self$.children[[idx]] <- child
        child$connector
      },
      result = DynChildWidget
    )
  ),
  .env = globalenv()
)

withr::defer({
  for (nm in c("DynChildWidget", "DynParentWidget")) {
    if (exists(nm, envir = globalenv())) rm(list = nm, envir = globalenv())
  }
}, envir = parent.frame())

# --- Tests ---

test_that("create_dynamic_child returns instance and connector", {
  local_mocked_bindings(
    jsfun = function(x) function(...) invisible(NULL)
  )

  result <- DynParentWidget$call(mock_setState)
  connector <- result$methods$addChild$call()

  expect_s3_class(connector, "ts_function")
})

test_that("dynamic child connector returns correct structure", {
  local_mocked_bindings(
    jsfun = function(x) function(...) invisible(NULL)
  )

  result <- DynParentWidget$call(mock_setState)
  connector <- result$methods$addChild$call()

  # Connect with NULL setState (no Rserve needed)
  child_result <- connector$call(NULL)

  # Should return properties, children, methods — same shape as any widget
  expect_true("properties" %in% names(child_result))
  expect_true("methods" %in% names(child_result))

  # Properties should include value and label
  expect_true("value" %in% names(child_result$properties))
  expect_true("label" %in% names(child_result$properties))

  # Methods should include increment
  expect_true("increment" %in% names(child_result$methods))
})

test_that("dynamic child gets defaults applied", {
  local_mocked_bindings(
    jsfun = function(x) function(...) invisible(NULL)
  )

  result <- DynParentWidget$call(mock_setState)
  connector <- result$methods$addChild$call()
  child_result <- connector$call(NULL)

  # Get default value — should be 0L
  val <- child_result$properties$value$get$call()
  expect_equal(val, 0L)
})

test_that("dynamic child receives parent and runs initialize", {
  local_mocked_bindings(
    jsfun = function(x) function(...) invisible(NULL)
  )

  result <- DynParentWidget$call(mock_setState)
  connector <- result$methods$addChild$call()
  child_result <- connector$call(NULL)

  # The child's initialize sets label = "child_of_parent" when parent != NULL
  lbl <- child_result$properties$label$get$call()
  expect_equal(lbl, "child_of_parent")
})

test_that("multiple dynamic children are independent", {
  local_mocked_bindings(
    jsfun = function(x) function(...) invisible(NULL)
  )

  result <- DynParentWidget$call(mock_setState)

  # Create and connect two children
  conn1 <- result$methods$addChild$call()
  conn2 <- result$methods$addChild$call()

  child1 <- conn1$call(NULL)
  child2 <- conn2$call(NULL)

  # Increment child1 only
  child1$methods$increment$call()

  val1 <- child1$properties$value$get$call()
  val2 <- child2$properties$value$get$call()

  expect_equal(val1, 1L)
  expect_equal(val2, 0L)
})

test_that("destroy nulls out setState", {
  # Test directly on the base ref class to avoid jsfun mock issues
  instance <- tsWidget$new()
  instance$setState <- function(...) invisible(NULL)

  expect_false(is.null(instance$setState))
  instance$destroy()
  expect_null(instance$setState)
})

test_that("compile_fn handles ts_function result type (widget connector)", {
  # A method that returns a widget constructor should produce nested Robj.ocap
  method_def <- ts_function(
    function() NULL,
    result = DynChildWidget
  )

  compiled <- ts_compile(method_def, name = "addChild")

  # Should contain nested Robj.ocap (outer method wrapping inner widget ctor)
  expect_match(compiled, "Robj\\.ocap\\(\\[\\], Robj\\.ocap")
})

test_that("register handles NULL fn without calling jsfun", {
  # Test directly on the base ref class
  instance <- tsWidget$new()
  instance$setState <- NULL  # initialize properly
  expect_null(instance$setState)

  # register with NULL should not call jsfun or error
  expect_no_error(instance$register(NULL))
  expect_null(instance$setState)
})
