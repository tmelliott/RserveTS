test_that("Union types", {
    number_or_null <- ts_union(ts_numeric(1), ts_null())
    expect_equal(number_or_null$input_type, "z.union([z.number(), z.null()])")

    expect_equal(number_or_null$check(5), 5)
    expect_equal(number_or_null$check(NULL), NULL)

    expect_error(number_or_null$check("not a number"))
})

test_that("Array types", {
    person <- ts_list(name = ts_character(), age = ts_integer())
    person_array <- ts_array(person)

    john <- list(name = "John", age = 30L)
    anne <- list(name = "Anne", age = 15L)

    expect_equal(person$check(john), john)
    expect_equal(person_array$check(list(john, anne)), list(john, anne))

    expect_error(person_array$check(john))
})

test_that("Optional arguments", {
    optional <- ts_optional(ts_numeric())
    expect_equal(optional$check(5), 5)
    expect_equal(optional$check(1:5), 1:5)

    expect_equal(optional$check(), NULL)
    expect_equal(optional$check(NULL), NULL)

    expect_error(optional$check("hello"))
})


# thing <- function(f) {
#     e <- new.env()
#     e$f <- f
#     e$call <- function(...) {
#         mc <- match.call(e$f)
#         print()
#         print(mc)
#         mc
#     }
#     e
# }

# t <- thing(function(x = TRUE) x)


test_that("Recursive types", {
    rl <- ts_recursive_list(
        list(
            name = ts_character(1)
        ),
        list(
            children = ts_self()
        )
    )

    expect_error(
        rl$check(list(name = "John", children = list(
            list(nam = "Maria")
        )))
    )
})
