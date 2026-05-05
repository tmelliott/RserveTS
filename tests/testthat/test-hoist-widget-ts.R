# Hoisted widget TS codegen (RserveTS) — TDD specification
#
# --- Human checkpoint (before implementing hoist) ---
#
# Minimal R app (see compile_hoist_mre() below):
#   - HoistMreChild: one integer property `n`, export = FALSE (default)
#   - HoistMreParent: property `title` + child widget `child = HoistMreChild`,
#     export = TRUE
#
# CURRENT compiler output (baseline, pre-hoist): a single
#   `export const HoistMreParent = Robj.ocap(...)` with the full child ctor
#   inlined under `children: Robj.list({ child: Robj.ocap([...], ...) })`.
#
# DESIRED output after hoisting (confirm this matches product intent):
#   1) Two widget exports, child BEFORE parent:
#        export const HoistMreChild = Robj.ocap(...);
#        export const HoistMreParent = Robj.ocap(...);
#   2) Parent line references the child binding, not an inlined child ocap:
#        ... children: Robj.list({ child: HoistMreChild }) ...
#   3) Type alias for the hoisted child:
#        export type THoistMreChild = z.infer<typeof HoistMreChild>;
#   4) App schema block still lists only explicitly exported widgets
#      (HoistMreParent only), not the hoisted child — same as today.
#
# If any of the above should differ, adjust this file and the plan before coding.
# ------------------------------------------------------------------------------

compile_hoist_mre <- function() {
    dir <- tempfile()
    dir.create(dir)
    src <- file.path(dir, "HoistMre.R")
    base <- file.path(dir, "HoistMre.rserve")
    ts_out <- paste0(base, ".ts")
    writeLines(
        c(
            "HoistMreChild <- createWidget(",
            "  \"HoistMreChild\",",
            "  properties = list(n = ts_integer(1L, default = 0L))",
            ")",
            "HoistMreParent <- createWidget(",
            "  \"HoistMreParent\",",
            "  properties = list(",
            "    title = ts_character(1L, default = \"\"),",
            "    child = HoistMreChild",
            "  )",
            ")",
            "HoistMreParent$export <- TRUE"
        ),
        src
    )
    ts_compile(src, filename = base)
    list(ts_out = ts_out, dir = dir, base = base)
}

test_that("hoist MRE: two export const widget lines, child before parent", {
    o <- compile_hoist_mre()
    on.exit(unlink(o$dir, recursive = TRUE), add = TRUE)

    lines <- readLines(o$ts_out, warn = FALSE)
    i_child <- grep("^export const HoistMreChild\\s*=", lines)
    i_parent <- grep("^export const HoistMreParent\\s*=", lines)
    expect_equal(length(i_child), 1L)
    expect_equal(length(i_parent), 1L)
    if (length(i_child) == 1L && length(i_parent) == 1L) {
        expect_lt(i_child[[1L]], i_parent[[1L]])
    }
})

test_that("hoist MRE: parent children slot uses identifier, not inlined child Robj.ocap", {
    o <- compile_hoist_mre()
    on.exit(unlink(o$dir, recursive = TRUE), add = TRUE)

    lines <- readLines(o$ts_out, warn = FALSE)
    parent_line <- lines[grepl("^export const HoistMreParent", lines)]
    expect_length(parent_line, 1L)
    expect_match(parent_line, "children:\\s*Robj\\.list\\(\\{\\s*child:\\s*HoistMreChild\\s*\\}\\)")
    expect_false(grepl("child:\\s*Robj\\.ocap", parent_line))
})

test_that("hoist MRE: type alias for hoisted child", {
    o <- compile_hoist_mre()
    on.exit(unlink(o$dir, recursive = TRUE), add = TRUE)

    text <- paste(readLines(o$ts_out, warn = FALSE), collapse = "\n")
    expect_match(text, "export type THoistMreChild = z\\.infer<typeof HoistMreChild>")
})

test_that("hoist MRE: app schema lists only explicitly exported widget", {
    o <- compile_hoist_mre()
    on.exit(unlink(o$dir, recursive = TRUE), add = TRUE)

    text <- paste(readLines(o$ts_out, warn = FALSE), collapse = "\n")
    expect_match(
        text,
        "export const HoistMreAppSchema = \\{[[:space:]]*HoistMreParent[[:space:]]*\\}"
    )
    expect_false(grepl("HoistMreAppSchema[^}]*HoistMreChild", text))
})
