#' Compile R functions
#'
#' Generates TypeScript schema for the given R function or file path. If a path, the R app is also generated.
#'
#' @param f A function or file path (length-one character string for file compilation).
#' @param ... Additional arguments. For the **file path** method, named arguments passed to [ts_deploy()] (e.g. `init`, `port`, `run`). For `ts_function` / `ts_widget` objects, further arguments are accepted and ignored by the current methods.
#' @return Character vector of TypeScript schema, or NULL if writing to file
#' @details
#' **`ts_function` method:** `name` defaults to `deparse(substitute(f))` and sets the generated `export const` symbol.
#'
#' **Character (file) method:** `filename` is the base path for output (default `[path of f].rserve`); `.R` and `.ts` extensions are appended. Arguments `filename`, `format`, and `prettier_cmd` must be passed by name; they are not part of `...`.
#'
#' * `format` — If `TRUE`, format the generated `.ts` with Prettier before writing (default `FALSE`). Requires [Prettier](https://prettier.io) or `npx prettier` on `PATH` when `TRUE`, unless `prettier_cmd` or option/env overrides are set. See README.
#' * `prettier_cmd` — Optional character vector argv (executable first). If `NULL`, uses option `RserveTS.prettier_cmd`, then environment variable `RserveTS_PRETTIER_CMD` (space-separated tokens), then `prettier` or `npx prettier` on `PATH`. A temporary `.ts` copy of the generated source is appended as the last argument (as with `prettier --parser typescript path/to/file.ts`). Another formatter (e.g. Biome) can be used if it accepts that invocation pattern.
#'
#' @md
#' @export
ts_compile <- function(f, ...) {
    UseMethod("ts_compile")
}

compile_fn <- function(f) {
    inputs <- f$args
    result <- f$result

    inputs <- sapply(inputs, \(x) x$input_type)
    fn_args <- paste(paste(inputs), collapse = ", ")
    # Use get_type dispatch so ts_function/ts_widget results compile correctly
    # (e.g., a method returning a widget connector produces nested Robj.ocap)
    ret_type <- get_type(result, "return")
    sprintf(
        "Robj.ocap([%s], %s)",
        fn_args,
        ret_type
    )
}

#' @export
ts_compile.ts_function <- function(f, ..., name = deparse(substitute(f))) {
    ocap_str <- compile_fn(f)

    sprintf(
        "export const %s = %s;", name, ocap_str
    )
}

#' @export
ts_compile.character <- function(
    f,
    ...,
    filename = sprintf("%s.rserve", tools::file_path_sans_ext(f)),
    format = FALSE,
    prettier_cmd = NULL) {
    if (length(f) > 1L) {
        for (path in f) {
            ts_compile.character(path, ..., format = format, prettier_cmd = prettier_cmd)
        }
        return(invisible(NULL))
    }

    if (!file.exists(f)) {
        warning(sprintf("File not found: %s", f))
        return()
    }
    # Track what exists before sourcing so we can find new definitions
    pre_globals <- ls(globalenv())
    e <- new.env(parent = globalenv())
    source(f, local = e)

    # Collect exports from both the local env and any new globals
    # (inner source() calls default to local=FALSE, putting defs in globalenv)
    new_globals <- setdiff(ls(globalenv()), pre_globals)
    candidates <- unique(c(ls(e), new_globals))
    # Look up each candidate from e first, then globalenv
    lookup <- function(name) {
        if (exists(name, envir = e, inherits = FALSE)) e[[name]]
        else if (exists(name, envir = globalenv(), inherits = FALSE)) get(name, envir = globalenv())
        else NULL
    }
    is_exported <- vapply(candidates, \(z) {
        obj <- lookup(z)
        inherits(obj, "ts_function") && isTRUE(obj$export)
    }, logical(1))
    exports <- candidates[is_exported]

    # Separate top-level app exports from child-only widget definitions.
    # A widget is "child-only" if its definition object is used as a child
    # property of another exported widget, or as the return type of a method.
    child_defs <- list()
    for (z in exports) {
        obj <- lookup(z)
        if (inherits(obj, "ts_widget")) {
            wp <- attr(obj, ".__props")$widgets
            child_defs <- c(child_defs, unname(wp))
            # Also check method return types for widget references
            method_defs <- attr(obj, ".__methods")$exported_defs
            for (m in method_defs) {
                if (inherits(m$result, "ts_widget")) {
                    child_defs <- c(child_defs, list(m$result))
                }
            }
        }
    }
    is_child_only <- vapply(exports, \(z) {
        obj <- lookup(z)
        any(vapply(child_defs, identical, logical(1), obj))
    }, logical(1))
    app_exports <- exports[!is_child_only]
    widget_exports <- exports[is_child_only]

    capitalize_first <- function(x) {
        paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    }

    safe_js_id <- function(nm) {
        is.character(nm) && length(nm) == 1L &&
            grepl("^[A-Za-z_][A-Za-z0-9_]*$", nm)
    }

    recursive_widget_children <- function(w) {
        if (!inherits(w, "ts_widget")) {
            return(list())
        }
        out <- list()
        wp <- attr(w, ".__props")$widgets
        if (length(wp)) {
            for (v in unname(wp)) {
                out <- c(out, list(v), recursive_widget_children(v))
            }
        }
        out
    }

    dedupe_widgets_first <- function(lst) {
        out <- list()
        for (x in lst) {
            if (!inherits(x, "ts_widget")) {
                next
            }
            if (any(vapply(out, identical, logical(1), x))) {
                next
            }
            out <- c(out, list(x))
        }
        out
    }

    collect_hoist_widgets <- function() {
        out <- list()
        for (z in exports) {
            obj <- lookup(z)
            if (inherits(obj, "ts_widget")) {
                out <- c(out, recursive_widget_children(obj))
                mdefs <- attr(obj, ".__methods")$exported_defs
                for (m in mdefs) {
                    if (inherits(m$result, "ts_widget")) {
                        r <- m$result
                        out <- c(out, list(r), recursive_widget_children(r))
                    }
                }
            }
        }
        dedupe_widgets_first(out)
    }

    topo_emit_order <- function(unique_widgets) {
        out <- list()
        walk <- function(w) {
            if (!inherits(w, "ts_widget")) {
                return()
            }
            if (any(vapply(out, identical, logical(1), w))) {
                return()
            }
            wp <- attr(w, ".__props")$widgets
            for (v in unname(wp)) {
                walk(v)
            }
            out <<- c(out, list(w))
        }
        for (w in unique_widgets) {
            walk(w)
        }
        out
    }

    resolve_hoist_id <- function(widget, taken) {
        for (nm in candidates) {
            obj <- lookup(nm)
            if (is.null(obj) || !identical(obj, widget)) {
                next
            }
            if (!safe_js_id(nm)) {
                next
            }
            if (nm %in% taken) {
                next
            }
            return(nm)
        }
        i <- 1L
        repeat {
            nm <- if (i == 1L) "hoistedChild" else sprintf("hoistedChild%d", i)
            if (!nm %in% taken) {
                return(nm)
            }
            i <- i + 1L
        }
    }

    hoist_widgets <- collect_hoist_widgets()
    hoist_topo <- topo_emit_order(hoist_widgets)

    clear_widget_schema_refs <- function() {
        for (w in hoist_topo) {
            attr(w, ".__ts_schema_ref") <- NULL
        }
        for (nm in exports) {
            obj <- lookup(nm)
            if (inherits(obj, "ts_widget")) {
                attr(obj, ".__ts_schema_ref") <- NULL
            }
        }
    }
    on.exit(clear_widget_schema_refs(), add = TRUE)

    taken <- exports
    hoist_const_lines <- character()
    hoist_type_lines <- character()

    for (w in hoist_topo) {
        ex_name <- NULL
        for (nm in exports) {
            if (identical(lookup(nm), w)) {
                ex_name <- nm
                break
            }
        }
        if (!is.null(ex_name)) {
            next
        }
        id <- resolve_hoist_id(w, taken)
        taken <- c(taken, id)
        attr(w, ".__ts_schema_ref") <- NULL
        hoist_const_lines <- c(
            hoist_const_lines,
            ts_compile(w, filename = "", name = id)
        )
        attr(w, ".__ts_schema_ref") <- id
        hoist_type_lines <- c(
            hoist_type_lines,
            sprintf(
                "export type T%s = z.infer<typeof %s>;",
                capitalize_first(id),
                id
            )
        )
    }

    exp_ws <- exports[vapply(exports, \(z) inherits(lookup(z), "ts_widget"), logical(1))]
    exp_ws_ord <- if (length(exp_ws)) {
        ord <- topo_emit_order(lapply(exp_ws, lookup))
        mapped <- vapply(ord, \(w) {
            hit <- exp_ws[vapply(exp_ws, \(nm) identical(lookup(nm), w), logical(1))]
            if (length(hit) == 1L) {
                return(hit[[1L]])
            }
            if (length(hit) > 1L) {
                stop("internal: ambiguous export name for widget")
            }
            NA_character_
        }, character(1))
        mapped <- mapped[!is.na(mapped)]
        unique(mapped, fromLast = FALSE)
    } else {
        character()
    }
    exports_emit_order <- c(exp_ws_ord, exports[!exports %in% exp_ws_ord])

    exportFns <- vapply(
        exports_emit_order,
        \(z) {
            obj <- lookup(z)
            if (inherits(obj, "ts_widget")) {
                attr(obj, ".__ts_schema_ref") <- NULL
                line <- ts_compile(obj, filename = "", name = z)
                attr(obj, ".__ts_schema_ref") <- z
                line
            } else {
                ts_compile(obj, filename = "", name = z)
            }
        },
        character(1)
    )

    type_aliases <- vapply(
        exports_emit_order,
        \(z) sprintf("export type T%s = z.infer<typeof %s>;", capitalize_first(z), z),
        character(1)
    )

    base_name <- tools::file_path_sans_ext(basename(f))
    schema_id <- gsub("[^a-zA-Z0-9_]", "_", base_name)
    if (schema_id == "" || grepl("^[0-9]", schema_id)) {
        schema_id <- "app"
    }
    cap_base <- paste0(toupper(substr(schema_id, 1, 1)), substr(schema_id, 2, nchar(schema_id)))
    schema_const <- sprintf("%sAppSchema", schema_id)
    type_app <- sprintf("T%sApp", cap_base)
    app_schema_block <- c(
        sprintf(
            "export const %s = {\n  %s\n} satisfies z.ZodRawShape;",
            schema_const,
            paste(app_exports, collapse = ",\n  ")
        ),
        "",
        sprintf(
            "export type %s = z.infer<z.ZodObject<typeof %s, \"strip\">>;",
            type_app,
            schema_const
        ),
        "",
        sprintf("export default %s;", schema_const)
    )

    src <- c(
        "// This file is auto-generated by ts_compile() - do not edit manually.",
        sprintf("// Source: %s", f),
        "",
        "import { Robj } from 'rserve-ts';",
        "import { z } from 'zod';",
        "\n",
        hoist_const_lines,
        exportFns,
        "\n",
        hoist_type_lines,
        type_aliases,
        "\n",
        app_schema_block
    )

    ts_out <- sprintf("%s.ts", filename)
    if (isTRUE(format)) {
        src <- format_ts_source(src, prettier_cmd = prettier_cmd)
    }
    cat(src, file = ts_out, sep = "\n")

    # R file
    ts_deploy(f, file = sprintf("%s.R", filename), silent = TRUE, ...)

    invisible()
}

#' @export
ts_compile.default <- function(f, ...) {
    warning("Not supported")
}
