hist_type <- ts_object(
    input_type = "z.object({
       breaks: z.array(z.number()),
       counts: z.array(z.number()),
       density: z.array(z.number()),
       mids: z.array(z.number()),
       xname: z.string(),
       equidist: z.boolean(),
     })",
    return_type = "Robj.list({
         breaks: Robj.numeric(),
         counts: Robj.numeric(),
         density: Robj.numeric(),
         mids: Robj.numeric(),
         xname: Robj.character(1),
         equidist: Robj.logical(1),
     })",
    check = function(x) {
        stopifnot(is.list(x))
        stopifnot(all(c("breaks", "counts", "density", "mids", "xname", "equidist") %in% names(x)))
        stopifnot(is.numeric(x$breaks))
        stopifnot(is.numeric(x$counts))
        stopifnot(is.numeric(x$density))
        stopifnot(is.numeric(x$mids))
        stopifnot(is.character(x$xname))
        stopifnot(is.logical(x$equidist))
        stopifnot(isTRUE(all.equal(length(x$breaks), length(x$counts), length(x$density), length(x$mids))))
        x
    }
)
