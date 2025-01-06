# overload input/return types
sample_num <- ts_function(
    sample,
    x = ts_numeric(0),
    result = ts_numeric(1)
)

sampler <- ts_function(
    function() {
        list(
            sample_one = sample_num(0)
        )
    },
    result = ts_list(
        num = sample_num
    )
)

ts_compile(d_normal)

# compile to:
# const sampler = R.ocap(
#   [],
#   R.list({
#     num: R.ocap([], R.numeric(1))
#   })
# );
