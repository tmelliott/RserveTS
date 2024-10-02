# overload input/return types
sample_num <- ts_function(
    sample,
    x = ts_numeric(0),
    result = ts_numeric(1)
)
ts_compile(sample_num)

# compile to:
# const out = {
#   sample_one: R.ocap([R.as_vector(z.number())], R.numeric(1)),
# };
