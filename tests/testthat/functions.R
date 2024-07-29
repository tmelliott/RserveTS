# overload input/return types
sample_one <- ts_function(
    sample,
    ts_overload(
        x = ts_numeric(),
        result = ts_numeric(1)
    ),
    ts_overload(
        x = ts_character(),
        result = ts_character(1)
    )
)
ts_compile(sample_one)

# compile to:
# const sample_one = (x: number) => Promise<number>;
# const sample_one = (x: string) => Promise<string>;
