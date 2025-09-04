# Demo app

1. Installing R and the the `RserveTS` package

```r
devtools::install_github("tmelliott/RserveTS")
```

2. Install node and npm (or pnpm!), then install dependencies

```bash
pnpm install
```

3. Build dependencies

```bash
pnpm build
```

4. Start Rserve in a separate terminal

```bash
pnpm rserve
```

5. Run the app

```bash
pnpm start
```

You should see some output in the terminal:

```bash
Running sampler script...

Mean: 3
First char: hello
Sample num: Float64Array(2) [ 4, 3, r_type: 'double_array' ]
```
