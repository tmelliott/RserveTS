# Build a simple ReactJS app

We will create a simple [ReactJS](https://reactjs.org/) application that
implements our favourite *Old Faithful* dataset. Of course, this is not
a great use-case for Rserve as it would be more appropriate to use a
REST API, but it is a simple example to demonstrate how to use Rserve
with a front-end application.

## Install the RserveTS package

``` r
devtools::install_github('tmelliott/RserveTS')
```

## Write the R code

The code is saved in a file called `faithful-app.R`, and we can preview
the results by calling the functions:

``` r
cat(readLines('faithful-app.R'), sep = '\n')
#> library(RserveTS)
#> 
#> get_hist <- ts_function(
#>     function(bins = ts_integer(1)) {
#>         h <- hist(faithful$waiting, breaks = bins, plot = FALSE)
#>         data.frame(x = h$mids, y = h$density)
#>     },
#>     result = ts_dataframe(x = ts_numeric(0), y = ts_numeric(0))
#> )
#> get_smoother <- ts_function(
#>     function(bandwidth = ts_numeric(1)) {
#>         d <- density(faithful$waiting, bw = bandwidth)
#>         data.frame(x = d$x, y = d$y)
#>     },
#>     result = ts_dataframe(x = ts_numeric(0), y = ts_numeric(0))
#> )

source('faithful-app.R')
#> Loading required package: objectProperties
#> Loading required package: objectSignals

get_hist$call(10)
#>       x            y
#> 1  42.5 0.0029411765
#> 2  47.5 0.0161764706
#> 3  52.5 0.0242647059
#> 4  57.5 0.0176470588
#> 5  62.5 0.0102941176
#> 6  67.5 0.0073529412
#> 7  72.5 0.0198529412
#> 8  77.5 0.0397058824
#> 9  82.5 0.0404411765
#> 10 87.5 0.0169117647
#> 11 92.5 0.0036764706
#> 12 97.5 0.0007352941
```

That’s it! We’ll use
[`ts_compile()`](http://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
later to create the server code and Typescript schema for the app.

## Create the React app

I’m using [Vite](https://vite.dev/) to create the app, but you could use
any framework. Whatever you use, you’ll need to be able to bundle the
code (including libraries such as [zod](https://zod.dev)).

``` bash
pnpm create vite faithful-demo --template vanilla-ts
cd faithful-demo
pnpm install
pnpm run dev
```

You should now be able to see the default Vite app running at
`http://localhost:5173` (or similar, see the console output).

Now install the `rserve-ts` and `zod` packages:

``` bash
pnpm install rserve-ts zod
```

### Create the server code

We now use the
[`ts_compile()`](http://tomelliott.co.nz/RserveTS/reference/ts_compile.md)
function to create two files:

- `faithful-app.rserve.R` is the file that will start the Rserve
  instance with your apps functions available.
- `faithful-app.rserve.ts` contains the TypeScript schema (using
  [zod](https://zod.dev)) that will let you use the R functions directly
  in the app like any other typescript function!

We’ll send these straight to the `faithful-demo/src` directory.

``` r
ts_compile('faithful-app.R', filename = 'faithful-demo/src/faithful-app.rserve')
```

### Write the app

The rest of the process simply requires writing TypeScript code. I won’t
go into detail since that’s not the focus of this vignette, but below
you can see the code written with some basic comments. Copy and paste
these to get the app running.

``` typescript
// main.ts
import "./style.css";

import RserveClient from "rserve-ts";
import faithfulApp from "./faithful-app.rserve";
import { z } from "zod";

document.querySelector<HTMLDivElement>("#app")!.innerHTML = `
  <div>
    <h1>Rserve and TypeScript</h1>
    <div class="card">
      Number of bins:
      <input type="number" id="n" value="10" size="5" />
      <button id="counter" type="button">Make histogram</button>
    </div>
    <div id="hist" style="display: flex; align-items: flex-end; gap: 2px;"></div>
  </div>
`;

type FaithfulApp = z.infer<z.ZodObject<typeof faithfulApp>>;

let getHist: FaithfulApp["get_hist"] | undefined = undefined;

async function connectToRserve() {
  const client = await RserveClient.create({ host: "ws://localhost:6311" });
  const app = await client.ocap(faithfulApp);
  console.log("Connected to Rserve: ", app);
  getHist = app.get_hist;
}
connectToRserve();

document
  .querySelector<HTMLButtonElement>("#counter")!
  .addEventListener("click", async () => {
    if (!getHist) return;
    const Nbin = parseInt(
      document.querySelector<HTMLInputElement>("#n")!.value
    );
    const hist = await getHist(Nbin);
    const maxY = Math.max(...hist.y);

    const histDiv = document.querySelector<HTMLDivElement>("#hist")!;
    histDiv.innerHTML = `
      ${Array.from(hist.y)
        .map(
          (yi) =>
            `<div style="height: ${
              (yi / maxY) * 180
            }px; flex: 1; background: pink;"></div>`
        )
        .join("")}
    `;
  });
```

``` css
// style.css
:root {
  font-family: Inter, system-ui, Avenir, Helvetica, Arial, sans-serif;
  line-height: 1.5;
  font-weight: 400;

  color-scheme: light dark;
  color: rgba(255, 255, 255, 0.87);
  background-color: #242424;

  font-synthesis: none;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

a {
  font-weight: 500;
  color: #646cff;
  text-decoration: inherit;
}
a:hover {
  color: #535bf2;
}

body {
  margin: 0;
  display: flex;
  place-items: center;
  min-width: 320px;
  min-height: 100vh;
}

h1 {
  font-size: 3.2em;
  line-height: 1.1;
}

#app {
  max-width: 1280px;
  margin: 0 auto;
  padding: 2rem;
  text-align: center;
}

.logo {
  height: 6em;
  padding: 1.5em;
  will-change: filter;
  transition: filter 300ms;
}
.logo:hover {
  filter: drop-shadow(0 0 2em #646cffaa);
}
.logo.vanilla:hover {
  filter: drop-shadow(0 0 2em #3178c6aa);
}

.card {
  padding: 2em;
}

.read-the-docs {
  color: #888;
}

button {
  border-radius: 8px;
  border: 1px solid transparent;
  padding: 0.6em 1.2em;
  font-size: 1em;
  font-weight: 500;
  font-family: inherit;
  background-color: #1a1a1a;
  cursor: pointer;
  transition: border-color 0.25s;
}
button:hover {
  border-color: #646cff;
}
button:focus,
button:focus-visible {
  outline: 4px auto -webkit-focus-ring-color;
}

input {
  border-radius: 8px;
  border: 1px solid transparent;
  padding: 0.6em 1.2em;
  font-size: 1em;
  font-weight: 500;
  font-family: inherit;
  background-color: #1a1a1a;
  color: #fff;
  outline: none;
}
input:hover {
  border-color: #646cff;
}
input:focus,
input:focus-visible {
  outline: 4px auto -webkit-focus-ring-color;
}

@media (prefers-color-scheme: light) {
  :root {
    color: #213547;
    background-color: #ffffff;
  }
  a:hover {
    color: #747bff;
  }
  button {
    background-color: #f9f9f9;
  }
  input {
    background-color: #f9f9f9;
    color: #000;
  }
}
```

## Run the app

To run the app, start the Rserve server:

``` bash
Rscript src/faithful-app.rserve.R
```

Then start the Vite server:

``` bash
pnpm run dev
```

You should now be able to see the app running at `http://localhost:5173`
(or similar, see the console output).
