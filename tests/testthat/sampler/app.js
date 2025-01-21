var R = require("./node_modules/rserve-ts/dist/index.js").default;
global.WebSocket = require("ws");

async function main() {
  const con = await R.create({
    host: "http://127.0.0.1:6311",
  });

  const app = await con.ocap();

  console.log(app);
  console.log(app.fn_mean);

  app.fn_mean(new Float64Array([1, 2, 3, 4, 5]), (err, res) => {
    if (err) {
      console.error(err);
      process.exit(1);
    }
    console.log("Mean:", res);
    process.exit(0);
  });
  // console.log("Mean:", m);
}
console.log("Running sampler script...\n");
main();
