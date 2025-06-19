// var R = require("./node_modules/rserve-ts/dist/index.js").default;
// global.WebSocket = require("ws");

import RserveClient from "rserve-ts";
import WebSocket from "ws";

interface global {
  WebSocket: typeof WebSocket;
}

global.WebSocket = WebSocket as any;

import appFuns from "./app.rserve";

async function main() {
  const con = await RserveClient.create({
    host: "http://127.0.0.1:6311",
  });
  const app = await con.ocap(appFuns);

  const m = await app.fn_mean(new Float64Array([1, 2, 3, 4, 5]));
  console.log("Mean:", m);

  const f = await app.fn_first(["hello", "world"]);
  console.log("First char:", f);

  const s = await app.sample_num(new Float64Array([1, 2, 3, 4, 5]), 2);
  console.log("Sample num:", s);

  console.log("\n-----------\n- checking recursive scoping ...\n");
  const store = await app.save_for_later(10);
  const value = await store.get();
  console.log("Retrieved value: ", value);

  console.log("\n-----------\n- checking optional arguments ...\n");
  // const optNum = await app.optional_fn(5);
  // console.log("Passed arg: ", optNum);
  const optNull = await app.optional_fn(undefined);
  console.log("Passed arg: ", optNull);

  process.exit(0);
}
console.log("Running sampler script...\n");
main();
