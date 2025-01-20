import { Robj } from "rserve-ts";
import { z } from "zod";

const fn_first = Robj.ocap(
  [z.union([z.string(), Robj.character(0)])],
  Robj.character(1)
);

const fn_mean = Robj.ocap(
  [z.union([z.number(), z.instanceof(Float64Array)])],
  Robj.numeric(1)
);

const sample_num = Robj.ocap([z.instanceof(Float64Array)], Robj.numeric(1));
