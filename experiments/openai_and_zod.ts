import { zodResponseFormat } from "openai/helpers/zod.mjs"
import { z, ZodType } from "zod"

function check<ZodInput extends ZodType>(schema: ZodInput) {
  console.log(JSON.stringify(zodResponseFormat(schema, "schema"), undefined, 4))
}

// OK
// check(z.enum(["a", "b", "c"]))

// OK
// check(z.union([z.number(), z.string()]))

// OK
// check(z.union([z.object({ x: z.number() }), z.object({ y: z.string() })]))

// OK
// check(z.intersection(z.object({ x: z.number() }), z.object({ y: z.string() })))

// OK
// check(z.literal("hello"))

// OK
// check(z.tuple([z.string(), z.string(), z.string()]))
