import { zodResponseFormat } from "openai/helpers/zod.mjs"
import zodToJsonSchema from "zod-to-json-schema"
import { z, ZodType } from "zod"

function check<ZodInput extends ZodType>(schema: ZodInput) {
  console.log(JSON.stringify(zodResponseFormat(schema, "schema"), undefined, 4))
}

// check(z.object({ name: z.string(), age: z.bigint() }))
// check(z.enum(["a", "b", "c"]))
// check(z.union([z.number(), z.string()]))
// check(z.union([z.object({ x: z.number() }), z.object({ y: z.string() })]))
// check(z.intersection(z.object({ x: z.number() }), z.object({ y: z.string() })))
// check(z.literal("hello"))
// check(z.tuple([z.string(), z.string(), z.string()]))
// check(z.boolean())
// check(z.object({ arg: z.array(z.string()) }))
// check(z.object({ arg: z.nullable(z.string()) }))
// check(z.object({ arg: z.tuple([]) }))
check(z.object({ arg: z.literal("hello") }))

// console.log(zodToJsonSchema(z.object({ name: z.string(), age: z.number().int() }), { target: "openApi3" }))
// console.log(zodToJsonSchema(z.object({ name: z.string(), age: z.number().int() }), { target: "openAi" }))

