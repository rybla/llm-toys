import { zodResponseFormat } from "openai/helpers/zod.mjs"
import { z, ZodType } from "zod"

function check<ZodInput extends ZodType>(schema: ZodInput) {
  console.log(JSON.stringify(zodResponseFormat(schema, "schema"), undefined, 4))
}

const Person_schema = z.object({
  name: z.string(),
  age: z.number().int(),
})
type Person = z.infer<typeof Person_schema>

const example_person: Person = {
  "name": "Bobert",
  "age": 1,
}

const example_untype_person: any = {
  "name": "Bobert",
  "age": 1
}

// const example_typed_person = Person_schema.parse(example_untype_person)

// check(Person_schema)

// check(z.object({ name: z.string(), age: z.bigint() }))
// check(z.enum(["a", "b", "c"]))
// check(z.union([z.number(), z.string()]))
check(z.union([z.object({ x: z.number() }), z.object({ y: z.string() })]))
// check(z.intersection(z.object({ x: z.number() }), z.object({ y: z.string() })))
// check(z.literal("hello"))
// check(z.tuple([z.string(), z.string(), z.string()]))
// check(z.boolean())
// check(z.object({ arg: z.array(z.string()) }))
// check(z.object({ arg: z.nullable(z.string()) }))