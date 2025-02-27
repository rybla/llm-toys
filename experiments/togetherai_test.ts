// PROBLEM: doesn't work with openai's version for some reason...

// import { z } from "zod"
// import OpenAI from "openai"
// import { zodResponseFormat } from "openai/helpers/zod.mjs"

// const client = new OpenAI({
//   apiKey: process.env.TogetherAi_ApiKey,
//   baseURL: "https://api.together.xyz/v1"
// })

// const Person_schema = z.object({
//   name: z.string(),
//   age: z.number().int(),
// })
// type Person = z.infer<typeof Person_schema>

// const response = await client.chat.completions.create({
//   // model: "meta-llama/Llama-3.3-70B-Instruct-Turbo",
//   model: "meta-llama/Meta-Llama-3.1-70B-Instruct-Turbo",
//   messages: [
//     { role: "user", content: "Generate some sample person data." },
//   ],
//   response_format: zodResponseFormat(Person_schema, "person"),
// })

// const msg = response.choices[0]!.message!
// const json = JSON.parse(msg.content!)
// const data = Person_schema.parse(json)
// console.log(JSON.stringify(data, undefined, 4))

import { z } from "zod"
import { zodToJsonSchema } from "zod-to-json-schema"
import Together from "together-ai"

const client = new Together({ apiKey: process.env.TogetherAi_ApiKey })

if (false) {
  const response = await client.chat.completions.create({
    model: "meta-llama/Llama-3.3-70B-Instruct-Turbo",
    messages: [
      { role: "user", content: "Write a short 1-paragraph tale of the Banana Wizard." },
    ],
  })
  console.log(JSON.stringify(response.choices[0]?.message, undefined, 4))
}

if (true) {
  const Person_schema = z.object({
    name: z.string(),
    age: z.number().int(),
  })
  const Persons_schema = z.array(Person_schema)

  const response = await client.chat.completions.create({
    // model: "meta-llama/Llama-3.3-70B-Instruct-Turbo", // this model doesn't work
    model: "meta-llama/Meta-Llama-3.1-70B-Instruct-Turbo",
    messages: [
      { role: "user", content: "Generate some sample person data." },
    ],
    response_format: {
      type: "json_object",
      // @ts-ignore
      schema: zodToJsonSchema(Person_schema, { target: "openAi" }),
    }
  })

  const msg = response.choices[0]!.message!
  const json = JSON.parse(msg.content!)
  const data = Person_schema.parse(json)
  console.log(JSON.stringify(data, undefined, 4))
}
