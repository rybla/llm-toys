import { z } from "zod"
import OpenAI from "openai"
import { zodResponseFormat } from "openai/helpers/zod.mjs"

const client = new OpenAI({
  apiKey: process.env.GoogleAi_ApiKey,
  baseURL: "https://generativelanguage.googleapis.com/v1beta/openai",
})

const Person_schema = z.object({
  name: z.string(),
  age: z.number().int(),
})

const response = await client.chat.completions.create({
  model: "gemini-2.0-flash",
  messages: [
    { role: "user", content: "Generate some sample person data." },
  ],
  response_format: zodResponseFormat(Person_schema, "person"),
})
const msg = response.choices[0]!.message!
const json = JSON.parse(msg.content!)
const data = Person_schema.parse(json)
console.log(JSON.stringify(data, undefined, 4))
