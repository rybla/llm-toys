import OpenAI from "openai"

const DEBUG = true

export const generate_ = ({ ok, error }) => (args) =>
  async () => {
    try {
      if (DEBUG) console.log("request", JSON.stringify(args, undefined, 4))
      const openai = new OpenAI({
        apiKey: args.apiKey,
        baseURL: args.baseURL,
        dangerouslyAllowBrowser: true,
      })
      delete args.apiKey
      delete args.baseURL
      const completion = await openai.chat.completions.create(args)
      if (DEBUG) console.log("completion", JSON.stringify(completion, undefined, 4))
      return ok(completion.choices[0].message)
    }
    catch (e) {
      if (DEBUG) console.log("error", e)
      return error(e.toString())
    }
  }

export const generate_structure_ = ({ ok, error }) => (args) =>
  async () => {
    try {
      if (DEBUG) console.log("request", JSON.stringify(args, undefined, 4))
      const openai = new OpenAI({
        apiKey: args.apiKey,
        baseURL: args.baseURL,
        dangerouslyAllowBrowser: true,
      })
      delete args.apiKey
      delete args.baseURL
      const completion = await openai.beta.chat.completions.parse(args)
      if (DEBUG) console.log("completion", JSON.stringify(completion, undefined, 4))
      return ok(completion.choices[0].message)
    }
    catch (e) {
      if (DEBUG) console.log("error", e)
      return error(e.toString())
    }
  }