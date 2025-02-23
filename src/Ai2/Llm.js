import OpenAI from "openai";

const DEBUG = false

export const generate_ = (args) =>
  async () => {
    if (DEBUG) console.log("request", JSON.stringify(args, undefined, 4))
    const openai = new OpenAI({
      apiKey: args.apiKey,
      baseURL: args.baseURL,
      dangerouslyAllowBrowser: true,
    });
    delete args.apiKey
    delete args.baseURL
    const completion = await openai.chat.completions.create(args)
    if (DEBUG) console.log("response", JSON.stringify(completion.choices[0], undefined, 4))
    return completion.choices[0].message;
  }

