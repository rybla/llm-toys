import OpenAI from "openai";

export const generate_ = ({
  ok, err,
  apiKey, baseURL,
  model, messages, tools, tool_choice, temperature,
}) => async () => {
  try {
    const openai = new OpenAI({ apiKey, baseURL, dangerouslyAllowBrowser: true, });
    const completion = await openai.chat.completions.create({ model, messages, tools, tool_choice, })
    return ok(completion.choices[0].message);
  }
  catch (error) {
    return err(error.toString());
  }
}