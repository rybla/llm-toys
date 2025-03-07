import OpenAI from "openai";

const port = 8013;

const server = Bun.serve({
  port,
  async fetch(req) {
    console.log(req.method, req.url);
    if (req.url === "http://localhost:8013/llm-toys/google/chat/completions") {
      try {
        const client = new OpenAI({
          apiKey: process.env.GoogleAi_ApiKey,
          baseURL: "https://generativelanguage.googleapis.com/v1beta/openai",
        })
        const data = await req.json()
        console.log("data:", data)
        if (!data) return new Response("No data", { status: 400 });
        const response = await client.chat.completions.create(data)
        console.log("response:", response)
        if (!response) return new Response("No response", { status: 500 });
        console.log("content:", response.choices[0]!.message!.content!)
        return Response.json(response)
      } catch (err) {
        console.error("Error:", err);
        return new Response(`Error: ${err}`, { status: 500 });
      }
    } else {
      var filePath = `docs${(new URL(req.url)).pathname}`;
      filePath = filePath.replace("/llm-toys", "")
      // console.log(`GET FILE ${filePath}`)
      const file = Bun.file(filePath);
      if (!(await file.exists())) return new Response(`Not Found: ${filePath}`, { status: 404 });
      return new Response(file);
    }
  }
});

console.log(`serving at http://${server.hostname}:${server.port}`);