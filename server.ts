const port = 8013;

const server = Bun.serve({
  port,
  async fetch(req) {
    const url_str = req.url.endsWith("/") ? `${req.url}index.html` : req.url
    const url = new URL(url_str);
    const filePath = `dist${url.pathname}`;
    console.log(`GET ${filePath}`)
    const file = Bun.file(filePath);
    if (!(await file.exists())) return new Response(`Not Found: ${url_str}`, { status: 404 });
    return new Response(file);
  }
});

console.log(`serving at http://${server.hostname}:${server.port}`);