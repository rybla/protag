import type { Server } from "bun";

const server: Server = Bun.serve({
  port: 8080,
  fetch(req: Request): Response {
    const url = new URL(req.url);
    var pathname = url.pathname;
    if (pathname.endsWith("/")) { pathname += "index.html" }
    const filePath = `./public${pathname}`;
    try { return new Response(Bun.file(filePath)); }
    catch (error) { return new Response("File not found", { status: 404 }); }
  },
});

console.log(`server url is http://${server.hostname}:${server.port}`);
