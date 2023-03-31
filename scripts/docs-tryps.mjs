import fs from "fs/promises";
import path from "path";
import { fileURLToPath } from "url";
import { globby } from "globby";
import lz from "lz-string";

let
  paths = [],
  exitCode = 0;

if (process.env.WATCHEXEC_WRITTEN_PATH) {
  paths = process.env.WATCHEXEC_WRITTEN_PATH.split(path.delimiter);
}
else {
  const __dirname = path.dirname(fileURLToPath(import.meta.url));
  paths = await globby(path.join(__dirname, "..", "docs", "**", "*.md"));
}

for (let p of paths) {
  const original = await fs.readFile(p, "utf8");
  const updated = updateMarkdown(original);
  if (original !== updated) {
    if (process.argv.includes("--check")) {
      console.error(`Try PureScript link is missing or stale in ${p}`);
      exitCode = 1;
    }
    else {
      process.stdout.write(`* Updating ${p}…`);
      await fs.writeFile(p, updated);
      process.stdout.write("done!\n");
    }
  }
}

if (exitCode) {
  process.exit(exitCode);
}

function updateMarkdown(markdown) {
  return markdown.replace(/```haskell[\S\s]*```(\s*\[!\[Open with Try PureScript\S+)?/gm, content => {
    let lines = content.trim().split("\n");
    lines = lines.slice(lines.findIndex(x => /```haskell/.test(x)) + 1);
    lines = lines.slice(0, lines.findIndex(x => /```/.test(x)));
    const code = lines.join("\n");
    return `\`\`\`haskell
${code}
\`\`\`

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=${lz.compressToEncodedURIComponent(code)})`;
  });
}