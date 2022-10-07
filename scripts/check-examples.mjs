import fs from "fs/promises";
import path from "path";
import { execa } from "execa";
import { diffLines } from "diff";

for (let x of ["examples", "example.dhall"]) {
  try {
    await fs.access(x);
  }
  catch {
    throw new Error(`Missing "${x}". Run this via "npm run check-examples".`);
  }
}

const updateFlag = process.argv.includes("--update");

const examplesDir = "examples";
const outputDir = path.join(examplesDir, "output");

await fs.mkdir(outputDir, { recursive: true });

const examplesListing = await fs.readdir(examplesDir);
const examples = examplesListing.flatMap(x => x.match(/^[A-Za-z]+(?=\.purs$)/g) || []);

for (let example of examples) {
  const exampleFile = path.join(outputDir, `${example}.css`);

  let expected = null;
  try {
    expected = await fs.readFile(exampleFile, "utf8");
  }
  catch {}

  const { stdout: actual } = await execa("spago", ["-x", "example.dhall", "run", "-m", `Example.${example}`]);

  const needsUpdate = updateFlag || !expected;

  if (needsUpdate) {
    await fs.writeFile(exampleFile, actual);
  }
  else {
    const diffs = diffLines(expected.trim(), actual.trim()).filter(({ added, removed }) => added || removed);
    if (diffs.length) {
      console.error([
        `Unexpected changes in ${example}.css:`,
        ...diffs.flatMap(({ added, value }) => {
          return value.split("\n").filter(x => x).map(line => `${added ? "+" : "-"} ${line}`);
        })
      ].join("\n"));
      process.exit(1);
    }
  }
}
