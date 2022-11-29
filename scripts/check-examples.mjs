import chalk from "chalk";
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

  const { stdout: actual } = await execa("spago", ["-x", "example.dhall", "run", "-p", path.join(examplesDir, `${example}.purs`), "-m", `Example.${example}`]);

  const needsUpdate = updateFlag || !expected;

  if (needsUpdate) {
    await fs.writeFile(exampleFile, actual);
  }
  else {
    const diffs = diffLines(expected, actual);
    if (diffs.some(({ added, removed }) => added || removed)) {
      process.stderr.write(`Unexpected changes in ${example}.css:\n\n`);
      for (let part of diffs) {
        const color = part.added ? "green" : part.removed ? "red" : "grey";
        if (part.added) {
          process.stderr.write(chalk.green(part.value));
        }
        else if (part.removed) {
          process.stderr.write(chalk.red(part.value));
        }
        else {
          process.stderr.write(chalk.gray(part.value));
        }
      }
      process.exit(1);
    }
  }
}

process.stdout.write(chalk.green("✓ CSS output is unchanged.") + "\n");

const typeErrorsDir = path.join(examplesDir, "type-errors");

const typeErrorsListing = await fs.readdir(typeErrorsDir);
const typeErrors = typeErrorsListing.flatMap(x => x.match(/^[A-Za-z]+(?=\.purs$)/g) || []);

for (let typeError of typeErrors) {
  try {
    await execa("spago", ["build", "-p", path.join(typeErrorsDir, `${typeError}.purs`)]);
    process.stdout.write(chalk.red(`✗ TypeError.${typeError} compiled successfully.\n`));
    process.exit(1);
  }
  catch {
    continue;
  }
}

process.stdout.write(chalk.green("✓ All type error examples fail to compile.") + "\n");
