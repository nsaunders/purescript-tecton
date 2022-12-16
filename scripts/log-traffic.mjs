import fs from "fs/promises";
import path from "path";
import { fileURLToPath } from "url";
import fetch from "node-fetch";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const trafficPath = path.resolve(__dirname, "../meta", "./traffic.json");

async function get(entity) {
  const res = await fetch(
    `https://api.github.com/repos/${
      process.env.GITHUB_REPOSITORY
    }/traffic/${
      entity
    }`, {
      headers: {
        "Accept": "application/vnd.github+json",
        "Authorization": `Bearer ${process.env.GITHUB_API_KEY}`,
        "X-GitHub-Api-Version": "2022-11-28",
      },
    }
  );
  if (!res.ok) {
    throw new Error(`Unexpected ${res.status} response: ${await res.text()}`);
  }
  const data = await res.json();
  const x = data[entity];
  if (!x) {
    throw new Error(`Unexpected ${res.status} response: ${JSON.stringify(data)}`);
  }
  return x
    .sort((a, b) => {
      const tsa = new Date(a.timestamp);
      const tsb = new Date(b.timestamp);
      if (tsa < tsb) return -1;
      if (tsa > tsb) return 1;
      return 0;
    })
    .reverse()
    .slice(0, 14)
    .reverse();
}

async function read() {
  try {
    return JSON.parse(await fs.readFile(trafficPath, "utf8"));
  }
  catch {
    return {
      clones: [],
      views: [],
    };
  }
}

async function write(updated) {
  return await fs.writeFile(trafficPath, JSON.stringify(updated, null, 2));
}

function merge(updated, existing) {
  return [
    ...existing.filter(existing =>
      !updated.some(updated => updated.timestamp === existing.timestamp)
    ),
    ...updated,
  ];
}

if (!process.env.GITHUB_API_KEY) {
  throw new Error("Required environment variable undefined: GITHUB_API_KEY");
}

if (!process.env.GITHUB_REPOSITORY) {
  throw new Error("Required environment variable undefined: GITHUB_REPOSITORY");
}

const { clones, views } = await read();
await write({
  clones: merge(await get("clones"), clones),
  views: merge(await get("views"), views),
});
