import path from "node:path";
import { pathToFileURL } from "node:url";
import { getExamplesPath, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default async function (pi: ExtensionAPI) {
  const entry = path.join(getExamplesPath(), "extensions", "subagent", "index.ts");
  const extension = await import(pathToFileURL(entry).href);
  return extension.default(pi);
}
