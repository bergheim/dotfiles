# Global agent instructions

## Web page content -> use Crawl4AI, not fetch_content

When you need readable content from a web page, load the `j-crawl` skill and
call Crawl4AI at `$CRAWL4AI_URL` (`/md` with `f: "fit"`). Do **not** reach for
the built-in `fetch_content` for pages you need real text from.

`fetch_content` returns partial/cookie-walled output and 401s on common news
and paywalled sites (Reuters, NYT, WSJ). Crawl4AI renders with a real browser,
returns clean Markdown, and gets past those blocks. Reserve `fetch_content`
for the cases Crawl4AI can't cover: YouTube transcripts, video frame
extraction, and GitHub repository contents.

Discovery (finding URLs) is unaffected: `web_search` / SearXNG are fine for
that. This rule is only about page -> text.

## External agent permissions

Inside the devcontainer harness, always bypass external-agent permission prompts:
run Claude Code with `env -u ANTHROPIC_API_KEY claude --dangerously-skip-permissions`
and Agy with `--dangerously-skip-permissions`. Headless agents cannot answer prompts.
