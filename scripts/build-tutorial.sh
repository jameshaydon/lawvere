#!/usr/bin/env bash
set -euo pipefail

# Render README.md into _site/tutorial.html using pandoc, themed to match
# the parchment palette of the web frontend.

if ! command -v pandoc >/dev/null; then
  echo "Error: pandoc not found on PATH." >&2
  echo "Install via 'brew install pandoc' (macOS) or 'apt-get install pandoc' (Debian/Ubuntu)." >&2
  exit 1
fi

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"
OUT="$PROJECT_ROOT/_site"
mkdir -p "$OUT"

echo "Rendering README.md → _site/tutorial.html ..."
pandoc README.md \
  --from gfm \
  --to html5 \
  --standalone \
  --toc --toc-depth=3 \
  --css=tutorial.css \
  --include-in-header  web/favicon_header.html \
  --include-before-body web/tutorial_header.txt \
  --metadata title="Lawvere — Tutorial" \
  --output "$OUT/tutorial.html"

cp web/tutorial.css "$OUT/"
echo "Done."
