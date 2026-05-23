#!/usr/bin/env bash
set -euo pipefail

# Build the Lawvere interpreter for WebAssembly and assemble the static site
# under _site/. Requires the GHC wasm toolchain on PATH.
#
# Pass --clean to wipe dist-newstyle before configuring.

echo "================================================"
echo "  Building Lawvere for WebAssembly"
echo "================================================"

if ! command -v wasm32-wasi-ghc >/dev/null; then
  echo "Error: wasm32-wasi-ghc not on PATH." >&2
  echo "You can follow https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta for installation." >&2
  exit 1
fi
if ! command -v wasm32-wasi-cabal >/dev/null; then
  echo "Error: wasm32-wasi-cabal not on PATH." >&2
  exit 1
fi

echo "  wasm32-wasi-ghc:   $(wasm32-wasi-ghc --version | head -n1)"
echo "  wasm32-wasi-cabal: $(wasm32-wasi-cabal --version | head -n1)"
echo ""

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"
OUT="$PROJECT_ROOT/_site"

if [ "${1:-}" = "--clean" ]; then
  echo "Cleaning dist-newstyle/ ..."
  rm -rf dist-newstyle
fi

mkdir -p "$OUT"

echo "Generating web/samples.js ..."
"$PROJECT_ROOT/scripts/generate-samples-js.sh"
echo ""

# Stack ships hpack; running 'stack build --dry-run' regenerates lawvere.cabal
# from package.yaml. If stack isn't around, the existing cabal file is used.
if command -v stack >/dev/null; then
  echo "Regenerating lawvere.cabal from package.yaml (via stack/hpack) ..."
  stack build --dry-run >/dev/null 2>&1 || true
fi

echo "Configuring Lawvere with -fWeb ..."
wasm32-wasi-cabal configure -fWeb

echo "Building bill-web ..."
wasm32-wasi-cabal build exe:bill-web

WASM_BIN=$(wasm32-wasi-cabal list-bin exe:bill-web)
if [ -z "$WASM_BIN" ] || [ ! -f "$WASM_BIN" ]; then
  echo "Error: could not locate built wasm binary." >&2
  exit 1
fi
echo "  → $WASM_BIN"

cp "$WASM_BIN" "$OUT/lawvere.wasm"

echo "Running post-link.mjs ..."
LIBDIR=$(wasm32-wasi-ghc --print-libdir)
if [ -f "$LIBDIR/post-link.mjs" ]; then
  node "$LIBDIR/post-link.mjs" -i "$OUT/lawvere.wasm" -o "$OUT/lawvere.js"
else
  echo "Error: post-link.mjs not found under $LIBDIR" >&2
  exit 1
fi

echo "Copying web/ assets ..."
cp web/index.html             "$OUT/"
cp web/lawvere-terminal.js    "$OUT/"
cp web/samples.js             "$OUT/"
cp web/manifest.json          "$OUT/"
cp web/favicon.ico            "$OUT/"
cp web/favicon-16x16.png      "$OUT/"
cp web/favicon-32x32.png      "$OUT/"
cp web/apple-touch-icon.png   "$OUT/"
cp web/icon-192x192.png       "$OUT/"
cp web/icon-512x512.png       "$OUT/"

WASM_SIZE=$(du -h "$OUT/lawvere.wasm" | cut -f1)

echo ""
echo "================================================"
echo "  Build complete!"
echo "================================================"
echo "  Output:    $OUT"
echo "  wasm size: $WASM_SIZE"
echo ""
echo "  Local preview:"
echo "    python3 -m http.server -d _site 8000"
echo "    open http://localhost:8000/"
echo ""
echo "  For the tutorial page, also run scripts/build-tutorial.sh."
