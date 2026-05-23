# Lawvere — browser frontend

Static assets served alongside the WebAssembly build of the Lawvere
interpreter.

## Files

| File                        | Purpose                                                          |
| --------------------------- | ---------------------------------------------------------------- |
| `index.html`                | Landing page, hosts the xterm.js terminal.                       |
| `lawvere-terminal.js`       | Terminal controller, WASI bootstrap, JSFFI wiring.               |
| `tutorial.css`              | Stylesheet for `tutorial.html` (rendered from README.md).        |
| `tutorial_header.txt`       | pandoc `--include-before-body` for the tutorial.                 |
| `favicon_header.html`       | pandoc `--include-in-header` (favicon + Google Font links).      |
| `manifest.json`             | PWA manifest.                                                    |
| `icon.svg`                  | Source artwork for the favicon set (A → B morphism).             |
| `favicon*.png`, `favicon.ico`, `apple-touch-icon.png`, `icon-*.png` | Generated from `icon.svg`. |

Files that are generated at build time and **not** checked in:

- `samples.js` — bundled `examples/*.law` + `README.md` (run `scripts/generate-samples-js.sh`).
- `_site/lawvere.wasm`, `_site/lawvere.js` — compiled module + JSFFI glue.

## Prerequisite

GHC WebAssembly backend is necessary.

See [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) repository for installation.

## Build

From the project root, with the GHC wasm toolchain on `PATH`:

```bash
./scripts/build-wasm.sh
./scripts/build-tutorial.sh   # optional: builds tutorial.html
```

Output lands in `_site/`.

## Local preview

```bash
python3 -m http.server -d _site 8000
# open http://localhost:8000/
```

## Regenerating the favicons

```bash
cd web
rsvg-convert -w 512 -h 512 icon.svg -o icon-512x512.png
rsvg-convert -w 192 -h 192 icon.svg -o icon-192x192.png
rsvg-convert -w 180 -h 180 icon.svg -o apple-touch-icon.png
rsvg-convert -w  32 -h  32 icon.svg -o favicon-32x32.png
rsvg-convert -w  16 -h  16 icon.svg -o favicon-16x16.png
magick favicon-16x16.png favicon-32x32.png favicon.ico
```
