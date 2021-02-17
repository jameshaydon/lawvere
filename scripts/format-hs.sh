#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../"

# Format Haskell using ormolu

ormolu_opts=(
  --ghc-opt -XTypeApplications
  --ghc-opt -XNumericUnderscores
  --ghc-opt -XBangPatterns
  --ghc-opt -XPatternSynonyms
  --ghc-opt -XOverloadedLabels
  --mode inplace
  --check-idempotence
)

for file in $(git ls-files | grep -e '\.hs$'); do
  ormolu "${ormolu_opts[@]}" "$file"
done

