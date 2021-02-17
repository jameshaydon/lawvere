#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../"

# Format Nix using nixpkgs-fmt

for file in $(git ls-files | grep -e '\.nix$'); do
  # Skip cabal2nix or niv generated files
  [ $(dirname "$file") == "nix/packages" ] && continue
  [ "$file" == "nix/sources.nix" ] && continue

  nixpkgs-fmt "$file"
done
