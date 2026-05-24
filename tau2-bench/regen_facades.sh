#!/usr/bin/env bash
# Regenerate the FIXED, committed per-domain tool facades in facades/ from the
# installed tau2-bench tool schemas. These files are committed and used as-is by
# run_bench.sh — they are NOT generated on the fly. Re-run this only when you
# upgrade tau2-bench (or add a domain), then review/commit the diff.
#
# Usage: ./regen_facades.sh [DOMAIN ...]   (defaults to the text-mode domains)
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p facades

DOMAINS="${*:-retail airline telecom mock telecom-workflow}"

camel() { echo "$1" | awk -F'[-_]' '{for(i=1;i<=NF;i++) printf "%s", toupper(substr($i,1,1)) substr($i,2)}'; }

for d in $DOMAINS; do
  out="facades/$(camel "$d").scala"
  if ./.venv/bin/python gen_tools.py --domain "$d" > "$out" 2>/dev/null; then
    echo "  $d -> $out ($(grep -c '^def ' "$out") tool fns)"
  else
    echo "  $d -> SKIP (generation failed — extra not installed? e.g. banking_knowledge needs [knowledge])"
    rm -f "$out"
  fi
done
echo "done. review & commit facades/*.scala"
