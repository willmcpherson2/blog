#!/usr/bin/env bash

set -euxo pipefail

init_el="$1"
src_dir="$2"
out_dir="$3"

mkdir -p "$out_dir"
rm -rf "$out_dir"/*
cp -r --no-preserve=mode "$src_dir"/* "$out_dir"

find "$out_dir" -type f -name "*.org" | while read -r org_file; do
  emacs -l "$init_el" "$org_file" --batch -f org-html-export-to-html --kill
done
