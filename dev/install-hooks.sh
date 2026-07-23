#!/bin/sh
#
# dev/install-hooks.sh -- install this repo's tracked git hooks into the local clone.
#
# Git hooks live in .git/ and are NOT tracked, so they don't travel with a clone or to another
# lab machine. This copies the tracked hook(s) from dev/git-hooks/ into the active hooks dir.
# Run it once per clone (and again after editing a tracked hook, to sync).
#
#   sh dev/install-hooks.sh
#
# Idempotent. Works from the main checkout or a worktree.

set -e

repo_root=$(git rev-parse --show-toplevel)
src="$repo_root/dev/git-hooks"

# Respect an explicitly-configured core.hooksPath; otherwise use the shared .git/hooks
# (common dir), which also covers worktrees.
hooks_dir=$(git config --get core.hooksPath || true)
if [ -z "$hooks_dir" ]; then
  hooks_dir="$(git rev-parse --git-common-dir)/hooks"
fi
# Resolve a relative core.hooksPath against the repo root.
case "$hooks_dir" in
  /*) : ;;                       # already absolute
  *)  hooks_dir="$repo_root/$hooks_dir" ;;
esac

mkdir -p "$hooks_dir"

installed=0
for hook in "$src"/*; do
  [ -f "$hook" ] || continue
  name=$(basename "$hook")
  cp "$hook" "$hooks_dir/$name"
  chmod +x "$hooks_dir/$name"
  echo "installed: $hooks_dir/$name"
  installed=$((installed + 1))
done

if [ "$installed" -eq 0 ]; then
  echo "no hooks found in $src" >&2
  exit 1
fi

echo "Done. $installed hook(s) installed into $hooks_dir"
