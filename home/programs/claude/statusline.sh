#!/bin/sh
# Locates the claude-lens plugin script dynamically since its path is
# versioned by the Claude Code plugin manager (not nix-pinned).
script=$(find "$HOME/.claude/plugins/cache" -path "*/claude-lens/*/claude-lens.sh" 2>/dev/null | sort -V | tail -1)
[ -x "$script" ] && exec "$script" "$@"
exit 0
