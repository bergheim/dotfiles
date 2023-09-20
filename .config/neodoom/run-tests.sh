#!/usr/bin/env sh

TMPDIR=$(mktemp -d)
CACHE_DIR="$TMPDIR/cache"

# Copy your configuration to the temporary directory
cp -r ~/.config/neodoom/* "$TMPDIR/"

emacs --batch \
    --init-directory ~/.config/neodoom  \
    --eval "(setq load-prefer-newer t)" \
    --eval "(setq debug-on-error t)" \
    --eval "(setq bergheim/config-dir \"$TMPDIR/\")" \
    --eval "(setq bergheim/cache-dir \"$CACHE_DIR/\")" \
    -l ~/.config/neodoom/init.el -f bergheim/run-all-tests 2>&1 |
    grep -E -v "Loading |already compiled"

rm -rf "$TMPDIR"
