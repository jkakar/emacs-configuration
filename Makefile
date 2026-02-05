.PHONY: rebuild

rebuild:
	mkdir -p ~/.config/emacs-plus
	cat > ~/.config/emacs-plus/build.yml <<'EOF'
# Use a community icon (see community/registry.json for available options)
icon: elrumo2

# Use an external icon (requires SHA256 verification)
# icon:
#   url: https://example.com/my-icon.icns
#   sha256: abc123...

# Apply community patches
# patches:
#   - patch-name-from-registry
EOF
	brew uninstall emacs-plus
	brew install emacs-plus
