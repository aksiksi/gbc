check:
	cargo check --no-default-features

shell:
	nix develop -c zsh

.PHONY: shell

