SHELL := /bin/sh

setup-bash-config:
	bin/setup-bash-config

stow-no-guix-home:
	bin/stow-no-guix-home

restow-no-guix-home:
	bin/restow-no-guix-home

unstow-no-guix-home:
	bin/unstow-no-guix-home

generate_systemd_values_files:
	./scripts/gen-emacs-systemd-config
