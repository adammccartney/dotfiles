SHELL := /bin/sh

setup-bash-config:
	mk-scripts/setup-bash-config

stow-no-guix-home:
	mk-scripts/stow-no-guix-home

restow-no-guix-home:
	mk-scripts/restow-no-guix-home

unstow-no-guix-home:
	mk-scripts/unstow-no-guix-home

generate_systemd_values_files:
	./bin/gen-emacs-systemd-config
