.DEFAULT_GOAL := help

################################################################################
# Server

serve: ## Launch the server.
	cabal v2-run -- csdc-server config.json secret.json

.PHONY: serve

################################################################################
# GUI

gui-build: ## Build the GUI into www/app.js
	mkdir -p www
	cd csdc-gui && elm make src/Main.elm --output ../www/app.js

.PHONY: gui-build

gui-build-optimized: ## Build the GUI into www/app.min.js
	mkdir -p www
	cd csdc-gui && elm make --optimize src/Main.elm --output ../www/app.js
	uglifyjs www/app.js \
	  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output=www/app.min.js

.PHONY: gui-build-optimized

################################################################################
# Haskell development

ghcid-api: ## Launch ghcid for csdc-api.
	ghcid --command "cabal v2-repl csdc-api"

.PHONY: ghcid-api

ghcid-base: ## Launch ghcid for csdc-base.
	ghcid --command "cabal v2-repl csdc-base"

.PHONY: ghcid-base

ghcid-auth: ## Launch ghcid for csdc-auth.
	ghcid --command "cabal v2-repl csdc-auth"

.PHONY: ghcid-auth

ghcid-server: ## Launch ghcid for the server executable.
	ghcid --command "cabal v2-repl csdc-api:csdc-server"

.PHONY: ghcid-server

################################################################################
# Elm development

elm-nix-update: ## Update Nix files for Elm.
	cd csdc-gui && elm2nix convert > elm-srcs.nix

.PHONY: elm-nix-update

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
