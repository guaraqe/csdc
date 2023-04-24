.DEFAULT_GOAL := help

################################################################################
# Server

serve: ## Launch the server.
	mkdir -p tmp
	cabal run --ghc-options="-O0" -- csdc-server serve --config=config.json

.PHONY: serve

TEST_ARGS=

serve-test: ## Launch the test script in the server app.
	mkdir -p tmp
	cabal run --ghc-options="-O0" -- csdc-server test --config=config.json $(TEST_ARGS)

.PHONY: serve-test

################################################################################
# GUI

gui-build: ## Build the GUI into www/app.js.
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
	ghcid --command 'cabal repl csdc-api --ghc-options="-O0"'

.PHONY: ghcid-api

ghcid-base: ## Launch ghcid for csdc-base.
	ghcid --command 'cabal repl csdc-base --ghc-options="-O0"'

.PHONY: ghcid-base

ghcid-server: ## Launch ghcid for the server executable.
	ghcid --command 'cabal repl csdc-api:csdc-server --ghc-options="-O0"'

.PHONY: ghcid-server

format-haskell: ## Format the Haskell code
	ormolu --mode inplace $$(git ls-files '*.hs')

.PHONY: format-haskell

hoogle: ## Launch the Hoogle server
	hoogle server --local -p 8888

.PHONY: hoogle

################################################################################
# Elm development

elm-nix-update: ## Update Nix files for Elm.
	cd csdc-gui && elm2nix convert > elm-srcs.nix && elm2nix snapshot

.PHONY: elm-nix-update

elm-docs: ## Elm Docs
	edp csdc-gui

.PHONY: elm-docs

################################################################################
# Database

psql: ## Launch psql with the correct arguments.
	PGHOST=localhost PGPORT=5432 PGDATABASE=csdc PGUSER=csdc PGPASSWORD=csdc psql

.PHONY: psql

################################################################################
# Docker

docker: ## Create and start the Docker image.
	docker-compose \
	  --file database/postgresql.yml \
	  --file database/ipfs.yml \
		build
	touch .docker
	docker-compose \
	  --file database/postgresql.yml \
	  --file database/ipfs.yml \
		up -d

.PHONY: docker

docker-clean: ## Clean the Docker image.
	docker-compose --file database/postgresql.yml down -v
	rm -f .docker

.PHONY: docker-clean

################################################################################
# Deployment

build-and-load-image:
	docker load < $$(nix-build deployment)

tag-and-push-image: build-and-load-image
	docker tag $$(cat secrets.json | jq -r .app_name) registry.fly.io/$$(cat secrets.json | jq -r .app_name)
	flyctl auth docker
	docker push registry.fly.io/$$(cat secrets.json | jq -r .app_name)

deploy: tag-and-push-image
	flyctl deploy --app $$(cat secrets.json | jq -r .app_name)

# Debugging

trytond-build:
	nix-build release.nix

start-image:
	docker run --network="host" gnuhealth-nix

inspect-image:
	docker exec -t -i $(docker ps -aq -f ancestor=gnuhealth-nix) /bin/bash

stop-image:
	docker stop $(docker ps -aq -f ancestor=gnuhealth-nix)

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
