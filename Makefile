.DEFAULT_GOAL = help

export SHELL := /bin/bash

include etc/nix.mk

OPT =
BIN ?= lamh

dev: clean ## build continuously
	@cabal build 2>&1 | source-highlight --src-lang=haskell --out-format=esc
	@fswatcher --path . --include "\.hs$$|\.cabal$$" --throttle 31 cabal -- $(OPT) build 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

dev-ghcid: clean ## build continuously using ghcid
	@ghcid --command="cabal $(OPTS) repl -fwarn-unused-binds -fwarn-unused-imports -fwarn-orphans" \
	       --reload=app/lamh.hs \
	       --restart=lamh.cabal \
	| source-highlight --src-lang=haskell --out-format=esc

build: clean # lint (breaks on multiple readers) ## build
	cabal $(OPT) build --jobs='$$ncpus' | source-highlight --src-lang=haskell --out-format=esc

test: ## test
	cabal $(OPT) test

lint: ## lint
	hlint app src

clean: ## clean
	cabal $(OPT) clean

run: ## run main, default: BIN=lamh
	cabal $(OPT) run ${BIN}

repl: ## repl
	cabal $(OPT) repl

help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed 's/^Makefile://1' \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	-@ghc --version
	-@cabal --version
	-@hlint --version
	-@ghcid --version --ignore-loaded
	-@echo BIN=$(BIN)

# @todo: not indempotent--fix later
init: ## init or update project but run "make install-nix" first
	${MAKE} -f etc/init.mk init

install-nix: # install nix
	${MAKE} -f etc/init.mk install-nix

shell: ## initialize project
	${MAKE} -f etc/init.mk nix-shell

# @todo: not indempotent--fix later
update: ## update project depedencies
	${MAKE} -f etc/init.mk cabal-update
	${MAKE} -f etc/init.mk install-pkgs

lambda-dev: ## deploy to s3 bucket in development
	cd etc && ${MAKE} -f deploy.mk $@

lambda-prod: ## deploy to s3 bucket in production
	cd etc && ${MAKE} -f deploy.mk $@

trigger-dev: ## trigger lambda in dev
	touch /tmp/_SYNC && aws s3 cp /tmp/_SYNC s3://earnest-deli-test-dev-us-east-1/

zip: ## build and zip lambda function
	${MAKE} -f etc/deploy.mk $@

curl: ## curl --head https://google.com
	echo $(NIX_SSL_CERT_FILE)
	curl --head https://google.com/
