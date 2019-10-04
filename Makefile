.DEFAULT_GOAL = help

export SHELL = /bin/bash
export PATH = .:${HOME}/.cabal/bin:${HOME}/.ghcup/bin:/usr/local/bin:/usr/bin:/bin

BIN ?= lamha

dev: clean ## build continuously
	@cabal v2-build 2>&1 | source-highlight --src-lang=haskell --out-format=esc
	@fswatcher --path . \
	   	   --include "\.hs$$|\.cabal$$" \
		   --throttle 31 \
		   cabal v2-build 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

dev-ghcid: clean ## build continuously using ghcid
	@ghcid --command="cabal v2-repl -fwarn-unused-binds -fwarn-unused-imports -fwarn-orphans" \
	       --reload=app/lamh.hs \
	       --restart=lamh.cabal \
	| source-highlight --src-lang=haskell --out-format=esc

build: clean # lint (breaks on multiple readers) ## build
	cabal v2-build --jobs='$$ncpus'

test: ## test
	cabal v2-test

lint: ## lint
	hlint app src

clean: ## clean
	cabal v2-clean

run: ## run main, default: BIN=lamh
	cabal v2-run ${BIN}

repl: ## repl
	cabal v2-repl

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	 | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	@ghc --version
	@cabal --version
	@hlint --version
	@ghcid --version --ignore-loaded
	@echo BIN=${BIN}

# @todo: not indempotent--fix later
init: ## initialize project
	${MAKE} -f etc/init.mk init

# @todo: not indempotent--fix later
update: ## update project depedencies
	${MAKE} -f etc/init.mk install-pkgs

lambda-dev: ## deploy to s3 bucket in development
	cd etc && ${MAKE} -f deploy.mk $@

lambda-prod: ## deploy to s3 bucket in production
	cd etc && ${MAKE} -f deploy.mk $@

trigger-dev: ## trigger lambda in dev
	touch /tmp/_SYNC && aws s3 cp /tmp/_SYNC s3://earnest-deli-test-dev-us-east-1/

zip: ## build and zip lambda function
	${MAKE} -f etc/deploy.mk $@
