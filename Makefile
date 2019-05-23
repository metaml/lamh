.DEFAULT_GOAL = help

export SHELL = /bin/bash
export PATH = ${HOME}/.cabal/bin:${HOME}/.ghcup/bin:/usr/local/bin:/usr/bin:/bin

BIN ?= lamha
VERSION ?= 1

dev: clean ## build continuously
	@(echo building... && sleep 2 && touch lamha.cabal) &
	@fswatcher --path . --include "\.hs$$|\.cabal$$" --throttle 31 cabal v2-build 2>&1 \
	| awk '{ if ($$0 ~ /Process returned 1/) { print "$(RED)" "- failure" "$(NON)" } \
		 else if ($$0 ~ /Process completed successfully/) { print "$(GRN)" "- success" "$(NON)" } \
		 else if ($$0 ~ /^.*error:$$/) { print "$(MAG)" "- " $$0 "$(NON)" } \
                 else { print }; }'

build: clean # lint (breaks on multiple readers) ## build
	cabal v2-build --jobs=8

test: ## test
	cabal v2-test

lint: ## lint
	hlint app src

clean: ## clean
	cabal v2-clean

run: ## run main, default: BIN=baka
	cabal v2-run ${BIN}

repl: ## repl
	cabal v2-repl

# @todo: hook in docker to produce static linux binary
deploy-dev: ## deploy to s3 bucket in development
	echo ${MAKE} -f etc/deploy.mk zip-sync-dev VERSION=${VERSION}

deploy-prod: ## deploy to s3 bucket in production
	echo ${MAKE} -f etc/deploy.mk zip-sync-prod VERSION=${VERSION}

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	 | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	@ghc --version
	@cabal --version

init: ## initialize project
	${MAKE} -f etc/init.mk init

update: ## update project
	${MAKE} -f etc/init.mk cabal-update

# colors
NON = \033[0m
RED = \033[1;31m
GRN = \033[1;32m
BLU = \033[1;34m
MAG = \033[1;35m
CYN = \033[1;36m
