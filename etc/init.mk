.DEFAULT_GOAL = help

GHC_VERSION = 8.6.5

export SHELL = /bin/bash
export PATH = ${HOME}/.cabal/bin:${HOME}/.ghcup/bin:/usr/local/bin:/usr/bin:/bin
export BOOTSTRAP_HASKELL_NONINTERACTIVE = true

init: install-ghcup install-ghc cabal-update install-pkgs ## install projects dependencies

clobber: ## clobber ~/.cabal and ~/.ghcup
	rm -rf ~/.cabal ~/.ghcup

install-ghcup: install-ghcup-deps  ## install ghcup
	curl https://get-ghcup.haskell.org -sSf | sh

install-ghc: ## install ghc
	ghcup install $(GHC_VERSION)
	ghcup set $(GHC_VERSION)

install-pkgs: BINS = ghcid hlint fswatcher
install-pkgs: ## install hackage binaries
	for i in $(BINS); do cabal v2-install --overwrite-policy=always $$i; done

install-ghcup-deps: ## install ghcup dependencies
	- brew update
	brew upgrade --force
	brew install curl coreutils gcc@8 gmp make ncurses python3 source-highlight xz
	- brew unlink gcc
	- brew unlink gcc@7
	brew unlink gcc@8 && brew link gcc@8

cabal-update: BINUP = ~/.ghcup/bin
cabal-update: ## cabal update
	ghcup set $(GHC_VERSION)
	ghcup install-cabal
	$(BINUP)/cabal v2-update

cabal-config: ## user cabal config
	cabal user-config update

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

.PHONY: install-pkgs
