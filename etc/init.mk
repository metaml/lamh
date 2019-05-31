.DEFAULT_GOAL = help

GHC_VERSION = 8.6.5

export SHELL = /bin/bash
export PATH = ${HOME}/.cabal/bin:${HOME}/.ghcup/bin:/usr/local/bin:/usr/bin:/bin

init: install-ghcup install-ghc install-pkgs ## install projects dependencies

install-ghcup: install-ghcup-deps  ## install ghcup
	curl https://raw.githubusercontent.com/haskell/ghcup/master/bootstrap-haskell -sSf | sh

install-ghc: ## install ghc
	ghcup install $(GHC_VERSION)
	ghcup set $(GHC_VERSION)

install-pkgs: cabal-update ## install hackage binaries
	ghcup install-cabal
	cabal new-install cabal-install
	cabal new-install fswatcher
	cabal new-install hlint

install-ghcup-deps: ## install ghcup dependencies
	brew update
	brew install curl coreutils gcc@7 gmp make ncurses python3 xz
	- brew unlink gcc
	- brew unlink gcc@7 && brew link gcc@7

cabal-update: ## cabal update
	cabal new-install --overwrite-policy=always Cabal cabal-install
	cabal new-update

cabal-config: ## user cabal config
	cabal user-config update

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
