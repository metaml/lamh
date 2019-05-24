.DEFAULT_GOAL = help

export SHELL = /bin/bash
export PATH = ${HOME}/.cabal/bin:${HOME}/.ghcup/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export BOOTSTRAP_HASKELL_NONINTERACTIVE = true
export GHC_VERSION = 8.6.5

init: install-ghcup install-ghc install-pkgs ## install projects dependencies

install-ghcup: install-ghcup-deps  ## install ghcup
	curl https://raw.githubusercontent.com/haskell/ghcup/master/bootstrap-haskell -sSf | sh

install-ghc: ## install ghc
	ghcup install $(GHC_VERSION)
	ghcup set $(GHC_VERSION)

install-pkgs: cabal-update ## install hackage binaries
	ghcup install-cabal
	cabal v2-install cabal-install
	cabal v2-install fswatcher
	cabal v2-install hlint

install-ghcup-deps: ## install ghcup dependencies
	apt-get install -y libc-bin curl coreutils gcc libgmp-dev libnuma-dev libtinfo-dev zlib1g-dev xz-utils

cabal-update: ## cabal update
	cabal v2-install --overwrite-policy=always Cabal cabal-install
	cabal v2-update

cabal-config: ## user cabal config
	cabal user-config update

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
