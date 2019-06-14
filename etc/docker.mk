.DEFAULT_GOAL = help

export SHELL = /bin/bash
export PATH = ${HOME}/.cabal/bin:${HOME}/.ghcup/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export BOOTSTRAP_HASKELL_NONINTERACTIVE = true
export GHC_VERSION = 8.6.5

init: install-ghcup-deps install-ghcup install-ghc install-pkgs ## install projects dependencies

install-ghcup-deps: ## install ghcup dependencies
	apt-get update -y
	apt-get install -y libc-bin curl coreutils gcc libgmp-dev libnuma-dev libtinfo-dev zlib1g-dev xz-utils zip

install-ghcup: ## install ghcup
	curl https://raw.githubusercontent.com/haskell/ghcup/master/bootstrap-haskell -sSf | sh

install-ghc: ## install ghc
	ghcup install $(GHC_VERSION)
	ghcup set $(GHC_VERSION)

install-pkgs: ## install hackage binaries
	ghcup install-cabal
	cabal new-install --overwrite-policy=always Cabal cabal-install
	cabal new-update
	cabal new-install --overwrite-policy=always fswatcher
	cabal new-install --overwrite-policy=always hlint

cabal-config: ## user cabal config
	cabal user-config update

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
