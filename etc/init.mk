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

install-pkgs: BINUP = ~/.ghcup/bin
install-pkgs: cabal-update ## install hackage binaries
	ghcup install-cabal
	$(BINUP)/cabal v2-install --installdir=${HOME}/.cabal --overwrite-policy=always cabal-install
        cabal v2-install --overwrite-policy=always fswatcher
        cabal v2-install --overwrite-policy=always ghcid
        cabal v2-install --overwrite-policy=always hlint
        cabal v2-install --overwrite-policy=always Cabal

install-ghcup-deps: ## install ghcup dependencies
	- brew update
	brew upgrade
	brew install curl coreutils gcc@8 gmp make ncurses python3 source-highlight xz
	- brew unlink gcc
	- brew unlink gcc@7
	brew unlink gcc@8 && brew link gcc@8

cabal-update: ## cabal update
	cabal v2-update
#	cabal v2-install --force-reinstalls --overwrite-policy=always Cabal cabal-install


cabal-config: ## user cabal config
	cabal user-config update

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
