.DEFAULT_GOAL = help

# called from top level Makefile
# include etc/nix.mk

init: nix ## initialize project

nix: ## nix
	curl -L https://nixos.org/nix/install | sh

cabal-update: ## cabal update
	cabal v2-update

cabal-config: ## user cabal config
	cabal user-config -update

clobber: ## clobber
	echo noop

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
