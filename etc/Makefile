.DEFAULT_GOAL = help

export LANG  = en_US.UTF-8

SHELL = /bin/bash
IMAGE ?= ghc

image: ## build ghc image
	docker build --tag ${IMAGE}:latest .

login: ## login into to docker hub
	docker login --username=metaml

push: ## push ghc image to docker hub
	docker tag ${IMAGE}:latest metaml/${IMAGE}:latest
	docker push metaml/${IMAGE}:latest

prune: ## prune
	docker image prune -a

clean: ## rm ghc image
	docker rmi --force ghc metaml/ghc

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-28s\033[0m %s\n", $$1, $$2}'
