.DEFAULT_GOAL = help

PROJECT = $(shell dirname ${PWD})
S3_BIN = s3-lambda
VERSION ?= $(shell date +%s)

deploy-dev: ## deploy to s3 bucket in development
deploy-dev: URL = s3://earnest-lambda-code-dev-us-east-1/s3-lambda/
deploy-dev: VER := $(VERSION)
deploy-dev: export AWS_PROFILE = development
deploy-dev: clean lambda s3-cp

deploy-prod: ## deploy to s3 bucket in production
deploy-prod: URL = s3://earnest-lambda-code-us-east-1/s3-lambda/
deploy-prod: export AWS_PROFILE = production
deploy-prod: clean lambda

lambda: ## zip lambda (linux-binary) in <lamha>/deploy
	docker run --rm --interactive --tty --volume $(PROJECT):/proj --volume $(PROJECT)/deploy:/root --workdir /proj ghc make zip

zip: clean ## build and zip lambda function: lamha
	[ "$(shell uname -s)" = "Linux" ] || ( echo "error: must be linux" && exit 1 )
	cabal new-configure --prefix=deploy/bootstrap --disable-executable-dynamic
	cabal new-build
	cabal new-install --overwrite-policy=always exe:lamha
	strip /root/.cabal/bin/lamha
	cp /root/.cabal/bin/lamha deploy/bootstrap
	cd deploy && zip s3-lambda.zip bootstrap && rm -f bootstrap

s3-cp: ## copy zip to s3
	cd ../deploy && mv s3-lambda.zip s3-lambda-${VER}.zip
	cd .. && aws s3 cp deploy/s3-lambda-${VER}.zip $(URL)

clean: ## create build dir or empty it
	( cd .. && mkdir -p deploy && rm -rf deploy/* )

update-docker: ## update project in docker
	docker run --rm --interactive --tty --volume $(PROJECT):/proj --volume $(PROJECT)/deploy:/root --workdir /proj ghc make init-docker

help: ## help
	@grep -E '^[a-zA-Z00-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
