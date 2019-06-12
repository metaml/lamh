.DEFAULT_GOAL = help

PROJECT = $(shell dirname ${PWD})
VERSION := polysemy-$(shell date +%s)

AWS_CFN_STACKS ?= ${HOME}/proj/aws-cfn-stacks
LAMBDA = aws-cfn-app-deli-test-s3-dev-bucket-lambda

lambda-dev: ## deploy to dev's s3 bucket
lambda-dev: URL = s3://earnest-lambda-code-dev-us-east-1/s3-lambda/
lambda-dev: export AWS_PROFILE = development
lambda-dev: clean lambda s3-cp
	$(AWS_CFN_STACKS)/bin/deli-cf update $(LAMBDA) --param version:$(VERSION)

lambda-prod: ## deploy to prod's s3 bucket
lambda-prod: URL = s3://earnest-lambda-code-us-east-1/s3-lambda/
lambda-prod: export AWS_PROFILE = production
lambda-prod: clean lambda s3-cp
	echo noop: $(AWS_CFN_STACKS)/bin/deli-cf update $(LAMBDA) --param version:$(VERSION)

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
	cd ../deploy && mv s3-lambda.zip s3-lambda-${VERSION}.zip
	cd .. && aws s3 cp deploy/s3-lambda-${VERSION}.zip $(URL)

clean: ## create build dir or empty it
	( cd .. && mkdir -p deploy && rm -rf deploy/* )

update-docker: ## update project in docker
	docker run --rm --interactive --tty --volume $(PROJECT):/proj --volume $(PROJECT)/deploy:/root --workdir /proj ghc make init-docker

help: ## help
	@grep -E '^[a-zA-Z00-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
