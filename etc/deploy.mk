.DEFAULT_GOAL = help

S3_BIN = s3-lambda
STS_S3_BIN = sts-s3-lambda
VERSION ?= 1
LINUX_BIN = $(shell stack --docker path --local-install-root)/bin

zip-sync-dev: zip s3-sync-dev ## zip and sync zipped lambda to s3 dev, require VERSION=<number>

zip-sync-prod: zip s3-sync-prod  ## zip and sync zipped lambda to s3 prod, require VERSION=<number>

s3-sync-dev: export AWS_PROFILE = development
	aws s3 cp lambda/s3-lambda-${VERSION}.zip s3://earnest-lambda-code-dev-us-east-1/s3-lambda/

s3-sync-prod: export AWS_PROFILE = production
s3-sync-prod: ## copy zipped lambda function to s3 prod
	aws s3 cp lambda/s3-lambda-${VERSION}.zip s3://earnest-lambda-code-us-east-1/s3-lambda/

## ${BIN} is dir and executable, e.g.: s3-lambda/s3-lambda
zip: zip-clean  ## create lambda zip package, e.g.: make zip VERSION=<number>
	@echo $(LINUX_BIN)
	cp $(LINUX_BIN)/s3-lambda lambda/bootstrap
	( cd lambda && zip s3-lambda-${VERSION}.zip bootstrap && rm -f bootstrap )
	cp $(LINUX_BIN)/sts-s3-lambda lambda/bootstrap
	( cd lambda && zip sts-s3-lambda-${VERSION}.zip bootstrap && rm -f bootstrap )

zip-clean: ## create build dir or empty it
	mkdir -p lambda && rm -rf ./lambda/*

pull-stack-build: ## pull stack-build docker image to build for ubuntu
	stack docker pull

help: ## help
	@grep -E '^[a-zA-Z00-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
