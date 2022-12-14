SHELL := /bin/bash

ifndef LIGO
LIGO=docker run -u $(id -u):$(id -g) --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next 
endif

PROTOCOL_OPT?=

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

test = @$(LIGO) run test ./test/$(1) $(PROTOCOL_OPT)

.PHONY: test

test: ## run tests (make test SUITE=fa2.1-single-asset.operators)
ifndef SUITE
	@$(call test,fa2-single-asset.test.mligo)
	@$(call test,fa2-multi-asset.test.mligo)
	@$(call test,fa2-NFT.test.mligo)
	@$(call test,fa2.1-single-asset.operators.test.mligo)
	@$(call test,fa2.1-multi-asset.operators.test.mligo)
	@$(call test,fa2.1-NFT.operators.test.mligo)
	@$(call test,fa2.1-single-asset.approvals.test.mligo)
	@$(call test,fa2.1-multi-asset.approvals.test.mligo)
	@$(call test,fa2.1-NFT.approvals.test.mligo)
else
	@$(call test,$(SUITE).test.mligo)
endif
