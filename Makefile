# Vairables that might need to be overriden.
ROOT_DIR ?= $(shell pwd)
BUILD_DIR ?= $(ROOT_DIR)/.build
OUTPUT_DIR ?= $(ROOT_DIR)/output
RTS_ARGS ?=
SRC_DIR ?= $(ROOT_DIR)/src
TEST_DIR ?= $(ROOT_DIR)/test

# Variables that we control
CLEAN_DEPS :=
DEPS := $(BUILD_DIR)/.deps
FIND_SRC_FILES_ARGS := \( -name '*.purs' -o -name '*.js' \) -type f
NODE_MODULES := $(ROOT_DIR)/node_modules/.stamp
PACKAGE_JSON := $(ROOT_DIR)/package.json
PACKAGE_LOCK := $(ROOT_DIR)/package-lock.json
SRC_FILES := $(shell find $(SRC_DIR) $(FIND_SRC_FILES_ARGS))
TEST_FILES := $(shell find $(TEST_DIR) $(FIND_SRC_FILES_ARGS))

# Colors for printing
CYAN := \033[0;36m
RESET := \033[0;0m

.DEFAULT_GOAL := test

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/help-unsorted: $(MAKEFILE_LIST) | $(BUILD_DIR)
	@grep \
		--extended-regexp '^[A-Za-z_-]+:.*?## .*$$' \
	  --no-filename \
	  $(MAKEFILE_LIST) \
	  > $@

$(BUILD_DIR)/help: $(BUILD_DIR)/help-unsorted | $(BUILD_DIR)
	@sort $< > $@

$(BUILD_DIR)/test.js: $(OUTPUT_DIR)/Test.Main/index.js | $(BUILD_DIR)
	npx purs bundle \
		$(RTS_ARGS) \
		$(OUTPUT_DIR)/*/*.js \
		--main Test.Main \
		--module Test.Main \
		--output $@

$(BUILD_DIR)/test.out: $(BUILD_DIR)/test.js
	node $< | tee $@.tmp # Store output in a temp file in case of a failure.
	mv $@.tmp $@ # Move the output where it belongs.

$(DEPS): packages.dhall spago.dhall $(NODE_MODULES) | $(BUILD_DIR)
	npx spago install $(RTS_ARGS)
	touch $@

$(NODE_MODULES): $(PACKAGE_JSON) $(PACKAGE_LOCK)
	npm install
	touch $@

$(OUTPUT_DIR)/Test.Main/index.js: $(SRC_FILES) $(TEST_FILES) $(DEPS)
	npx spago build -p "$(TEST_DIR)/Main.purs $(TEST_DIR)/Test/**/*.purs" -u "$(RTS_ARGS)"

.PHONY: clean
clean: $(CLEAN_DEPS) ## Remove all dependencies and build artifacts, starting with a clean slate
	rm -fr \
		$(BUILD_DIR) \
		$(OUTPUT_DIR) \
		$(ROOT_DIR)/.spago \
		$(ROOT_DIR)/node_modules

.PHONY: help
help: $(BUILD_DIR)/help ## Display this help message
	@awk 'BEGIN {FS = ":.*?## "}; {printf "$(CYAN)%-30s$(RESET) %s\n", $$1, $$2}' $<

.PHONY: test
test: $(BUILD_DIR)/test.out ## Build and run tests

.PHONY: variables
variables:
	$(info $$(DEPENDENCIES) is [$(DEPENDENCIES)])
	$(info $$(OUTPUT) is [$(OUTPUT)])
	$(info $$(SRCS) is [$(SRCS)])
	$(info $$(SRC_OUTPUTS) is [$(SRC_OUTPUTS)])
	$(info $$(TESTS) is [$(TESTS)])
	$(info $$(TEST_OUTPUTS) is [$(TEST_OUTPUTS)])