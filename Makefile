# Time-stamp: <2018-02-13 18:41:13 kmodi>

# Makefile to export org documents to md for Hugo from the command line
# Run just "make" to see usage examples.

MAKE_ := $(MAKE) --no-print-directory

EMACS ?= emacs
EMACS_exists := $(shell command -v $(EMACS) 2> /dev/null)
ifeq ("$(EMACS_exists)","")
	EMACS := /tmp/emacs/bin/emacs
endif

# EMACS_BIN_SOURCE and EMACS_BIN_VERSION are used later in the vcheck rule
# only if EMACS_exists has evaluated to "".
EMACS_BIN_SOURCE ?= https://github.com/npostavs/emacs-travis/releases/download/bins
EMACS_BIN_VERSION ?= 26

# Directory where the required elisp packages are auto-installed
TMPDIR ?= /tmp
ELESS_ELPA=$(TMPDIR)/$(USER)/eless-dev/

ELESS_ELISP_DIR="$(shell pwd)/build/"
ORG_FILE=$(shell pwd)/eless.org

# Function to be run in emacs --batch
FUNC=

.PHONY: default help emacs-batch \
	eless html info ghub doc docs all vcheck \
	ctemp clean

default: eless

help:
	@echo "Help for Eless building"
	@echo "====================================================="
	@echo " make eless <-- Build eless bash script"
	@echo " make doc   <-- Build eless documentation"
	@echo " make all   <-- Build eless script + documentation"
	@echo " make help  <-- Show this help"

# Note: The Org file from $(ORG_FILE) is loaded *after* the --eval
# section gets evaluated i.e. --eval '(progn ..)' $(ORG_FILE) If the
# order is reversed i.e. i.e.$(ORG_FILE) --eval '(progn ..)', the act
# of loading the $(ORG_FILE) file first will load the older Org
# version that ships with Emacs and then run the stuff in --eval that
# loads the new Org version.. and thus we'll end up with mixed Org in
# the load-path.
emacs-batch:
	@echo ""
	@echo "$(ORG_FILE) ::"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"ELESS_ELPA\" \"$(ELESS_ELPA)\")\
	(setq-default make-backup-files nil)\
	(load-file (expand-file-name \"setup-eless.el\" \"$(ELESS_ELISP_DIR)\"))\
	)" $(ORG_FILE) \
	-f $(FUNC) \
	--kill

eless:
	@$(MAKE_) emacs-batch FUNC=eless-build-script

html:
	@$(MAKE_) emacs-batch FUNC=eless-build-html-docs

info:
	@$(MAKE_) emacs-batch FUNC=eless-build-info-docs

ghub:
	@$(MAKE_) emacs-batch FUNC=eless-build-github-docs

doc docs: html info ghub

all: vcheck eless doc

vcheck:
ifeq ("$(EMACS_exists)","")
	@curl -fsSkL --retry 9 --retry-delay 9 -O $(EMACS_BIN_SOURCE)/emacs-bin-$(EMACS_BIN_VERSION).tar.gz
	@tar xf emacs-bin-$(EMACS_BIN_VERSION).tar.gz -C /
endif
	@echo "Emacs binary used: $(EMACS)"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"ELESS_ELPA\" \"$(ELESS_ELPA)\")\
	(load-file (expand-file-name \"setup-eless.el\" \"$(ELESS_ELISP_DIR)\"))\
	(message \"[Version check] Emacs %s\" emacs-version)\
	(message \"[Version check] %s\" (org-version nil :full))\
	)" \
	--kill

ctemp:
	@find $(shell pwd)/docs -name "*.*~" -delete

clean: ctemp
	@find ./docs/content -name "*.md" -delete
	@rm -rf $(ELESS_TEST_SITE_DIR)/public $(ELESS_TEST_SITE_DIR)/content-golden
	@rm -rf $(ELESS_ELPA)
	@rm -rf ./docs/public
	@rm -rf /tmp/hugo/bin

# Set a make variable during rule execution
# https://stackoverflow.com/a/1909390/1219634

# Check if an executable exists
# https://stackoverflow.com/a/34756868/1219634
