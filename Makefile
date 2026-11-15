CABAL_OPTS = --hyperlink-source

CABAL_FILE = prefix-units.cabal
# The package version, and the tag named by `source-repository this`.
VERSION := $(shell awk 'tolower($$1) == "version:" { print $$2; exit }' $(CABAL_FILE))
REPO_TAG := $(shell awk '/^[^[:space:]]/ { this = (tolower($$1) == "source-repository" && $$2 == "this") } this && tolower($$1) == "tag:" { print $$2 }' $(CABAL_FILE))

build:
	cabal build

int-docs:
	cabal haddock \
	  $(CABAL_OPTS) \
	  --internal

haddock:
	cabal haddock \
	  $(CABAL_OPTS)

configure:
	cabal clean
	cabal configure --enable-tests --enable-coverage

test:
	cabal build
	cabal test --test-show-details=direct

dist:
	cabal sdist

lint:
	@rm -f report.html
	hlint \
	  --report -c Data tests

# Prints the CHANGES.md section for $(VERSION), without its heading.
# Fails if the section is missing or empty.
release-notes:
	@awk -v ver="$(VERSION)" ' \
	  $$1 == "##" { if (found) exit; found = (NF == 3 && $$2 == "Version" && $$3 == ver); next } \
	  found { print; if (NF) content = 1 } \
	  END { if (!content) { print "CHANGES.md has no entry for version " ver > "/dev/stderr"; exit 1 } } \
	' CHANGES.md

# Checks that the cabal file and CHANGES.md are ready for releasing
# $(VERSION).
check-release:
	@test -n "$(VERSION)" || \
	  { echo "No version found in $(CABAL_FILE)." >&2; exit 1; }
	@test "$(REPO_TAG)" = "v$(VERSION)" || \
	  { echo "source-repository this names tag '$(REPO_TAG)', expected 'v$(VERSION)'." >&2; exit 1; }
	@$(MAKE) -s --no-print-directory release-notes > /dev/null
	cabal check

# Checks that TAG is the tag for the version in the cabal file, then
# runs check-release. Used by the release workflow.
check-tag:
	@test -n "$(TAG)" || \
	  { echo "Usage: make check-tag TAG=vX.Y.Z" >&2; exit 1; }
	@test "$(TAG)" = "v$(VERSION)" || \
	  { echo "Tag $(TAG) does not match cabal version $(VERSION)." >&2; exit 1; }
	@$(MAKE) --no-print-directory check-release

# Creates the signed tag for the version in the cabal file.
release-tag: check-release
	@git diff --quiet HEAD || \
	  { echo "Commit or stash local changes before tagging." >&2; exit 1; }
	@! git rev-parse -q --verify "refs/tags/v$(VERSION)" > /dev/null || \
	  { echo "Tag v$(VERSION) already exists." >&2; exit 1; }
	git tag -s -m 'Release version $(VERSION)' v$(VERSION)

clean:
	cabal clean
	rm -f report.html

.PHONY: build int-docs haddock configure test dist lint clean \
	release-notes check-release check-tag release-tag
