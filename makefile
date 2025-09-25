.PHONY: clean clean/* test test/* docs docs/*

# ============================================================================ #
#  BUILD
# ============================================================================ #
build: build/cpp build/hs

build/cpp:
	cd cpp && $(MAKE) build

build/hs:
	cd hs && cabal build

# ============================================================================ #
#  CLEAN
# ============================================================================ #
clean: clean/cpp clean/hs
	rm -rf docs

clean/cpp:
	cd cpp && $(MAKE) clean

clean/hs:
	cd hs && cabal clean

# ============================================================================ #
#  DOCUMENTATION
# ============================================================================ #
docs: docs/cpp docs/hs

docs/cpp:
	cd cpp && $(MAKE) docs
	mkdir -p docs
	cp -r cpp/build/docs/html docs/cpp

docs/hs:
	cd hs && cabal haddock
	mkdir -p docs
	cp -r hs/dist-newstyle/build/*/*/Anatree-*/doc/html/Anatree docs/hs

# ============================================================================ #
#  TEST
# ============================================================================ #
test: test/cpp test/hs

test/cpp:
	cd cpp && $(MAKE) tests

test/hs:
	cd hs && cabal test

