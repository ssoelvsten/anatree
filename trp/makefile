.PHONY: test lib

TROUPE_DIR = $$TROUPE

TROUPEC  = $(TROUPE_DIR)/bin/troupec
LOCAL_SH = $(TROUPE_DIR)/local.sh

lib:
	$(TROUPEC) lib/pi.trp -m
	$(TROUPEC) lib/Id.trp -m

build:
	$(TROUPEC) src/Anatree.trp -m --include=./lib/

install:
	@echo "TODO: Implement installation to '/bin/trp/' ?"

clean:
	rm -rf src/out
	rm -rf out

test: test/Unit

test/Unit: build
	$(LOCAL_SH) ./test/Unit.trp --include=./src/ --include=./lib/

test/Unit.debug:
	$(LOCAL_SH) ./test/Unit.trp --include=./src/ --include=./lib -v
