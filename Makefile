build:
	cabal build --disable-optimization -fexecutable
.PHONY: build

test:
	cabal test --disable-optimization --test-options="--hide-successes --ansi-tricks=false $(TESTARGS)"
.PHONY: test

run:
	cabal run typst-hs --disable-optimization -fexecutable
.PHONY: run

pretty-tests.html: build
	cp $@ $@.bkp || echo "No existing file to backup"
	sh test.sh | pandoc --toc --toc-depth=2 --mathml -s | \
       sed -r -e 's/("test\/typ\/[^"]*")/<mark>\1<\/mark>/g' | \
       sed -r -e 's/❌/<mark>❌<\/mark>/g' | \
       sed -r -e 's/terminated/<mark>terminated<\/mark>/g' > $@

regen-tests:
	rm -rf test/typ
	cp -r ../typst/tests/typ test/
	cp -r ../typst/assets test/
	rm -rf test/assets/fonts
	sed -i '' -r -e 's/conifer/green/g; s/forest/red/g' test/typ/*/*.typ
	sed -i '' -r -e 's/(image|xml|read|json|yaml|toml|csv)\("\//\1\("test\/assets\/files\//g' test/typ/*/*.typ
	find test/typ -name '*.typ' \
        ! -path 'test/typ/compiler/module.typ' \
        ! -path 'test/typ/compiler/modules/*' \
        ! -path 'test/typ/compiler/module.typ' \
        -exec bash -c "echo {} && awk -f split.awk {} && rm {}" \;

rm-golden:
	rm -rf test/out

