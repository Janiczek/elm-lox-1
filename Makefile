.PHONY: run
run: dist/elm.js test
	node src/index.mjs example.lox

.PHONY: test
test: dist/elm.js
	elm-test-rs

.PHONY: repl
repl: dist/elm.js
	node src/index.mjs

dist/elm.js: $(shell find src -name '*.elm')
	elm make src/Main.elm --output dist/elm.js
