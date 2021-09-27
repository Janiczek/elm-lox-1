.PHONY: run
run: dist/elm.js
	node src/index.mjs example.lox

dist/elm.js: $(shell find src -name '*.elm')
	elm make src/Main.elm --output dist/elm.js
