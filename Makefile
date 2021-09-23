.PHONY: run
run: dist/elm.js
	node src/index.mjs example.lox

dist/elm.js: src/Main.elm
	elm make src/Main.elm --output dist/elm.js
