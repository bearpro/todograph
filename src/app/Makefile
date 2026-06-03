build:
	mkdir -p ./dist
	elm make Main.elm --debug --output=dist/main.js
	cp -f ./static/index.html ./dist/index.html

run-dev: build
	elm-live -d dist -s index.html Main.elm -- --output elm-stuff/elm-live/test.html
