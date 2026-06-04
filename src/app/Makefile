build:
	mkdir -p ./dist
	rm -f ./dist/*
	elm make Main.elm --debug --output=dist/main.js
	cp -f ./static/index.html ./dist/index.html

run-dev: build
	elm-live -d dist -s index.html Main.elm -- --output dist/main.js
