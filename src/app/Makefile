build:
	mkdir -p ./dist
	rm -rf ./dist/*
	elm make src/Main.elm --debug --output=dist/main.js
	cp -R ./static/. ./dist/

build-prod:
	mkdir -p ./dist
	rm -rf ./dist/*
	elm make src/Main.elm --optimize --output=dist/main.js
	cp -R ./static/. ./dist/

run-dev: build
	elm-live -d dist -s index.html --pushstate true src/Main.elm -- --output dist/main.js
