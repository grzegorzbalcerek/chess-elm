readme:
	cat README.md

compile:
	elm make src/Chess/Game.elm

test:
	npx elm-test

clean:
	rm -rf elm-stuff
	rm main.js
	rm index.html

init:
	elm init
	npm install elm-test
	npx elm-test init
