.PHONY: clean

default: run

build:
	spago build

assets/index.js: build
	spago bundle-app --main Main --to assets/index.js

run: assets/index.js assets/index.html
	parcel serve assets/index.html -o bundle/index.html --open

clean:
	rm -rf bundle
	rm assets/index.js
