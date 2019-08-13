all: watch

clean:
	@git clean -fxd

prepare: clean
	@npm install elm elm-test elm-verify-examples

test:
	@npx elm-verify-examples
	@npx elm-test

watch: test
	@npx elm-test --watch

repl:
	@npx elm repl
