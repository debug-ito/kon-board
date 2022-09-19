ELM_SRC="kon-elm/src"
VERSION := $(shell perl -nle 'if(/^\s*version:\s*(\S+)/) { print $$1 }' < kon-board/kon-board.cabal )

all: backend frontend

backend:
	cabal v2-build kon-board-server

frontend: static/main-$(VERSION).js static/index.html

$(ELM_SRC)/Bridge.elm:
	cabal v2-run kon-board-gen-elm $(ELM_SRC)

static/main-$(VERSION).js: $(ELM_SRC)/Bridge.elm
	cd kon-elm && elm make src/Main.elm --output=../$@

static/index.html: static/index.html.template
	perl -nle 's/\$$\{VERSION\}/$(VERSION)/g; print' < $^ > $@

test: $(ELM_SRC)/Bridge.elm
	cabal v2-test --job=1 all && ( cd kon-elm && npx elm-test )

run: static/main-$(VERSION).js static/index.html
	cabal v2-run kon-board-server

clean:
	rm -rf $(ELM_SRC)/Bridge.elm static/main*.js static/index.html; cabal v2-clean
