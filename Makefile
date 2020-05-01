ELM_SRC="kon-elm/src"

all: static/main.js

$(ELM_SRC)/Bridge.elm:
	stack run kon-board-gen-elm $(ELM_SRC)

static/main.js: $(ELM_SRC)/Bridge.elm
	cd kon-elm && elm make src/Main.elm --output=../$@

test: $(ELM_SRC)/Bridge.elm
	stack test && ( cd kon-elm && elm-test )

clean:
	rm -rf $(ELM_SRC)/Bridge.elm static/main.js; stack clean
