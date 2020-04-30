ELM_SRC="kon-elm/src"

all: static/main.js

$(ELM_SRC)/Bridge.elm:
	stack test && stack run kon-board-gen-elm $(ELM_SRC)

static/main.js: $(ELM_SRC)/Bridge.elm
	cd kon-elm && elm make src/Main.elm --output=../$@

clean:
	rm -rf $(ELM_SRC)/Bridge.elm static/main.js; stack clean
