STATIC := ../static
ELM_SRC_FILES = $(shell find $(ELM_SRC) -type f -name '*.elm' 2>/dev/null)

$(STATIC)/main.js: $(ELM_SRC_FILES)
	elm make src/Main.elm --yes --warn --output $@
