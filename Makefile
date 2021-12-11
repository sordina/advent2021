
objects := $(shell ls src/ | sed 's/hs/o/' | grep Advent | grep -v b | xargs echo)

.PHONY: all
all: $(objects)

%.o: src/%.hs
	$(eval x = $(shell echo $< | sed -E 's/src.//; s/Advent(..)\.hs/\1/; s/0([0-9])/\1/g'))
	@echo
	cabal run advent2021 $(x)  < "data/day$(x).input"
	@echo
	cabal run advent2021 $(x)b < "data/day$(x).input"
	@echo
