
objects := $(shell ls src/ | sed 's/hs/o/' | grep Advent | grep -v b | xargs echo)

.PHONY: all
all: build
	cabal exec -- make every

.PHONY: build
build:
	cabal build

.PHONY: run
run:
	@[ "$(day)" ] || ( echo "Usage: make run day=NN" && exit 1 )
	cabal build
	@echo
	cabal exec advent2021 $(day) < "data/day$(day).input"
	@echo
	cabal exec advent2021 $(day)b < "data/day$(day).input"
	@echo

.PHONY: every
every: $(objects)

.PHONY: doctest-interactive
doctest-interactive:
	find {src,*.cabal} | entr -- cabal exec -- doctest -fdefer-typed-holes -isrc src/*.hs

.PHONY: %.o
%.o: src/%.hs
	$(eval x = $(shell echo $< | sed -E 's/src.//; s/Advent(..)\.hs/\1/; s/0([0-9])/\1/g'))
	@echo
	@echo make $@
	@echo
	advent2021 $(x)  < "data/day$(x).input"
	@echo
	advent2021 $(x)b < "data/day$(x).input"
	@echo
