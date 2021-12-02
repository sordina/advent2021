.PHONY: all
all: day1 day1b

.PHONY: day1 echo
day1:
	cabal run advent2021 1 < data/day1.input
	@echo

.PHONY: day1b echo
day1b:
	cabal run advent2021 1b < data/day1.input
	@echo