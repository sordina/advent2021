.PHONY: all
all: day1 day1b day2 day2b

.PHONY: day1
day1:
	cabal run advent2021 1 < data/day1.input
	@echo

.PHONY: day1b
day1b:
	cabal run advent2021 1b < data/day1.input
	@echo

.PHONY: day2
day2:
	cabal run advent2021 2 < data/day2.input
	@echo

.PHONY: day2b
day2:
	cabal run advent2021 2b < data/day2.input
	@echo