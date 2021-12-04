.PHONY: all
all: day1 day1b day2 day2b day3 day3b day4 day4b

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

.PHONY: day3
day3:
	cabal run advent2021 3 < data/day3.input
	@echo

.PHONY: day3b
day3b:
	cabal run advent2021 3b < data/day3.input
	@echo

.PHONY: day4
day4:
	cabal run advent2021 4 < data/day4.input
	@echo

.PHONY: day4b
day4b:
	cabal run advent2021 4b < data/day4.input
	@echo