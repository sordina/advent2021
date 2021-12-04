all: day1 day1b day2 day2b day3 day3b day4 day4b
.PHONY: all day1 day1b day2 day2b day3 day3b day4 day4b

day1:
	cabal run advent2021 1 < data/day1.input
	@echo

day1b:
	cabal run advent2021 1b < data/day1.input
	@echo

day2:
	cabal run advent2021 2 < data/day2.input
	@echo

day2b:
	cabal run advent2021 2b < data/day2.input
	@echo

day3:
	cabal run advent2021 3 < data/day3.input
	@echo

day3b:
	cabal run advent2021 3b < data/day3.input
	@echo

day4:
	cabal run advent2021 4 < data/day4.input
	@echo

day4b:
	cabal run advent2021 4b < data/day4.input
	@echo