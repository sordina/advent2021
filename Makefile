all: day1 day1b day2 day2b day3 day3b day4 day4b day5 day5b day6 day6b day7 day7b day8 day8b day9 day9b day10 day10b
.PHONY: all day1 day1b day2 day2b day3 day3b day4 day4b day5 day5b day6 day6b day7 day7b day8 day8b day9 day9b day10 day10b

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

day5:
	cabal run advent2021 5 < data/day5.input
	@echo

day5b:
	cabal run advent2021 5b < data/day5.input
	@echo

day6:
	cabal run advent2021 6 < data/day6.input
	@echo

day6b:
	cabal run advent2021 6b < data/day6.input
	@echo

day7:
	cabal run advent2021 7 < data/day7.input
	@echo

day7b:
	cabal run advent2021 7b < data/day7.input
	@

day8:
	cabal run advent2021 8 < data/day8.input
	@echo

day8b:
	cabal run advent2021 8b < data/day8.input
	@echo

day9:
	cabal run advent2021 9 < data/day9.input
	@echo

day9b:
	cabal run advent2021 9b < data/day9.input
	@echo

day10:
	cabal run advent2021 10 < data/day10.input
	@echo

day10b:
	cabal run advent2021 10b < data/day10.input
	@echo