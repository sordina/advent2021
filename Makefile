.PHONY: all
all: day1

.PHONY: day1
day1:
	cabal run advent2021 1 < data/day1.input