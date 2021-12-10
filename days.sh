#!/bin/sh

ls 'src/' | grep Advent | gsed -E 's/Advent(..)\.hs/day\1 day\1b/; s/0([0-9])/\1/g' | grep day | xargs echo
