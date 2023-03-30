#!/bin/sh
rm -f samples/*.out
echo "Test log:" > test.log
for x in samples/*
do
    echo "\n\n_____ $x _____" >> test.log
    cat $x >> test.log
    echo >> test.log
    cat $x > $x.out
    echo "" > $x.out
    echo "." > $x.out
    cabal run <$x >$x.out 2>> test.log
done

ERRORS=$(rg -c '^"input"' test.log)
if [ "x$ERRORS" == "x" ]
then
  echo "All good"
else
  echo "$ERRORS errors"
fi
