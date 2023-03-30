TESTS=$(find test/typ -name '*.typ')

for TEST in $TESTS
do
  echo "## $TEST"
  echo ""
  echo ":::::: columns"
  echo "::: {.column .typst}"
  echo '````````````'
  cat $TEST
  echo '````````````'
  echo ":::"
  echo "::: {.column .eval}"
  echo '````````````'
  cat ${TEST%.typ}.eval
  echo '````````````'
  echo ":::"
  echo "::: {.column .parse}"
  echo '````````````'
  cat ${TEST%.typ}.parse
  echo '````````````'
  echo ":::"
  echo "::::::"
  echo ""
done
