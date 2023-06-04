#!/bin/bash

function timeout_child {
    trap -- "" SIGTERM
    child=$!
    timeout=$1
    (
            sleep $timeout
            kill $child 2>/dev/null
    ) &
    wait $child
}

cat <<EOF
<style>
body { min-width: 100%; max-width: 100% }
.typst { width: 50%; min-width: 50%; }
.html { width: 50%; min-width: 50%; }
</style>
EOF


for s in $(find test/typ -type d)
do
  echo "## $s"
  for t in $s/*.typ
  do
    grep -q 'Error:' $t && continue
    >&2 echo $t
    echo "### $t"
    echo ""
    echo ":::::: columns"
    echo "::: {.column .typst}"
    echo '````````````'
    echo "#let test = (x,y) => { if x == y [✅] else [❌(#repr(x) /= #repr(y))] }" > $t.rev
    cat $t >> $t.rev
    cat $t
    echo '````````````'
    echo ":::"
    echo "::: {.column .html}"
    echo '````````````{=html}'
    (cabal run typst-hs -fexecutable --disable-optimization -- --html $t.rev 2>&1) & timeout_child 2
    echo '````````````'
    echo ":::"
    echo "::::::"
    echo ""
  done
done
