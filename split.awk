/^---$/ {
    if ( out ) {
        close(out)
        file = FILENAME
        sub(/\..*/, "", file)
        if (lines) {
          cnt++
          lines = 0
        }
        out = 0
    }
    next
}
{ if (!out) {
    file = FILENAME
    sub(/\..*/, "", file)
    out = sprintf("%s-%02d.typ", file, cnt)
    lines = 0
  }
  if ($0 ~ /^$/ || $0 ~ /^---$/ || $0 ~ /^\/\//) { } else { lines++ }
  print > out
}
