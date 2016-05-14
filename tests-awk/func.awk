{  print max($1, max($2, $3)) }

function max(m, n) {
   if (m > n) return m
   else return n
}
