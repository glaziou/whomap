area <-
function(x, y) {
  sum(wrap(x) * y - x * wrap(y)) / 2
}
