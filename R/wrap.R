wrap <-
function(x) {
  n <- length(x)
  x[c(n, seq_len(n - 1))]
}
