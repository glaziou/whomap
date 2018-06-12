centroid <-
function (x1, y1) {
  n <- length(x1)
  wrap <- c(n, 1:(n - 1))
  x2 <- x1[wrap]
  y2 <- y1[wrap]
  a <- x1 * y2 - x2 * y1
  s <- sum(a) * 3
  if (s < 1e-3) {
    c(mean(x1), mean(y1))
  } else {
    c(sum((x1 + x2) * a)/s, sum((y1 + y2) * a)/s)
  }
}
