info <-
function(df) {
  info <- c(centroid(df$long, df$lat), area(df$lat, df$long))
  names(info) <- c("long", "lat", "area")
  info
}
