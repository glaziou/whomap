mapdata <-
function(X, world){
  dta <- merge(world, X, by.x = 'id', by.y = 'iso3', all.x = TRUE)
  dta <- dta[order(dta$order), ]
  return(dta)
}
