mapdata <-
function(X){
  dta <- merge(gworld, X, by.x = 'id', by.y = 'iso3')
  dta <- dta[order(dta$order), ]
  return(dta)
}
