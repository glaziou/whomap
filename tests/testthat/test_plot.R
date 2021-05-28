test_that('whomap() returns a ggplot, except when passed a misspecified dataframe', {
  p <- whomap()
  expect_true(is.ggplot(p))
  expect_error(whomap(data.frame(iso3=NA)))
})

test_that('returns the correct number of categories in var when it is a factor with missing values', {
  dta <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'),
                    var=as.factor(c(1,2,3,4,NA)))
  lvl <- length(levels(dta$var))
  p <- whomap(dta)
  lvl2 <- length(unique(p$data$var))
  expect_equal(lvl + 2, lvl2)
})
