test_that('whomap() + add_marker() returns a ggplot', {
  p <- whomap() + add_marker('BRA')
  expect_true(is.ggplot(p))
  expect_error(whomap() + add_marker('FRANCE'))
})
