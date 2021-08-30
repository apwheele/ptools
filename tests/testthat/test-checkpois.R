
test_that("Checking check_pois function", {
  cnt <- c(0,1,2)
  res <- check_pois(cnt,0,2,1,silent=TRUE)
  # If over all potential freq
  # This should sum to 100
  expect_equal(sum(res$Prop), 100)
  # If over all potential freq
  # These two sums should be equal
  expect_equal(sum(res$Freq),length(cnt))
  # Checking dimensions
  expect_equal(dim(res),c(length(cnt),7))
  # Checking Poisson density
  expect_equal(res$PoisD/100,dpois(res$Int,1))
})
