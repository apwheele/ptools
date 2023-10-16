test_that("scan checks", {
  # Taken from https://understandinguncertainty.org/when-cluster-real-cluster
  # Coolserdash's comment
  p1 <- ptools::scanw(208,2,0.6,6)
  r1 <- 0.03902238
  expect_equal(round(p1,3),round(r1,3))
  # Example from description
  p2 <- ptools::scanw(260,1,1,6)
  r2 <- 0.486814
  expect_equal(round(p2,3),round(r2,3))
})