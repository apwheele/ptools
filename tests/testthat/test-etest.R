
test_that("Paper rep 1 [tolerance]", {
  pe <- 0.0884
  res <- ptools::e_test(0,3)
  expect_equal(res, pe, tolerance=0.0005)
})

test_that("Paper rep 2 [rounded]", {
  pe <- 0.1749
  res <- round(ptools::e_test(6,2),4)
  expect_equal(res, pe)
})

test_that("reverse arguments equal", {
  expect_equal(ptools::e_test(6,2), ptools::e_test(2,6))
})

test_that("Not defined 0", {
  expect_equal(ptools::e_test(0,0,silent=TRUE), -1)
})

test_that("Not defined Negative k args", {
  expect_equal(ptools::e_test(3,-2,silent=TRUE), -1)
})

test_that("Not defined Negative n args", {
  expect_equal(ptools::e_test(3,3,1,-1,silent=TRUE), -1)
})

test_that("Not over 1", {
  expect_lte(ptools::e_test(20,20), 1)
})