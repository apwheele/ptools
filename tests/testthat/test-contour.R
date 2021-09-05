
test_that("Test contour results", {
  pre <- 0:15
  post <- pre*1.5
  st <- 5
  le <- c(0,-1,1)
  ma_x <- 30
  res <- pois_contour(pre,post,hr=ma_x,lev=le,steps=st)
  # only three levels
  expect_equal(unique(res$levels),le)
  # number of steps correct
  expect_equal(dim(res),c(st*length(le),3))
  # min/max of X correct
  expect_equal(min(res$x),5)
  expect_equal(max(res$x),ma_x)
  # levels 0 should be exactly 1.5 more
  zer = res[res$levels == 0,]
  expect_equal(zer$x*1.5,zer$y)
})

# Need to work on tests to make sure the variance estimates are correct