test_that("Paper Example G Stats", {
  res3 <- small_samptest(c(3,0,0,0,0,0,0))
  res2 <- small_samptest(c(2,1,0,0,0,0,0))
  res1 <- small_samptest(c(1,1,1,0,0,0,0))
  test_stats <- c(11.68,7.86,5.08)
  res_stats <- c(res3$test_stat,res2$test_stat,res1$test_stat)
  expect_equal(test_stats,round(res_stats,2))
})

# https://andrewpwheeler.com/2017/03/31/using-the-exact-reference-distribution-for-small-sample-benford-tests/
test_that("Benford Example Permutation N", {
  UpProb <- c(0.301029995663981, 0.176091259055681, 0.0579919469776867, 
              0.0511525224473813,0.0457574905606751, 0.367976785294594)
  ZeroAdd <- c(1,1,3,6,2,0)
  resSmall <- small_samptest(d=ZeroAdd,p=UpProb,type="KS")
  n <- sum(ZeroAdd)
  m <- length(ZeroAdd)
  choose_stat <- choose(m+n-1,m-1)
  # Test type is correct
  expect_equal(resSmall$test,'KS')
  expect_equal(dim(resSmall$CDF)[1],choose_stat)
})

test_that("Power Example", {
  r1 <- small_samptest(c(3,0,0,0,0,0,0))
  p1 <- powalt(r1,c(1/2,1/2,0,0,0,0,0))
  r2 <- small_samptest(c(11,0,0,0,0,0,0))
  p2 <- powalt(r2,c(1/4,1/4,1/4,1/4,0,0,0))
  check_pow <- c(0.25,0.58)
  te_pow <- round(c(p1$pow,p2$pow),2)
  expect_equal(te_pow, check_pow)
})

test_that("Reuse CDF", {
  cv <- c(3,0,0,0,0,0,0)
  r1 <- small_samptest(cv, type="V")
  r2 <- small_samptest(cv, type="V", cdf=r1$CDF)
  expect_equal(r1, r2)
})

test_that("Chi/G Equal", {
  cv <- c(3,0,0,0,0,0,0)
  r1 <- small_samptest(cv, type="Chi")
  r2 <- small_samptest(cv, type="G")
  expect_equal(r1$AggregateStatistics[,2:3], r2$AggregateStatistics[,2:3])
})

