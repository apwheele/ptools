
test_that("test near_string1 (loop version)", {
  n <- 10
  mult <- 3
  dat <- data.frame(x=1:n,y=0,ti=(1:n)*mult,id=1:n)
  # These should be all separate
  res1 <- near_strings1(dat,'id','x','y','ti',1,1)
  expect_equal(max(res1$CompNum), 1)
  # These should be all together
  res2 <- near_strings1(dat,'id','x','y','ti',2,2*mult)
  expect_equal(max(res2$CompId), 1)
  # Should be the same number of rows as original dat
  expect_equal(nrow(dat),nrow(res1))
})

test_that("test near_string2 (kdtree version)", {
  n <- 10
  mult <- 3
  dat <- data.frame(x=1:n,y=0,ti=(1:n)*mult,id=1:n)
  # These should be all separate
  res1 <- near_strings2(dat,'id','x','y','ti',1,1)
  expect_equal(max(res1$CompNum), 1)
  # These should be all together
  res2 <- near_strings2(dat,'id','x','y','ti',2,2*mult)
  expect_equal(max(res2$CompId), 1)
  # Should be the same number of rows as original dat
  expect_equal(nrow(dat),nrow(res1))
})

test_that("two near_string versions should have the same output", {
  n <- 10
  mult <- 3
  dat <- data.frame(x=1:n,y=0,ti=(1:n)*mult,id=1:n)
  res1 <- near_strings1(dat,'id','x','y','ti',1,1)
  res2 <- near_strings2(dat,'id','x','y','ti',1,1)
  expect_equal(res1,res2)
})

test_that("Checking two components", {
  # Two components time clusters
  s <- c(0,0,0,4,4)
  ccheck <- c(1,1,1,2,2)
  dat <- data.frame(x=1:5,y=0,
                    ti=s,
                    id=1:5)
  res1 <- near_strings1(dat,'id','x','y','ti',2,1)
  res2 <- near_strings2(dat,'id','x','y','ti',2,1)
  expect_equal(max(res1$CompId), 2)
  expect_equal(max(res2$CompId), 2)
  expect_equal(res1$CompId, ccheck)
  expect_equal(res2$CompId, ccheck)
  # Two components xy clusters
  dat <- data.frame(x=s,y=s,
                    ti=1:5,
                    id=1:5)
  res1 <- near_strings1(dat,'id','x','y','ti',1,2)
  res2 <- near_strings2(dat,'id','x','y','ti',1,2)
  expect_equal(max(res1$CompId), 2)
  expect_equal(max(res2$CompId), 2)
  expect_equal(res1$CompId, ccheck)
  expect_equal(res2$CompId, ccheck)
})