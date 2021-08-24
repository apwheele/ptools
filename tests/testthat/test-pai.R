
test_that("simple pai check", {
  crime_dat <- data.frame(id=1:3,
                          obs=c(6,3,0),
                          pred=c(3,2,1))
  crime_dat$const <- 1
  p1 <- ptools::pai(crime_dat,'obs','pred','const')
  # PEI is perfect
  pei_check <- c(1,1,1)
  expect_equal(p1$PEI,pei_check)
  # PAI is 2, 1.5, 1
  pai_check <- c(2,1.5,1)
  expect_equal(p1$PAI,pai_check)
  # RRI is simple as well
  rri_check <- cumsum(crime_dat$pred)/cumsum(crime_dat$obs)
  expect_equal(p1$RRI,rri_check)
  # Dimension check 1
  expect_equal(dim(p1),c(nrow(crime_dat),12))
  # Dimension check 2
  ot_var <- 'id'
  p2 <- ptools::pai(crime_dat,'obs','pred','const',ot_var)
  expect_equal(dim(p2),c(nrow(crime_dat),12 + length(ot_var)))
})

test_that("pai_summary equivalence", {
  crime_dat <- data.frame(id=1:6,
                          obs=c(6,7,3,2,1,0),
                          pred=c(8,4,4,2,1,0))
  crime_dat$const <- 1
  p1 <- ptools::pai(crime_dat,'obs','pred','const')
  p2 <- p1
  pl <- list(p1,p2); labs <- c('one','two')
  th <- c(1,3,5)
  res <- ptools::pai_summary(pl,th,labs)
  # dim wide should be (3,11)
  expect_equal(dim(res),c(length(th),1 + 5*length(pl)))
  # should be equal between the two results
  expect_equal(res$PAI.one,res$PAI.two)
  # Long format check
  res_long <- ptools::pai_summary(pl,th,labs, wide=FALSE)
  # Dimension check again
  expect_equal(dim(res_long),c(length(th)*length(pl),7))
  # Long equality check
  l1 <- res_long[res_long$lab == labs[1],2:7]
  l2 <- res_long[res_long$lab == labs[2],2:7]
  row.names(l2) <- 1:length(th); row.names(l1) <- 1:length(th)
  expect_equal(l1,l2)
})


# Other checks? 
# Zero errors for RRI?