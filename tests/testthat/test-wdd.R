test_that("Local -10", {
  res <- ptools::wdd(c(20,20),c(20,10), silent=TRUE)
  expect_equal(res[['Est_Local']], -10)
})

test_that("Displacement -10", {
  res <- ptools::wdd(c(20,20),c(20,20),c(20,20),c(20,10), silent=TRUE)
  expect_equal(res[['Est_Displace']], -10)
})

test_that("Total -20", {
  res <- ptools::wdd(c(20,20),c(20,10),c(20,20),c(20,10), silent=TRUE)
  expect_equal(res[['Est_Total']], -20)
})

test_that("Est & Variance Paper", {
  res <- ptools::wdd(c(308,318),c(207,110),c(150,140),c(178,157), 
         alpha=0.05, silent=TRUE)
  paper_res <- c(-118,sqrt(1568),-2.98,-196,-40)
  sub_res <- res[5:9]
  sub_res['Z'] <- round(sub_res['Z'],2)
  sub_res['LowCI'] <- round(sub_res['LowCI'])
  sub_res['HighCI'] <- round(sub_res['HighCI'])
  names(paper_res) <- names(sub_res)
  expect_equal(sub_res, paper_res)
})

# https://andrewpwheeler.com/2021/01/09/the-wdd-test-with-different-pre-post-time-periods/
test_that("Time Weights", {
  res <- ptools::wdd(c(100,50),c(80,20),time_weights=c(2,1), 
         alpha=0.05, silent=TRUE)
  bp_res <- c(-20,sqrt(115))
  sub_res <- res[5:6]
  names(bp_res) <- names(sub_res)
  expect_equal(sub_res,bp_res)
})

# https://andrewpwheeler.com/2021/02/23/the-wdd-test-with-different-area-sizes/
test_that("Area Weights", {
  res <- ptools::wdd(c(308,318),c(207,110),c(150,140),c(178,157), 
         area_weights=c(0.9,1.2,1.6,1.5),
         alpha=0.05, silent=TRUE)
  bp_res <- c(-99.69444444,35.4280755,-2.813995483,-169.1321965,-30.25669241)
  sub_res <- res[5:9]
  names(bp_res) <- names(sub_res)
  expect_equal(sub_res,bp_res)
})

# https://andrewpwheeler.com/2020/11/19/amending-the-wdd-test-to-incorporate-harm-weights/
test_that("Cumulative Harm", {
  mv <- ptools::wdd(c(133,91),c(130,74), silent=TRUE)
  th <- ptools::wdd(c(388,305),c(327,202), silent=TRUE)
  bu <- ptools::wdd(c(148,97),c(398,190), silent=TRUE)
  rob <- ptools::wdd(c(86,64),c(144,92), silent=TRUE)
  ass <- ptools::wdd(c(94,67),c(183,96), silent=TRUE)
  tot <- data.frame(rbind(mv,th,bu,rob,ass))
  weight <- c(3,2,5,7,10)
  res <- ptools::wdd_harm(tot$Est_Local,tot$SE_Local,weight,alpha=0.05,silent=TRUE)
  bp_res <- c(-1721.00000000,304.10360077,-5.65925558,-2317.03210507,-1124.96789493)
  names(bp_res) <- names(res)
  expect_equal(res,bp_res,tolerance=1e-5)
})

# ToDo, a simple test for both time and area weights
# Simple test for no post values

