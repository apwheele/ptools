
test_that("Area Inverse Operation", {
  area_check <- 1000
  wh <- ptools::hex_dim(area_check^2)
  area <- ptools::hex_area(wh[1]/2)
  expect_equal(area_check,sqrt(area))
})

test_that("Side Inverse Operation", {
  area_check <- 1000
  wh <- ptools::hex_dim(area_check^2)
  wi <- ptools::hex_wd(wh[1])
  expect_equal(wh[2],wi)
})

# Other checks? 