test_that("the mu value remains consistent", {
  expect_equal(myncurve(2, 3, 4)$mu, 2)
  expect_equal(myncurve(-4, 10, -7)$mu, -4)
  expect_equal(myncurve(8, 0.5, 4)$mu, 8)
})

test_that("the sigma value remains consistent and is valid", {
  expect_equal(myncurve(2, 3, 4)$sigma, 3)
  expect_equal(myncurve(-4, 10, -7)$sigma, 10)
  expect_equal(myncurve(8, 0.5, 4)$sigma, 0.5)
  expect_error(myncurve(3, 0, 3), "sigma")
  expect_error(myncurve(-2, -4, 4), "sigma")
})

test_that("the area is calculated correctly", {
  expect_equal(myncurve(2, 3, 4)$area, round(pnorm(4, 2, 3), 4))
  expect_equal(myncurve(-4, 10, -7)$area, round(pnorm(-7, -4, 10), 4))
  expect_equal(myncurve(8, 0.5, 4)$area, round(pnorm(4, 8, 0.5), 4))
})
