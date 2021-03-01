
test_that("hdg_density() works", {
  dens_180 <- hdg_density(180, bw = 5)
  dens_180_reg <- stats::density(180, bw = 5, from = 0, to = 360 - (1 / 512))
  expect_true(all(abs(dens_180$y- dens_180_reg$y) < 1e-5))

  dens_180 <- hdg_density(180, bw = 5, weights = 0.5)
  dens_180_reg <- stats::density(180, weights = 1, bw = 5, from = 0, to = 360 - (1 / 512))
  expect_true(all(abs(dens_180$y- dens_180_reg$y) < 1e-5))

  # check more than one value
  dens_180 <- hdg_density(c(181, 180, 179), bw = 5)
  dens_180_reg <- stats::density(c(181, 180, 179), bw = 5, from = 0, to = 360 - (1 / 512))
  expect_true(all(abs(dens_180$y- dens_180_reg$y) < 1e-3))

  # check na.rm
  dens_180 <- hdg_density(c(181, 180, 179), bw = 5)
  dens_180_reg <- hdg_density(c(181, 180, 179, NA), bw = 5, na.rm = TRUE)
  expect_identical(dens_180$y, dens_180_reg$y)
})

test_that("plot function for hdg_density() works", {
  dens_180 <- hdg_density(180, bw = 5)
  expect_identical(plot(dens_180), dens_180)
})

test_that("hdg_plot() works", {
  expect_identical(hdg_plot(180), 180)
})
