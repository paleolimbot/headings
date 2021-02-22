
test_that("hdg normalizing works", {
  expect_equal(
    hdg_norm(c(-10, 0:359, 370)),
    c(350, 0:359, 10)
  )
})

test_that("uv normalizing works", {
  uv <- tibble::tibble(u = 0:1, v = 0:1)
  expect_equal(
    uv_norm(uv),
    tibble::tibble(u = c(NA, 1 / sqrt(2)), v = c(NA, 1 / sqrt(2)))
  )
})

test_that("uv--hdg conversion works", {
  hdgs <- 0:360

  expect_equal(
    hdg_from_uv(uv_from_hdg(hdgs)),
    c(hdgs[-length(hdgs)], 0)
  )

  expect_equal(rad_from_hdg(0), pi / 2)
  expect_equal(rad_from_hdg(90), 0)
  expect_equal(rad_from_hdg(180), -pi / 2)
  expect_equal(rad_from_hdg(270), -pi)
})

test_that("uv--radian conversion works", {
  hdgs <- 0:360

  expect_equal(
    hdg_from_rad(rad_from_hdg(hdgs)),
    c(hdgs[-length(hdgs)], 0)
  )

  expect_equal(uv_from_hdg(0), uv(0, 1))
  expect_equal(uv_from_hdg(90), uv(1, 0))
  expect_equal(uv_from_hdg(180), uv(0, -1))
  expect_equal(uv_from_hdg(270), uv(-1, 0))
})

test_that("hdg diff works", {
  expect_equal(hdg_diff(-179:179, 0), -179:179)
  expect_equal(hdg_diff(-179:179 + 180, 180), -179:179)
})

test_that("mean and sd of hdgs works", {
  expect_equal(hdg_mean(0:10), 5)
  expect_equal(hdg_mean(-5:5), 0)
  expect_equal(hdg_mean(c(350, 10)), 0)
  expect_equal(hdg_sd(-5:5), sd(0:10))

  expect_identical(hdg_mean(c(1, 1, NA), na.rm = FALSE), NA_real_)
  expect_identical(hdg_mean(c(1, 1, NA), na.rm = TRUE), 1)
  expect_identical(hdg_sd(c(1, 1, NA), na.rm = FALSE), NA_real_)
  expect_identical(hdg_sd(c(1, 1, NA), na.rm = TRUE), 0)
})

test_that("weighted mean of headings works", {
  expect_equal(
    hdg_mean(0:5, weights = c(0, 0, 10, 0, 0, 0)),
    2
  )
})

test_that("weighted sd of headings works", {
  expect_false(hdg_sd(0:5, weights = c(10, 0, 0, 0, 0, 10)) == hdg_sd(0:5))
})

test_that("sanitizers work", {
  expect_identical(as_uv(uv(1, 2)), uv(1, 2))
  expect_error(as_uv(1), "Can't convert")
  expect_identical(as_hdg(1), 1)
  expect_error(as_hdg("not a number"), "Can't convert")
})
