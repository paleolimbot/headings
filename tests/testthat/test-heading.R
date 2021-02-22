
test_that("heading normalizing works", {
  expect_equal(
    heading_normalize(c(-10, 0:359, 370)),
    c(350, 0:359, 10)
  )
})

test_that("uv--heading conversion works", {
  headings <- 0:360

  expect_equal(
    heading_from_uv(uv_from_heading(headings)),
    c(headings[-length(headings)], 0)
  )

  expect_equal(
    uv_from_heading(0),
    tibble::tibble(u = 0, v = 1)
  )

  expect_equal(
    uv_from_heading(90),
    tibble::tibble(u = 1, v = 0)
  )

  expect_equal(
    uv_from_heading(180),
    tibble::tibble(u = 0, v = -1)
  )

  expect_equal(
    uv_from_heading(270),
    tibble::tibble(u = -1, v = 0)
  )
})

test_that("heading diff works", {
  expect_equal(heading_diff(-179:179, 0), -179:179)
  expect_equal(heading_diff(-179:179 + 180, 180), -179:179)
})

test_that("mean and sd of headings works", {
  expect_equal(heading_mean(0:10), 5)
  expect_equal(heading_mean(-5:5), 0)
  expect_equal(heading_mean(c(350, 10)), 0)
  expect_equal(heading_sd(-5:5), sd(0:10))

  expect_identical(heading_mean(c(1, 1, NA), na.rm = FALSE), NA_real_)
  expect_identical(heading_mean(c(1, 1, NA), na.rm = TRUE), 1)
  expect_identical(heading_sd(c(1, 1, NA), na.rm = FALSE), NA_real_)
  expect_identical(heading_sd(c(1, 1, NA), na.rm = TRUE), 0)
})
