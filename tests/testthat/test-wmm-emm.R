
test_that("wmm defaults work", {
  # taken from the test values distributed with WMM2020
  test_values <- read.table(
    system.file("extdata/WMM2020_TEST_VALUES.txt", package = "headings"),
    skip = 18,
    header = FALSE
  )

  extract <- wmm_extract(
    lon = test_values$V4,
    lat = test_values$V3,
    year = test_values$V1,
    height = test_values$V2
  )

  expect_identical(
    round(extract$decl, 2),
    test_values$V5
  )
})

test_that("wmm_extract() warns for out-of-range year values", {
  expect_warning(wmm_extract(0, 0, year = 2019), "must be between")
  expect_warning(wmm_extract(0, 0, year = 2025.1), "must be between")
})

test_that("wmm_extract() can handle NA values", {
  expect_true(is.na(wmm_extract(0, 0, year = 2020, height = NA)$decl))
  expect_true(is.na(wmm_extract(0, 0, year = NA, height = 0)$decl))
  expect_true(is.na(wmm_extract(0, NA, year = 2020, height = 0)$decl))
  expect_true(is.na(wmm_extract(NA, 0, year = 2020, height = 0)$decl))
})

test_that("wmm_extract() warns for out-of-range lat/lon values", {
  expect_warning(wmm_extract(-181, 0, year = 2020, height = 0), "must be between")
  expect_warning(wmm_extract(181, 0, year = 2020, height = 0), "must be between")
  expect_warning(wmm_extract(0, -91, year = 2020, height = 0), "must be between")
  expect_warning(wmm_extract(0, 91, year = 2020, height = 0), "must be between")
})

test_that("emm_extract() defaults work", {
  # taken from the test values distributed with EMM2017
  test_values <- read.table(
    system.file("extdata/EMM2017TestValues.txt", package = "headings"),
    skip = 18,
    header = FALSE
  )

  extract <- emm_extract(
    lon = test_values$V4,
    lat = test_values$V3,
    year = test_values$V1,
    height = test_values$V2
  )

  expect_identical(
    round(extract$decl, 2),
    test_values$V5
  )
})


test_that("mm_ellipsoidal_height() works", {
  expect_true(abs(mm_ellipsoidal_height(0, 0, 0) - 0) < 0.02)
  expect_equal(
    mm_ellipsoidal_height(0, 0, 10) - 10,
    mm_ellipsoidal_height(0, 0, 0) - 0
  )

  expect_warning(mm_ellipsoidal_height(181, 0, height = 0), "must be between")
  expect_warning(mm_ellipsoidal_height(0, -91, height = 0), "must be between")
  expect_true(is.na(mm_ellipsoidal_height(0, NA, height = 0)))
  expect_true(is.na(mm_ellipsoidal_height(NA, 0, height = 0)))
  expect_true(is.na(mm_ellipsoidal_height(0, 0, height = NA)))
})

test_that("mm_version() works", {
  expect_match(mm_version(), "2019-12-10")
})


