
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

test_that("wmm_version() works", {
  expect_match(wmm_version(), "2019-12-10")
})


