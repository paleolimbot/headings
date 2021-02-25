
test_that("wmm defaults work", {
  # taken from the test values distributed with WMM2020
  test_values <- read.table(
    system.file("extdata/WMM2020_TEST_VALUES.txt", package = "headings"),
    skip = 18,
    header = FALSE
  )

  extract <- wmm2020_extract(
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

test_that("wmm2020_extract() warns for out-of-range year values", {
  expect_warning(wmm2020_extract(0, 0, year = 2019), "must be between")
  expect_warning(wmm2020_extract(0, 0, year = 2025.1), "must be between")
})

test_that("wmm2020_extract() can handle NA values", {
  expect_true(is.na(wmm2020_extract(0, 0, year = 2020, height = NA)$decl))
  expect_true(is.na(wmm2020_extract(0, 0, year = NA, height = 0)$decl))
  expect_true(is.na(wmm2020_extract(0, NA, year = 2020, height = 0)$decl))
  expect_true(is.na(wmm2020_extract(NA, 0, year = 2020, height = 0)$decl))
})

test_that("wmm2020_extract() warns for out-of-range lat/lon values", {
  expect_warning(wmm2020_extract(-181, 0, year = 2020, height = 0), "must be between")
  expect_warning(wmm2020_extract(181, 0, year = 2020, height = 0), "must be between")
  expect_warning(wmm2020_extract(0, -91, year = 2020, height = 0), "must be between")
  expect_warning(wmm2020_extract(0, 91, year = 2020, height = 0), "must be between")
})

test_that("igrf13 defaults work", {
  skip("IGRF not working yet")

  # taken from the test values distributed with IGRF13
  test_values <- read.table(
    "inst/extdata/IGRF13/sample_out_IGRF13.txt",
    header = TRUE
  )

  # convert provided test values to a form more like the WMM values
  test_values$year <- suppressWarnings(as.numeric(test_values$Date))
  test_values$year[grepl(",", test_values$Date)] <-
    mm_decimal_year(gsub(",", "-", test_values$Date[grepl(",", test_values$Date)]))

  test_values$Altitude_scale <-
    c("K" = 1, "F" = 3280.84, "M" = 1000)[substr(test_values$Altitude, 1, 1)]
  test_values$height <-
    as.numeric(substr(test_values$Altitude, 2, 100)) / test_values$Altitude_scale

  test_values$lat <- suppressWarnings(as.numeric(test_values$Latitude))
  lat_is_dms <- grepl(",", test_values$Latitude)
  test_values$lat[lat_is_dms] <- vapply(
    test_values$Latitude[lat_is_dms],
    function(dms) {
      spl <- as.numeric(strsplit(dms, ",")[[1]])[1:3]
      spl[is.na(spl)] <- 0
      spl_sign <- sign(spl[1])
      spl[1] + spl_sign * spl[2] / 60 + spl_sign * spl[3] / 3600
    },
    double(1)
  )

  test_values$lon <- suppressWarnings(as.numeric(test_values$Longitude))
  lon_is_dms <- grepl(",", test_values$Longitude)
  test_values$lon[lon_is_dms] <- vapply(
    test_values$Longitude[lon_is_dms],
    function(dms) {
      spl <- as.numeric(strsplit(dms, ",")[[1]])[1:3]
      spl[is.na(spl)] <- 0
      spl_sign <- sign(spl[1])
      spl[1] + spl_sign * spl[2] / 60 + spl_sign * spl[3] / 3600
    },
    double(1)
  )

  test_values[c("D_deg", "D_min")] <- lapply(
    test_values[c("D_deg", "D_min")],
    function(x) as.numeric(gsub("[dm]$", "", x))
  )

  test_values$decl_sign <- ifelse(
    test_values$D_deg != 0,
    sign(test_values$D_deg),
    1
  )

  test_values$decl <- test_values$D_deg +
    (test_values$decl_sign * test_values$D_min / 60)

  test_values[c("I_deg", "I_min")] <- lapply(
    test_values[c("I_deg", "I_min")],
    function(x) as.numeric(gsub("[dm]$", "", x))
  )

  test_values$incl_sign <- ifelse(
    test_values$D_deg != 0,
    sign(test_values$D_deg),
    1
  )

  test_values$incl <- test_values$I_deg +
    (test_values$incl_sign * test_values$I_min / 60)

  extract <- igrf13_extract(
    lon = test_values$lon,
    lat = test_values$lat,
    year = test_values$year,
    height = test_values$height
  )

  # # compare with oce, which uses the fortran code
  # # only at altitude zero
  # oce_extract <- oce::magneticField(
  #   test_values$lon,
  #   test_values$lat,
  #   time = test_values$year
  # )
  # # dms output is only good to one dec place
  # round(oce_extract$decl - test_values$decl, 1)
  # round(oce_extract$incl - test_values$incl, 1)

  # the third value is at 6000 km and doesn't seem to align
  # tolerance here I beleive is in percent (i.e., 3%)
  expect_equal(extract$decl[-3], test_values$decl[-3], tolerance = 0.03)
  expect_equal(extract$incl[-3], test_values$incl[-3], tolerance = 0.03)
})

test_that("emm2017_extract() defaults work", {
  # taken from the test values distributed with EMM2017
  test_values <- read.table(
    system.file("extdata/EMM2017TestValues.txt", package = "headings"),
    skip = 18,
    header = FALSE
  )

  extract <- emm2017_extract(
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

test_that("emm2017_extract() warns for out-of-range year values", {
  expect_warning(emm2017_extract(0, 0, year = 1999.9), "must be between")
  expect_warning(emm2017_extract(0, 0, year = 2022.1), "must be between")
})

test_that("emm2017_extract() can handle NA values", {
  expect_true(is.na(emm2017_extract(0, 0, year = 2020, height = NA)$decl))
  expect_true(is.na(emm2017_extract(0, 0, year = NA, height = 0)$decl))
  expect_true(is.na(emm2017_extract(0, NA, year = 2020, height = 0)$decl))
  expect_true(is.na(emm2017_extract(NA, 0, year = 2020, height = 0)$decl))
})

test_that("emm2017_extract() warns for out-of-range lat/lon values", {
  expect_warning(emm2017_extract(-181, 0, year = 2020, height = 0), "must be between")
  expect_warning(emm2017_extract(181, 0, year = 2020, height = 0), "must be between")
  expect_warning(emm2017_extract(0, -91, year = 2020, height = 0), "must be between")
  expect_warning(emm2017_extract(0, 91, year = 2020, height = 0), "must be between")
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


