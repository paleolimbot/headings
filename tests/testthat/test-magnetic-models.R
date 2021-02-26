
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
  # taken from the test values distributed with IGRF13
  test_values <- read.table(
    system.file("extdata/IGRF13/sample_out_IGRF13.txt", package = "headings"),
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

  # test against the supplied file
  extract <- igrf13_extract(
    lon = test_values$lon,
    lat = test_values$lat,
    year = test_values$year,
    height = test_values$height
  )

  # the third value is at 6000 km and doesn't seem to align
  # tolerance here I beleive is in percent (i.e., 0.03%)
  expect_equal(extract$decl[-3], test_values$decl[-3], tolerance = 0.0003)
  expect_equal(extract$incl[-3], test_values$incl[-3], tolerance = 0.005)

  # compare with oce, which uses the fortran code only at altitude zero
  long_term_coords <- expand.grid(
    year = c(1905, 1945, 1995, 2025),
    lon = c(-90, 0, 90),
    lat = c(-45, 0, 45)
  )

  # oce_extract <- oce::magneticField(
  #   long_term_coords$lon,
  #   long_term_coords$lat,
  #   time = long_term_coords$year
  # )

  # dput(round(oce_extract$declination, 3))
  # dput(round(oce_extract$inclination, 3))
  oce_extract_decl <- c(
    23.421, 23.471, 22.342, 20.853, -27.231, -26.646, -22.814,
    -21.5, -30.25, -39.443, -43.767, -40.749, 8.11, 9.191, 5.539,
    2.34, -16.751, -13.616, -7.95, -3.814, -1.377, -2.949, -2.558,
    -1.819, 4.502, 3.279, -0.529, -2.664, -14.871, -8.899, -2.94,
    1.391, 5.381, 3.192, 2.888, 1.81
  )
  oce_extract_incl <- c(
    -50.833, -48.646, -47.823, -47.668, -55.738, -59.83, -63.224,
    -63.168, -71.468, -72.06, -73.548, -74.667, 13.057, 18.072, 20.183,
    19.415, -12.044, -20.665, -27.675, -30.339, -20.156, -22.3, -20.704,
    -17.314, 74.944, 75.145, 73.064, 70.918, 62.043, 61.561, 60.578,
    60.504, 62.908, 64.373, 64.644, 66.392
  )

  extract0 <- igrf13_extract(
    long_term_coords$lon,
    long_term_coords$lat,
    year = long_term_coords$year,
    height = 0
  )

  expect_identical(round(extract0$decl, 3), oce_extract_decl)
  expect_identical(round(extract0$incl, 3), oce_extract_incl)
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
