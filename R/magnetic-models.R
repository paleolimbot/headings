
#' IGRF and NCEI World/Enhanced Magnetic Model Interface
#'
#' @param lon,lat Vectors of coordinates.
#' @param year A decimal year (e.g., 2020.2120). The World Magnetic Model
#'   is valid between 2020.0 and 2025.0; the Enhanced Magnetic Model
#'   is valid between 2000.0 and 2022.0.
#' @param date An R date object or value that can be coerced to one.
#' @param height Height above the WGS84 ellipsoid in kilometers. Use
#'   [mm_ellipsoidal_height()] to convert geoid elevations (EGM9615)
#'   to ellipsoidal values.
#'
#' @return A data frame with columns - `decl`: Magnetic variation in degrees -
#'   `incl`: Inclination of the magnetic field in degrees - `decl_err`,
#'   `incl_err`: Error associated with declination and inclination,
#'   respectively.
#' @export
#'
#' @references
#' NCEI World Magnetic Model <https://www.ngdc.noaa.gov/geomag/WMM/DoDWMM.shtml>.
#'
#' NCEI Enhanced Magnetic Model <https://www.ngdc.noaa.gov/geomag/EMM/index.html>.
#'
#' IGRF-13 <https://www.ngdc.noaa.gov/IAGA/vmod/home.html>.
#'
#' NCEI Geomagnetic Modeling Team and British Geological Survey.
#' 2019. World Magnetic Model 2020. NOAA National Centers for Environmental
#' Information. \doi{10.25921/11v3-da71}, 2020, accessed 2021-02-23.
#'
#' Chulliat, A., W. Brown, P. Alken, C. Beggan, M. Nair, G. Cox, A. Woods, S.
#' Macmillan, B. Meyer and M. Paniccia, The US/UK World Magnetic Model for
#' 2020-2025: Technical Report, National Centers for Environmental Information,
#' NOAA, \doi{10.25923/ytk1-yx35}, 2020.
#'
#' @examples
#' wmm2020_extract(-64, 45, mm_decimal_year("2021-01-01"))
#' emm2017_extract(-64, 45, mm_decimal_year("2021-01-01"))
#' igrf13_extract(-64, 45, mm_decimal_year("2021-01-01"))
#'
wmm2020_extract <- function(lon, lat, year = mm_decimal_year(Sys.Date()),
                        height = mm_ellipsoidal_height(lon, lat, 0)) {
  lon <- vctrs::vec_cast(lon, double())
  lat <- vctrs::vec_cast(lat, double())
  height <- vctrs::vec_cast(height, double())
  year <- vctrs::vec_cast(year, double())

  mm_check_lon_lat(lon, lat)

  if (any(year > 2025.0, na.rm = TRUE) || any(year < 2020.0, na.rm = TRUE)) {
    warning("`year` must be between 2020.0 and 2025.0", immediate. = TRUE)
  }

  coords <- vctrs::vec_recycle_common(
    lambda = lon,
    phi = lat,
    height = height,
    year = year
  )

  coef <- cpp_mm_read_coef(system.file("extdata/WMM.COF", package = "headings"))
  tibble::as_tibble(cpp_mm_extract(coef, coords))
}

#' @rdname wmm_extract
#' @export
igrf13_extract <- function(lon, lat, year = mm_decimal_year(Sys.Date()),
                        height = mm_ellipsoidal_height(lon, lat, 0)) {
  lon <- vctrs::vec_cast(lon, double())
  lat <- vctrs::vec_cast(lat, double())
  height <- vctrs::vec_cast(height, double())
  year <- vctrs::vec_cast(year, double())

  mm_check_lon_lat(lon, lat)

  if (any(year < 1900.0, na.rm = TRUE) || any(year > 2025.0, na.rm = TRUE)) {
    warning("`year` must be between 1900.0 and 2025.0", immediate. = TRUE)
  }

  coords <- tibble::tibble(
    lambda = lon,
    phi = lat,
    height = height,
    year = year
  )

  # different coef file for each 5-year period
  year5 <- floor(coords$year / 5) * 5
  coef_year <- as.integer(pmin(2020, pmax(1900, year5)))
  coef_year_unique <- unique(coef_year)

  output <- tibble::tibble(
    decl = vctrs::vec_rep(NA_real_, length(coef_year)),
    incl = vctrs::vec_rep(NA_real_, length(coef_year)),
    decl_err = vctrs::vec_rep(NA_real_, length(coef_year)),
    incl_err = vctrs::vec_rep(NA_real_, length(coef_year))
  )

  for (coef_year_val in setdiff(coef_year_unique, NA_real_)) {
    coef <- igrf_coef_for_year(coef_year_val)
    indices <- which(coef_year == coef_year_val)
    output[indices, ] <- cpp_mm_extract(coef, coords[indices, ])
  }

  # no error model for IGRF, so NA these columns
  output$decl_err <- NA_real_
  output$incl_err <- NA_real_

  output
}

igrf_coef_for_year <- function(coef_year) {
  coef_file <- system.file(
    paste0("extdata/IGRF13/", coef_year, ".COF"),
    package = "headings"
  )

  cpp_mm_read_coef(coef_file)
}

#' @rdname wmm_extract
#' @export
emm2017_extract <- function(lon, lat, year = mm_decimal_year(Sys.Date()),
                        height = mm_ellipsoidal_height(lon, lat, 0)) {
  lon <- vctrs::vec_cast(lon, double())
  lat <- vctrs::vec_cast(lat, double())
  height <- vctrs::vec_cast(height, double())
  year <- vctrs::vec_cast(year, double())

  mm_check_lon_lat(lon, lat)

  if (any(year < 2000.0, na.rm = TRUE) || any(year > 2022.0, na.rm = TRUE)) {
    warning("`year` must be between 2000.0 and 2022.0", immediate. = TRUE)
  }

  coords <- tibble::tibble(
    lambda = lon,
    phi = lat,
    height = height,
    year = year
  )

  coef_year <- as.integer(pmin(2017, pmax(2000, coords$year)))
  coef_year_unique <- unique(coef_year)

  output <- tibble::tibble(
    decl = vctrs::vec_rep(NA_real_, length(coef_year)),
    incl = vctrs::vec_rep(NA_real_, length(coef_year)),
    decl_err = vctrs::vec_rep(NA_real_, length(coef_year)),
    incl_err = vctrs::vec_rep(NA_real_, length(coef_year))
  )

  mutable_coef <- emm_coef_for_year(2017)

  for (coef_year_val in setdiff(coef_year_unique, NA_real_)) {
    coef <- emm_coef_for_year(coef_year_val)
    cpp_mm_coalesce_for_emm2017(mutable_coef, coef, coef_year_val != 2017)

    indices <- which(coef_year == coef_year_val)
    output[indices, ] <- cpp_mm_extract(mutable_coef, coords[indices, ])
  }

  # no error model for EMM, so NA these columns
  output$decl_err <- NA_real_
  output$incl_err <- NA_real_

  output
}

emm_coef_for_year <- function(coef_year) {
  coef_file <- system.file(
    paste0("extdata/EMM", coef_year, ".COF"),
    package = "headings"
  )

  coef_sv_file <- system.file(
    paste0("extdata/EMM", coef_year, "SV.COF"),
    package = "headings"
  )

  cpp_mm_read_coef_sv(coef_file, coef_sv_file)
}


#' @rdname wmm_extract
#' @export
mm_ellipsoidal_height <- function(lon, lat, height) {
  lon <- vctrs::vec_cast(lon, double())
  lat <- vctrs::vec_cast(lat, double())
  height <- vctrs::vec_cast(height, double())

  mm_check_lon_lat(lon, lat)

  coords <- vctrs::vec_recycle_common(
    lambda = lon,
    phi = lat,
    height = height
  )

  cpp_mm_ellipsoidal_height(coords, headings::mm_egm9615_geoid_int)
}

#' @rdname wmm_extract
#' @export
mm_decimal_year <- function(date) {
  date <- as.Date(date)
  date_lt <- as.POSIXlt(date)
  (date_lt$year + 1900) + date_lt$yday / 365
}

#' @rdname wmm_extract
#' @export
mm_version <- function() {
  cpp_mm_version()
}

#' @rdname wmm_extract
"mm_egm9615_geoid_int"

mm_check_lon_lat <- function(lon, lat) {
  if (any(lon > 180, na.rm = TRUE) || any(lon < -180, na.rm = TRUE)) {
    warning("`lon` must be between -180 and 180", immediate. = TRUE)
  }

  if (any(lat > 90, na.rm = TRUE) || any(lat < -90, na.rm = TRUE)) {
    warning("`lat` must be between -90 and 90", immediate. = TRUE)
  }
}
