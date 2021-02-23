
wmm_read_coefficients <- function(file) {
  cpp_wmm_read_cof(path.expand(file));
}

wmm_extract <- function(lon, lat, year = wmm_decimal_year(Sys.Date()),
                        height = 0, coef = NULL) {
  if (is.null(coef)) {
    coef <- wmm_read_coefficients(system.file("extdata/WMM.COF", package = "headings"))
  }

  lon <- vctrs::vec_cast(lon, double())
  lat <- vctrs::vec_cast(lat, double())
  height <- vctrs::vec_cast(height, double())
  year <- vctrs::vec_cast(year, double())

  coords <- tibble::tibble(
    lambda = lon,
    phi = lat,
    height = height,
    year = year
  )

  tibble::as_tibble(cpp_wmm_extract(coef, coords))
}

wmm_decimal_year <- function(date) {
  date <- vctrs::vec_cast(date, vctrs::new_date())
  date_lt <- as.POSIXlt(date)
  (date_lt$year + 1900) + date_lt$yday / 365
}

wmm_version <- function() {
  cpp_wmm_version()
}
