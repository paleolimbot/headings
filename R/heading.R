
#' Create and export headings
#'
#' @param hdg A heading in degrees, where 0 is north,
#'   90 is east, 180 is south, and 270 is west. Values
#'   outside the range [0-360) are coerced to this range
#'   using [hdg_norm()].
#' @param uv,u,v A data.frame with columns `u` (magnitude east)
#'   and `v` (magnitude north).
#' @param rad An angle in radians such that a `hdg` of 90 is
#'   zero radians and a `hdg` of 0 is `pi / 2` radians.
#'
#' @export
#'
#' @examples
#' hdg_norm(-10:10)
#' hdg_from_uv(uv(1, 0))
#' uv_from_hdg(5:10)
#' uv_norm(uv(2, 0))
#'
hdg_norm <- function(hdg) {
  hdg <- as_hdg(hdg)
  while (any((hdg < 0 )| (hdg >= 360), na.rm = TRUE)) {
    hdg <- (hdg + 360) %% 360
  }

  hdg
}

#' @rdname hdg_norm
#' @export
uv <- function(u, v) {
  new_data_frame(recycle_common(u = cast_double(u), v = cast_double(v)))
}

#' @rdname hdg_norm
#' @export
uv_norm <- function(uv) {
  uv <- as_uv(uv)

  len <- sqrt(uv$u ^ 2 + uv$v ^ 2)
  len[len < .Machine$double.eps] <- NA_real_

  uv(uv$u / len, uv$v / len)
}

#' @rdname hdg_norm
#' @export
uv_from_hdg <- function(hdg) {
  hdg <- as_hdg(hdg)

  uv(
    cos((90 - hdg) * pi / 180),
    sin((90 - hdg) * pi / 180)
  )
}

#' @rdname hdg_norm
#' @export
hdg_from_uv <- function(uv) {
  uv <- as_uv(uv)

  radians <- atan2(uv$v, uv$u)
  hdg_norm(90 - (radians * 180 / pi))
}

#' @rdname hdg_norm
#' @export
rad_from_hdg <- function(hdg) {
  hdg <- as_hdg(hdg)
  (90 - hdg) * pi / 180
}

#' @rdname hdg_norm
#' @export
hdg_from_rad <- function(rad) {
  rad <- as_hdg(rad)
  hdg_norm(90 - (rad * 180 / pi))
}

#' Heading arithmetic
#'
#' These functions provide basic summary statistics for headings
#' that are commonly used. For more advanced statistics on circular data,
#' use [hdg_circular()] and the
#' [circular package](https://cran.r-project.org/package=circular).
#'
#' @inheritParams hdg_norm
#' @param hdg_ref A reference heading against which
#'   to compare `hdg`.
#' @param na.rm Use `TRUE` to remove missing values
#' @param weights Optional weights for each value. Note that for `hdg_sd()`
#'   this will trigger a bootstrap estimation as there is no clear way to
#'   apply a weighted standard deviation to circular data.
#' @param bootstrap_n For weighted standard deviation, the number of bootstrap
#'   replicates.
#'
#' @export
#'
#' @examples
#' hdg_mean(-10:10)
#' hdg_mean(c(350, 10))
#'
#' hdg_diff(350:370, 0)
#'
hdg_mean <- function(hdg, weights = 1, na.rm = FALSE) {
  hdg <- as_hdg(hdg)
  recycled <- recycle_common(hdg, weights)

  if (na.rm) {
    is_na <- is.na(recycled[[1]]) | is.na(recycled[[2]])
    hdg <- recycled[[1]][!is_na]
    weights <- recycled[[2]][!is_na]
  } else {
    hdg <- recycled[[1]]
    weights <- recycled[[2]]
  }

  uv <- uv_from_hdg(hdg) * weights
  hdg_from_uv(lapply(uv, sum))
}

#' @rdname hdg_mean
#' @export
hdg_diff <- function(hdg, hdg_ref) {
  common <- recycle_common(h1 = hdg, h2 = hdg_ref)
  uv1 <- uv_from_hdg(common$h1)
  uv2 <- uv_from_hdg(common$h2)

  # get the chord lengths
  uv_chord <- Map("-", uv1, uv2)
  chord_len <- sqrt(uv_chord$u ^ 2 + uv_chord$v ^ 2)
  angle_chord <- 2 * asin(chord_len / 2) * 180 / pi

  # apply sign of cross-product for direction
  # in this case the sign aligns with negative values counterclockwise
  # on the compass and positive values clockwise on the compass
  direction <- sign(uv1$u * uv2$v - uv1$v * uv2$u)
  direction[direction == 0] <- 1

  angle_chord * direction
}

#' @rdname hdg_mean
#' @export
hdg_sd <- function(hdg, weights = NULL, na.rm = FALSE, bootstrap_n = 100) {
  hdg <- as_hdg(hdg)

  if (!is.null(weights)) {
    sd_boot <- vapply(
      seq_len(bootstrap_n),
      function(i) {
        hdg_sd(
          sample(hdg, bootstrap_n, replace = TRUE, prob = weights),
          na.rm = na.rm
        )
      },
      double(1)
    )

    stats::median(sd_boot)
  } else if (na.rm) {
    hdg <- hdg[!is.na(hdg)]
    r_bar <- hdg_resultant(hdg)
    sqrt(-2 * log(r_bar)) * 180 / pi
  } else {
    r_bar <- hdg_resultant(hdg)
    sqrt(-2 * log(r_bar)) * 180 / pi
  }
}

hdg_resultant <- function(hdg) {
  x <- rad_from_hdg(as_hdg(hdg))
  sin_r <- sum(sin(x))
  cos_r <- sum(cos(x))
  sqrt(sin_r ^ 2 + cos_r ^ 2) / length(x)
}

#' Calculate declination, true heading, and magnetic heading
#'
#' For more detailed output, see [wmm2020_extract()].
#'
#' @inheritParams hdg_norm
#' @param model One of IGRF13, WMM2020, or EMM2017.
#' @param height A height above the earth's surface (as approximated
#'  by the EGM9615 geoid) in kilometers. If `height_ref` is
#'  "ellipsoid", this is interpreted as the height above the
#'  WGS84 ellipsoid.
#' @param height_ref One of geoid or ellipsoid.
#' @inheritParams wmm2020_extract
#'
#' @return A declination, true heading, or magnetic heading.
#' @export
#'
#' @examples
#' hdg_decl(-64, 45, year = 2021)
#' hdg_true_from_magnetic(13.40, -64, 45, year = 2021)
#' hdg_magnetic_from_true(356.51, -64, 45, year = 2021)
#'
hdg_decl <- function(lon, lat, year, height = 0,
                     height_ref = c("geoid", "ellipsoid"),
                     model = c("IGRF13", "WMM2020", "EMM2017")) {
  model <- match.arg(model)
  height_ref <- match.arg(height_ref)

  if (height_ref == "geoid") {
    height <- mm_ellipsoidal_height(lon, lat, height)
  }

  result <- switch(
    model,
    "IGRF13" = igrf13_extract(lon, lat, year = year, height = height),
    "WMM2020" = wmm2020_extract(lon, lat, year = year, height = height),
    "EMM2017" = emm2017_extract(lon, lat, year = year, height = height),
    stop(sprintf("Unknown model: '%s'", model))
  )

  result$decl
}

#' @rdname hdg_decl
#' @export
hdg_true_from_magnetic <- function(hdg, lon, lat, year,
                                   height = 0,
                                   height_ref = c("geoid", "ellipsoid"),
                                   model = c("IGRF13", "WMM2020", "EMM2017")) {
  model <- match.arg(model)
  height_ref <- match.arg(height_ref)

  recycle_common(
    hdg = as_hdg(hdg),
    lon = cast_double(lon),
    lat = cast_double(lat),
    year = cast_double(year)
  )

  hdg_norm(hdg + hdg_decl(lon, lat, year, height, height_ref, model))
}

#' @rdname hdg_decl
#' @export
hdg_magnetic_from_true <- function(hdg, lon, lat, year,
                                   height = 0,
                                   height_ref = c("geoid", "ellipsoid"),
                                   model = c("IGRF13", "WMM2020", "EMM2017")) {
  model <- match.arg(model)
  height_ref <- match.arg(height_ref)

  recycle_common(
    hdg = as_hdg(hdg),
    lon = cast_double(lon),
    lat = cast_double(lat),
    year = cast_double(year)
  )

  hdg_norm(hdg - hdg_decl(lon, lat, year, height, height_ref, model))
}

# internal for sanitizing
as_uv <- function(uv) {
  if (!is.list(uv)) {
    stop(
      sprintf(
        "Can't convert `uv` (<%s>) to <list(u = double(), v = double()>)",
        class(uv)[1]
      )
    )
  }
  uv(uv$u, uv$v)
}

as_hdg <- function(hdg) {
  cast_double(hdg)
}
