
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
#' @inheritParams hdg_norm
#' @param hdg_ref A reference heading against which
#'   to compare `hdg`.
#' @param na.rm Use `TRUE` to remove missing values
#' @param weights Optional weights for each value
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
hdg_sd <- function(hdg, weights = 1, na.rm = FALSE) {
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

  # must be a unit vector
  uv <- uv_from_hdg(hdg)

  # similarly, weights must be 0...1
  weights <- weights / max(weights, na.rm = na.rm)

  # need the mean as a unit vector
  uv_mean <- uv_norm(lapply(uv * weights, sum))

  # ...to get the chord lengths
  uv_chord <- Map("-", uv, uv_mean)
  chord_len <- sqrt(uv_chord$u ^ 2 + uv_chord$v ^ 2)
  angle_chord <- 2 * asin(chord_len / 2) * 180 / pi

  n_non_zero_weights <- sum(weights != 0)

  # scale angle_chord ^ 2 according to weights
  sqrt(
    sum(weights * (angle_chord ^ 2)) /
      (((n_non_zero_weights - 1) * sum(weights)) / n_non_zero_weights)
  )
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
