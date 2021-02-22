
heading_normalize <- function(heading) {
  (heading + 360) %% 360
}

uv_from_heading <- function(true_heading) {
  tibble::tibble(
    u = cos((90 - true_heading) * pi / 180),
    v = sin((90 - true_heading) * pi / 180)
  )
}

heading_from_uv <- function(uv) {
  result <- ifelse(
    uv$v >= 0,
    atan(uv$u / uv$v) * 180 / pi,
    180 + atan(uv$u / uv$v) * 180 / pi
  )

  (result + 360) %% 360
}

heading_mean <- function(true_heading, na.rm = FALSE) {
  if (na.rm) {
    true_heading <- true_heading[!is.na(true_heading)]
  }

  uv <- uv_from_heading(true_heading)
  heading_from_uv(lapply(uv, sum))
}

heading_diff <- function(h1, h2) {
  common <- vctrs::vec_recycle_common(h1 = h1, h2 = h2)
  uv1 <- uv_from_heading(common$h1)
  uv2 <- uv_from_heading(common$h2)

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

heading_sd <- function(true_heading, na.rm = FALSE) {
  if (na.rm) {
    true_heading <- true_heading[!is.na(true_heading)]
  }

  uv <- uv_from_heading(true_heading)

  # need the mean as a unit vector
  uv_mean <- lapply(uv, sum)
  uv_mean_len <- sqrt(uv_mean$u ^ 2 + uv_mean$v ^ 2)
  uv_mean$u <- uv_mean$u / uv_mean_len
  uv_mean$v <- uv_mean$v / uv_mean_len

  # ...to get the chord lengths
  uv_chord <- Map("-", uv, uv_mean)
  chord_len <- sqrt(uv_chord$u ^ 2 + uv_chord$v ^ 2)
  angle_chord <- 2 * asin(chord_len / 2) * 180 / pi

  sqrt(sum(angle_chord ^ 2) / (length(true_heading) - 1))
}
