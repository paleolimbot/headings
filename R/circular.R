
#' Use functions from the circular package
#'
#' @inheritParams hdg_norm
#'
#' @return A [circular::circular()] object.
#' @export
#'
#' @examples
#' if (requireNamespace("circular", quietly = TRUE)) {
#'   hdg_circular(1:10)
#' }
#'
hdg_circular <- function(hdg) {
  circular::as.circular(
    hdg,
    units = "degrees",
    type = "angles",
    template = "geographics",
    modulo = "asis",
    zero = 90,
    rotation = "clock"
  )
}
