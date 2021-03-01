
#' Heading-aware kernel density
#'
#' Computes an approximate density useful for visualization. For proper
#' circular densities, use [hdg_circular()] and [circular::density.circular()].
#'
#' @inheritParams hdg_norm
#' @param bw The bandwidth of the smoothing kernel. Automatic methods
#'   are not available, so you will have to set this value manually to
#'   obtain the smoothness you want.
#' @inheritParams stats::density
#' @param density A [hdg_density()] object.
#' @param main,xlab,ylab,axes See [graphics::plot()].
#' @param ... For [hdg_density()], dots are unused; for `plot.hdg_density()`,
#'   dots are passed to [graphics::lines()]; for `hdg_plot()`, passed to
#'   [graphics::points()]
#'
#' @return An object identical to [stats::density()] but with class
#'   "hdg_density".
#' @importFrom stats density
#' @export
#'
#' @examples
#' x <- head(kamloops2016$wind_dir, 1000)
#' hdg_density(x, na.rm = TRUE)
#' plot(hdg_density(x, na.rm = TRUE))
#' hdg_plot(x)
#'
hdg_density <- function(hdg, bw = 5,
                        kernel = c("gaussian", "epanechnikov", "rectangular",
                                   "triangular", "biweight",
                                   "cosine", "optcosine"),
                        weights = NULL, n = 512, na.rm = FALSE, ...) {
  data.name <- deparse(substitute(hdg))
  call <- match.call()
  x <- as_hdg(hdg)

  kernel <- match.arg(kernel)
  kernel_val <- stats::density(
    0,
    bw = bw,
    kernel = kernel,
    n = 512,
    from = -180,
    to = 180 - (1 / 512)
  )

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (is.null(weights)) {
    weights <- rep(1 / length(x), length(x))
  } else {
    weights <- rep_len(weights, length(x))
    weights <- weights / sum(weights, na.rm = TRUE)
  }

  dens_x <- seq(0, 360 - (1 / 512), length.out = 512)
  dens_y <- vapply(
    dens_x,
    function(bin_center) {
      diffs <- hdg_diff(x, bin_center)
      sum(
        stats::approx(kernel_val$x, kernel_val$y, xout = diffs)$y * weights,
        na.rm = TRUE
      )
    },
    double(1)
  )

  structure(
    list(
      x = dens_x,
      y = dens_y,
      bw = kernel_val$bw,
      n = n,
      call = call,
      data.name = data.name,
      has.na = FALSE
    ),
    class = c("hdg_density", "density")
  )
}

#' @rdname hdg_density
#' @importFrom graphics plot
#' @export
plot.hdg_density <- function(x, main = NULL, xlab = NULL, ylab = NULL,
                             axes = TRUE, ...) {
  hdg_uv <- uv_from_hdg(x$x)
  radius <- x$y
  radius_rng <- c(-max(radius), max(radius))
  radius_breaks <- setdiff(pretty(c(0, max(radius))), 0)

  if (is.null(main)) {
    main <- deparse(x$call)
  }

  if (is.null(xlab)) {
    xlab <- sprintf("N = %s Bandwidth = %0.3g", x$n, x$bw)
  }

  if (is.null(ylab)) {
    ylab <- "Density"
  }

  # set up the base plot
  graphics::plot(
    double(), double(),
    main = main, xlab = xlab, ylab = ylab,
    xlim = radius_rng, ylim = radius_rng,
    axes = axes,
    asp = 1
  )

  # draw the axes
  if (axes) {
    graphics::abline(h = 0, col = "grey80")
    graphics::abline(v = 0, col = "grey80")
    axis_hdg <- seq(0, 360, length.out = 512)
    for (y_ax_crc in radius_breaks) {
      graphics::lines(uv_from_hdg(axis_hdg) * y_ax_crc, col = "grey80")
    }
  }

  # draw the actual data!
  graphics::polygon(hdg_uv * radius, ...)

  invisible(x)
}

#' @rdname hdg_density
#' @export
hdg_plot <- function(hdg, density = hdg_density(hdg, na.rm = TRUE),
                     main = NULL, xlab = NULL, ylab = NULL,
                     axes = TRUE, ...) {
  if (is.null(main)) {
    main <- deparse(substitute(hdg))
  }

  graphics::plot(density, main = main, xlab = xlab, ylab = ylab, axes = axes)
  density_scale <- max(density$y)
  graphics::points(uv_from_hdg(hdg) * density_scale, ...)

  invisible(hdg)
}
