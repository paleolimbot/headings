
cast_double <- function(x, arg = deparse(substitute(x))) {
  if (is.object(x) || (!identical(x, NA) &&!is.double(x) && !is.integer(x))) {
    type_lab <- class(x)[1]
    stop(
      sprintf(
        "Can't cast `%s` (<%s>) to <double>",
        arg,
        type_lab
      ),
      call. = FALSE
    )
  }

  as.double(x)
}

new_data_frame <- function(x) {
  structure(x, row.names = c(NA, length(x[[1]])), class = "data.frame")
}

recycle_common <- function(...) {
  dots <- list(...)
  lengths <- vapply(dots, length, integer(1))
  non_constant_lengths <- unique(lengths[lengths != 1])
  if (length(non_constant_lengths) == 0) {
    final_length <- 1
  } else if(length(non_constant_lengths) == 1) {
    final_length <- non_constant_lengths
  } else {
    lengths_label <- paste0(non_constant_lengths, collapse = ", ")
    stop(sprintf("Incompatible lengths: %s", lengths_label))
  }

  lapply(dots, rep_len, final_length)
}
