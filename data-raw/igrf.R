
curl::curl_download(
  "https://www.ngdc.noaa.gov/IAGA/vmod/geomag70_linux.tar.gz",
  "data-raw/igrf-13.tar.gz"
)

untar("data-raw/igrf-13.tar.gz", exdir = "data-raw")

file.copy(
  "data-raw/geomag70_linux/sample_out_IGRF13.txt",
  "inst/extdata/IGRF13"
)

# .COF file needs some help to make it in a form that WMM can handle

library(tidyverse)
lines <- read_lines("data-raw/geomag70_linux/IGRF13.COF")

lines_start <- str_which(lines, "^\\s*[A-Z]")
lines_end <- c(lines_start[-1] - 1, length(lines))

lines_wmm_friendly <- lines %>%
  str_replace("\\s+[A-Z]+[0-9]+", "")

lines_epoch <- str_extract(lines_wmm_friendly[lines_start], "[0-9.]+") %>%
  as.numeric()

lines_ranges <- Map(":", lines_start, lines_end)
lines_combined <- Map("[", list(lines_wmm_friendly), lines_ranges)

# need to append "9999..." to the end of the coefs to make them WMM friendly
lines_combined_wmm_friendly <- Map(
  c, lines_combined,
  paste0(strrep("9999", 10), "\n", strrep("9999", 10))
)

lines_filename <- sprintf("inst/extdata/IGRF13/%s.COF", lines_epoch)
walk2(lines_combined_wmm_friendly, lines_filename, write)

# IGRF 14 (2024)
dir.create("inst/extdata/IGRF14")
curl::curl_download(
  "https://www.ngdc.noaa.gov/IAGA/vmod/coeffs/igrf14coeffs.txt",
  "inst/extdata/IGRF14/igrf14coeffs.txt"
)

# Convert igrf14coeffs.txt to individual .COF files
igrf14_lines <- readLines("inst/extdata/IGRF14/igrf14coeffs.txt")

# Header row (line 4) has: g/h n m 1900.0 1905.0 ... 2025.0 2025-30
header_tokens <- strsplit(trimws(igrf14_lines[4]), "\\s+")[[1]]
epoch_tokens <- header_tokens[4:(length(header_tokens) - 1)]
epochs <- as.numeric(epoch_tokens)

# Parse data rows (line 5 onwards)
data_lines <- igrf14_lines[5:length(igrf14_lines)]
parsed <- lapply(data_lines, function(line) {
  parts <- strsplit(trimws(line), "\\s+")[[1]]
  list(
    gh = parts[1],
    n = as.integer(parts[2]),
    m = as.integer(parts[3]),
    vals = as.numeric(parts[4:(3 + length(epochs))]),
    sv = as.numeric(parts[length(parts)])
  )
})

# Build lookup by "g_n_m" / "h_n_m"
lookup <- list()
for (rec in parsed) {
  lookup[[paste(rec$gh, rec$n, rec$m, sep = "_")]] <- rec
}

# Unique (n, m) pairs in file order
nm_pairs <- unique(do.call(rbind, lapply(parsed, function(r) c(r$n, r$m))))

# Generate one .COF file per epoch
for (i in seq_along(epochs)) {
  epoch <- epochs[i]
  next_epoch <- if (i < length(epochs)) epochs[i + 1] else epoch + 5
  is_final <- (epoch == max(epochs))

  if (is_final) {
    model_name <- sprintf("IGRF%d", as.integer(epoch))
    sv_degree <- 8
  } else if (epoch >= 1945) {
    model_name <- sprintf("DGRF%d", as.integer(epoch))
    sv_degree <- 0
  } else {
    model_name <- sprintf("IGRF%02d", as.integer(epoch) %% 100)
    sv_degree <- 0
  }

  header_line <- sprintf(
    "  %.2f 13 %2d  0 %.2f %.2f   -1.0  600.0         %s   0",
    epoch, sv_degree, epoch, next_epoch, model_name
  )

  coef_lines <- character(nrow(nm_pairs))
  for (j in seq_len(nrow(nm_pairs))) {
    n <- nm_pairs[j, 1]
    m <- nm_pairs[j, 2]

    gnm <- lookup[[paste("g", n, m, sep = "_")]]$vals[i]
    g_sv <- lookup[[paste("g", n, m, sep = "_")]]$sv

    if (m == 0) {
      hnm <- 0.0
      h_sv <- 0.0
    } else {
      hnm <- lookup[[paste("h", n, m, sep = "_")]]$vals[i]
      h_sv <- lookup[[paste("h", n, m, sep = "_")]]$sv
    }

    if (is_final && n <= 8) {
      dgnm <- g_sv
      dhnm <- h_sv
    } else {
      dgnm <- 0.0
      dhnm <- 0.0
    }

    coef_lines[j] <- sprintf(
      "%2d %2d %10.2f %10.2f %10.2f %10.2f %4d",
      n, m, gnm, hnm, dgnm, dhnm, j
    )
  }

  all_lines <- c(header_line, coef_lines, strrep("9", 40), strrep("9", 40))
  writeLines(all_lines, sprintf("inst/extdata/IGRF14/%.0f.COF", epoch))
}
