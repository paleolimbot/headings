
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
