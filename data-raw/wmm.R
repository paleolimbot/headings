
# NOAA World Magnetic Model
# https://www.ngdc.noaa.gov/geomag/WMM/wmm_ldownload.shtml
# https://www.ngdc.noaa.gov/geomag/WMM/soft.shtml

curl::curl_download(
  "https://www.ngdc.noaa.gov/geomag/WMM/data/WMM2020/WMM2020_Linux.tar.gz",
  "data-raw/wmm2020.tar.gz"
)

untar("data-raw/wmm2020.tar.gz", exdir = "data-raw")

src_dir <- "data-raw/WMM2020_Linux/src"

file.copy(
  c(
    "data-raw/WMM2020_Linux/src/GeomagnetismHeader.h",
    "data-raw/WMM2020_Linux/src/GeomagnetismLibrary.c"
  ),
  "src"
)

file.copy("data-raw/WMM2020_Linux/bin/WMM.COF", "inst/extdata")
file.copy("data-raw/WMM2020_Linux/WMM2020_TEST_VALUES.txt", "inst/extdata")


# include the geoid as package data instead of the 8 MB header file
egm9615h <- readr::read_file("data-raw/WMM2020_Linux/src/EGM9615.h")
float_values <- stringr::str_extract(egm9615h, stringr::regex("\\{.*?\\}", dotall = TRUE))
wmm_egm9615_geoid_dbl <- float_values %>%
  stringr::str_remove_all("[{}]") %>%
  stringr::str_split("\\s*,\\s*") %>%
  .[[1]] %>%
  as.numeric()

# these are all fixed-precision at 3 decimal places, so store as int
wmm_egm9615_geoid_int <- as.integer(wmm_egm9615_geoid_dbl * 1000)

usethis::use_data(wmm_egm9615_geoid_int, overwrite = TRUE)
