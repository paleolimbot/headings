
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

