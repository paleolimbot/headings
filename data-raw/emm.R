
curl::curl_download(
  "https://www.ngdc.noaa.gov/geomag/EMM/data/geomag/EMM2017_Sph_Linux.zip",
  "data-raw/emm2017.zip"
)

unzip("data-raw/emm2017.zip", exdir = "data-raw")

file.copy(
  "data-raw/EMM2017_Linux/EMM2017TestValues.txt",
  "inst/extdata"
)

file.copy(
  list.files("data-raw/EMM2017_Linux", ".COF$", full.names = TRUE),
  "inst/extdata"
)
