
# Kamloops, BC, 2016 (hourly)
kamloops2016 <- weathercan::weather_dl(
  station_ids = 51423,
  start = "2016-01-01",
  end = "2016-12-31"
)

kamloops2016 <- as.data.frame(kamloops2016)

usethis::use_data(kamloops2016, overwrite = TRUE)
