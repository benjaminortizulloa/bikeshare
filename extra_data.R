library(magrittr)
x <- readr::read_csv('data/2017-2019_DC_overview_summary.csv') %>%
  dplyr::mutate(year = "ave")
y <- readr::read_csv('data/2020_DC_overview_summary.csv') %>%
  dplyr::mutate(month = as.integer(month)) %>%
  dplyr::select(year, month, ave_time = total_time, ave_trip = total_trips, location)

z <- rbind(x,y)

readr::write_csv(z, 'data/dc_overview_summary.csv')
