library('magrittr')
library(sf)

docks <- readr::read_csv('data/Capital_Bike_Share_Locations.csv') %>%
  dplyr::select(dock = TERMINAL_NUMBER, long = LONGITUDE, lat = LATITUDE)

cleanOD <- function(month, year){
  filename <- paste0("data/", year, month, "-capitalbikeshare-tripdata.csv")
  
  x <- readr::read_csv(filename) %>%
    dplyr::select(Duration, origin = 'Start station number', destination = 'End station number') %>%
    dplyr::mutate(origin = as.numeric(origin), destination = as.numeric(destination))

  
  x1 <- dplyr::filter(x, origin < destination)
  
  x2 <- dplyr::filter(x, origin > destination)
  names(x2) <- c('Duration', "destination", "origin")
  
  print(x)
  
  x <- dplyr::bind_rows(x1, x2) %>%
    dplyr::group_by(origin, destination) %>%
    dplyr::summarize(
      total_time = sum(Duration),
      trips = dplyr::n()
    ) %>%
    dplyr::arrange(desc(trips)) %>%
    dplyr::left_join(docks, by = c("origin" = "dock")) %>%
    dplyr::rename(o_long = long, o_lat = lat) %>%
    dplyr::left_join(docks, by = c("destination" = "dock")) %>%
    dplyr::rename(d_long = long, d_lat = lat) %>%
    dplyr::ungroup() %>%
    dplyr::select(-origin, -destination) %>%
    dplyr::mutate(year = year, month = month)
  
  return(x)
}

c(paste0('0', 1:9), 10:12) %>%
  paste0("2018", ., "-capitalbikeshare-tripdata.csv") %>%
  vapply(function(x){x %in% dir('data/')}, logical(1)) %>% table

test <-c(paste0('0', 1:9), 10:12) %>%
  lapply(function(x){cleanOD(x, 2019)}) %>%
  dplyr::bind_rows()

readr::write_csv(test, 'data/2019_capital.csv')

test <- c(paste0('0', 1:9), 10:12) %>%
  lapply(function(x){cleanOD(x, 2018)}) %>%
  dplyr::bind_rows()

readr::write_csv(test, 'data/2018_capital.csv')

cleanOD2 <- function(filename, year){
  x <- readr::read_csv(filename) %>%
    dplyr::select(Duration, origin = 'Start station number', destination = 'End station number', date = 'Start date') %>%
    dplyr::mutate(origin = as.numeric(origin), destination = as.numeric(destination), year = year, month = lubridate::month(date)) %>%
    dplyr::select(-date)
  
  
  x1 <- dplyr::filter(x, origin < destination)
  
  x2 <- dplyr::filter(x, origin > destination)
  names(x2) <- c('Duration', "destination", "origin", "year", "month")
  
  x <- dplyr::bind_rows(x1, x2) %>%
    {print(.); .} %>%
    dplyr::group_by(origin, destination, year, month) %>%
    dplyr::summarize(
      total_time = sum(Duration),
      trips = dplyr::n()
    ) %>%
    dplyr::arrange(desc(trips)) %>%
    dplyr::left_join(docks, by = c("origin" = "dock")) %>%
    dplyr::rename(o_long = long, o_lat = lat) %>%
    dplyr::left_join(docks, by = c("destination" = "dock")) %>%
    dplyr::rename(d_long = long, d_lat = lat) %>%
    dplyr::ungroup() %>%
    dplyr::select(-origin, -destination)
  
  return(x)
}

test <- paste0('data/2017Q', 1:4, '-capitalbikeshare-tripdata.csv') %>%
  lapply(function(x){cleanOD2(x, 2017)}) %>%
  dplyr::bind_rows()

readr::write_csv(test, 'data/2017_capital.csv')


raw_trip<- lapply(paste0('data/',2017:2019, '_capital.csv'), function(x){
  readr::read_csv(x) %>%
    dplyr::mutate(month = as.numeric(month))
}) %>%
  dplyr::bind_rows()

dc_overview_summary <- raw_trip %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(
    total_time = sum(total_time),
    total_trips = sum(trips)
  ) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    ave_time = mean(total_time),
    ave_trip = mean(total_trips)
  ) %>%
  dplyr::mutate(location = 'DC')

readr::write_csv(dc_overview_summary, 'data/2017-2019_DC_overview_summary.csv')

dc_geo_summary <- raw_trip %>%
  dplyr::filter(!is.na(o_long), !is.na(d_long)) %>%
  dplyr::group_by(month, o_long, o_lat, d_long, d_lat) %>%
  dplyr::summarise(
    ave_time = mean(total_time),
    ave_trips = mean(trips),
    tot_time = sum(total_time),
    tot_trips = sum(trips),
    instances = n()
  ) 


dc_geo_summary_filter <- dplyr::filter(dc_geo_summary, ave_trips >=30) %>% dplyr::ungroup()
dc_geo_summary_filter$trip_cut <- dc_geo_summary_filter$ave_trips %>% cut(breaks = 5) %>% as.numeric()
dc_geo_summary_filter$trip_cut_no <- dc_geo_summary_filter$ave_trips %>% ggplot2::cut_number(n = 5) %>% as.numeric()

non_geo_summary <- dc_geo_summary_filter %>%
  dplyr::left_join(docks, by = c("o_long" = "long", "o_lat" = "lat")) %>%
  dplyr::rename(origin = dock) %>%
  dplyr::left_join(docks, by = c("d_long" = "long", "d_lat" = "lat"))%>%
  dplyr::rename(destination = dock) %>%
  dplyr::select(-d_long, -d_lat, -o_long, -o_lat) %>%
  # dplyr::select(origin, destination, month, ave_time, ave_trips, trip_cut, trip_cut_no) %>%
  dplyr::group_by(origin, destination) %>%
  tidyr::nest()

readr::write_rds(non_geo_summary, 'data/2017-2019_DC_OD_summary.rds')

docks2 <- docks %>% 
  sf::st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  sf::as_Spatial()

# test_od <- stplanr::od2line(dplyr::select(non_geo_sumamry, -data), docks2)
# test_route <- stplanr::line2route(test_od, route_fun = stplanr::route_osrm)

# test_rout_sf <- sf::st_as_sf(test_route)
# test_rout_sf
# sf::st_write(test_rout_sf, 'data/2017-2019_routes.gpkg')
# 
# test2 <- test %>%
#   dplyr::ungroup() %>%
#   mutate(id = 1:nrow(.) %>% as.character()) %>%
#   tidyr::unnest()

# geo_sum <- test_rout_sf %>%
#   dplyr::left_join(test2)
# 
# geo_sum %>%
#   dplyr::select(origin, destination) %>%
#   sf::st_write('data/dc_route_key.gpkg')

route_key <- sf::st_read('data/dc_route_key.gpkg')

geo_sum <- non_geo_summary %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id = 1:nrow(.) %>% as.character()) %>%
  tidyr::unnest() 

geo_sum <- dplyr::left_join(route_key, geo_sum)

sf::st_write(geo_sum, 'data/2017-2019_routes_summary_full.gpkg')

geo_sum <- sf::st_read('data/2017-2019_routes_summary_full.gpkg')

dc_bound <- sf::st_read('data/Washington_DC_Boundary-shp/Washington_DC_Boundary.shp')

dc_routes <- route_key %>%
  dplyr::filter(sf::st_within(., dc_bound, sparse = F))

dc_routes$keep <- T
st_geometry(dc_routes) <- NULL

dc_routes_data <- dplyr::inner_join(geo_sum, dplyr::distinct(dc_routes))
  
sf::st_write(dc_routes_data,'data/2017-2019_routes_summary_DC.gpkg')

features_month <-dc_routes_data %>%
  # dplyr::filter(!is.na(distance)) %>%
  dplyr::filter(month == 1) 

feature_intersect <- features_month %>%
  sf::st_intersection()

for (int in seq_along(feature_intersect$id)) {
  feature_intersect$trip_cut[int] = max(features_month[feature_intersect$origins[[int]], ]$trip_cut, na.rm = TRUE)
  feature_intersect$trip_cut_no[int] = max(features_month[feature_intersect$origins[[int]], ]$trip_cut_no, na.rm = TRUE)
}

single_lines <- test_intersect %>% st_collection_extract('LINESTRING')

dc_single_lines <- single_lines %>%
  filter(st_within(., dc_bound, sparse = F))

sf::st_write(dc_single_lines, 'data/dc_january.geojson')
