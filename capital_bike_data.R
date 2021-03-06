library('magrittr')
library(sf)

docks <- readr::read_csv('data/Capital_Bike_Share_Locations.csv') %>%
  dplyr::select(dock = TERMINAL_NUMBER, long = LONGITUDE, lat = LATITUDE)

dock_name <- readr::read_csv('data/Capital_Bike_Share_Locations.csv') %>%
  dplyr::select(dock = TERMINAL_NUMBER, addy = ADDRESS)

cleanOD <- function(month, year){
  # filename <- paste0("data/", year, month, "-capitalbikeshare-tripdata.csv")
  filename <- paste0("data/", month, year, "-capitalbikeshare-tripdata.csv")
  x <- readr::read_csv(filename) %>%
    dplyr::mutate(Duration = difftime(ended_at, started_at, units = 'min') %>% ceiling() %>% as.integer()) %>%
    # dplyr::select(Duration, origin = 'Start station number', destination = 'End station number') %>%
    dplyr::select(Duration, start_station_name, end_station_name) %>%
    dplyr::left_join(dplyr::rename(dock_name, origin = dock), by = c('start_station_name' = 'addy')) %>%
    dplyr::left_join(dplyr::rename(dock_name, destination = dock), by = c('end_station_name' = 'addy')) %>%
    dplyr::mutate(origin = as.numeric(origin), destination = as.numeric(destination)) %>%
    dplyr::select(-start_station_name, -end_station_name)

  
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
    # dplyr::left_join(docks, by = c("origin" = "dock")) %>%
    # dplyr::rename(o_long = long, o_lat = lat) %>%
    # dplyr::left_join(docks, by = c("destination" = "dock")) %>%
    # dplyr::rename(d_long = long, d_lat = lat) %>%
    dplyr::ungroup() %>%
    # dplyr::select(-origin, -destination) %>%
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
non_geo_summary <- readr::read_rds('data/2017-2019_DC_OD_summary.rds')

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

dc_routes2 <- dc_routes %>%
  dplyr::group_by(origin, destination) %>%
  dplyr::count() %>%
  dplyr::select(-n)

  
# dc_routes$keep <- T
# st_geometry(dc_routes) <- NULL
# dc_routes <- dplyr::distinct(dc_routes) %>%
#   dplyr::mutate(org_dest = paste0(origin, '_', destination))

dc_routes3 <- geo_sum %>%
  sf::st_set_geometry(NULL) %>%
  # dplyr::mutate(org_dest = paste0(origin, '_', destination)) %>%
  # dplyr::filter(org_dest %in% dc_routes$org_dest) %>%
  # dplyr::inner_join(geo_sum, dplyr::distinct(dc_routes)) %>%
  dplyr::select(origin, destination, month, tot_time, tot_trips, instances, id) %>%
  dplyr::distinct()

dc_routes_data <- dplyr::left_join(dc_routes2, dc_routes3)
  
sf::st_write(dc_routes_data,'data/2017-2019_routes_summary_dc_final.gpkg')

dc_routes_data <- sf::st_read('data/2017-2019_routes_summary_dc_final.gpkg')

features_month <-dc_routes_data %>%
  # dplyr::filter(!is.na(distance)) %>%
  dplyr::filter(month == 1) 

feature_intersect <- features_month %>%
  sf::st_intersection() %>% 
  sf::st_collection_extract('LINESTRING')

for (int in seq_along(feature_intersect$id)) {
  feature_intersect[int,]$tot_trips <- sum(features_month[feature_intersect$origins[[int]], ]$tot_trips, na.rm = T)
  feature_intersect[int,]$tot_time <- sum(features_month[feature_intersect$origins[[int]], ]$tot_time, na.rm = T)
  feature_intersect[int,]$instances <- sum(features_month[feature_intersect$origins[[int]], ]$instances, na.rm = T)
}

final_features <- feature_intersect %>%
  dplyr::ungroup() %>%
  dplyr::select(month, tot_time, tot_trips, instances) %>%
  dplyr::mutate(ave_time = tot_time/instances, ave_trips = tot_trips/instances) %>%
  dplyr::mutate(trip_cut = cut(ave_trips, breaks = 5) %>% as.numeric(),
                trip_cut_no = ggplot2::cut_number(ave_trips, n = 5) %>% as.numeric() )
# feature_intersect <- feature_intersect %>% st_collection_extract('LINESTRING')

# dc_single_lines <- single_lines %>%
#   filter(st_within(., dc_bound, sparse = F))

sf::st_write(final_features, 'data/dc_summary_1.geojson')

purrr::map(6:12, function(x){
  print(paste0('started', x))
  features_month <-dc_routes_data %>%
    # dplyr::filter(!is.na(distance)) %>%
    dplyr::filter(month == x) 
  
  feature_intersect <- features_month %>%
    sf::st_intersection() %>% 
    sf::st_collection_extract('LINESTRING')
  
  for (int in seq_along(feature_intersect$id)) {
    feature_intersect[int,]$tot_trips <- sum(features_month[feature_intersect$origins[[int]], ]$tot_trips, na.rm = T)
    feature_intersect[int,]$tot_time <- sum(features_month[feature_intersect$origins[[int]], ]$tot_time, na.rm = T)
    feature_intersect[int,]$instances <- sum(features_month[feature_intersect$origins[[int]], ]$instances, na.rm = T)
  }
  
  final_features <- feature_intersect %>%
    dplyr::ungroup() %>%
    dplyr::select(month, tot_time, tot_trips, instances) %>%
    dplyr::mutate(ave_time = tot_time/instances, ave_trips = tot_trips/instances) %>%
    dplyr::mutate(trip_cut = cut(ave_trips, breaks = 5) %>% as.numeric(),
                  trip_cut_no = ggplot2::cut_number(ave_trips, n = 5) %>% as.numeric() )
  # feature_intersect <- feature_intersect %>% st_collection_extract('LINESTRING')
  
  # dc_single_lines <- single_lines %>%
  #   filter(st_within(., dc_bound, sparse = F))
  
  filename <- paste0('data/dc_summary_', x, '.geojson')
  sf::st_write(final_features, filename)
  print(paste0('finished', x))
})

all_dc <- lapply(1:12, function(x){
  filename <- paste0('data/dc_summary_', x, '.geojson')
  x <- sf::st_read(filename)
  return(x)
}) %>%
  do.call(rbind, .)

all_dc$type = 'summary'

write_sf(all_dc, 'data/all_dc_summary.geojson')

year2020_1_3 <- lapply(paste0('0', 1:3), function(x){
  cleanOD(2020, x)
})

year2020_4_6 <- lapply(paste0('0', 4:6), function(x){
  cleanOD(2020, x)
})

year_2020 <- dplyr::bind_rows(c(year2020_1_3, year2020_4_6))
names(year_2020) <- c('origin', 'destination', 'total_time', 'trips', 'month', 'year')

readr::write_csv(year_2020, 'data/2020_capital.csv')

dc_overview_summary_2020 <- year_2020 %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(
    total_time = sum(total_time),
    total_trips = sum(trips)
  )  %>%
#   dplyr::group_by(month) %>%
#   dplyr::summarise(
#     ave_time = mean(total_time),
#     ave_trip = mean(total_trips)
#   ) %>%
  dplyr::mutate(location = 'DC')

readr::write_csv(dc_overview_summary_2020, 'data/2020_DC_overview_summary.csv')

dc_geo_summary_2020 <- year_2020 %>%
  dplyr::filter(origin != 0, destination != 0) %>%
  dplyr::group_by(origin, destination, year, month) %>%
  dplyr::summarise(
    ave_time = mean(total_time),
    ave_trips = mean(trips),
    tot_time = sum(total_time),
    tot_trips = sum(trips),
    instances = dplyr::n()
  ) 

dc_geo_summary_filter_2020 <- dplyr::filter(dc_geo_summary_2020, ave_trips > 3) %>% dplyr::ungroup()

geo_sum_2020<- dc_geo_summary_filter_2020 %>%
  dplyr::group_by(origin, destination) %>%
  tidyr::nest()

dc_routes3_2020 <- geo_sum_2020 %>%
  tidyr::unnest() %>%
  # sf::st_set_geometry(NULL) %>%
  # dplyr::mutate(org_dest = paste0(origin, '_', destination)) %>%
  # dplyr::filter(org_dest %in% dc_routes$org_dest) %>%
  # dplyr::inner_join(geo_sum, dplyr::distinct(dc_routes)) %>%
  dplyr::select(origin, destination, year, month, tot_time, tot_trips, instances) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()

dc_routes_data_2020 <- dplyr::left_join(dc_routes2, dc_routes3_2020)

# if had time...
# need_routes <- dplyr::anti_join(dc_routes3_2020, dc_routes2) %>% dplyr::select(origin, destination) %>% dplyr::distinct()

sf::st_write(dc_routes_data_2020,'data/2020_routes_summary_dc_final.gpkg')

dc_routes_data_2020$month <- as.integer(dc_routes_data_2020$month)

purrr::map(1:6, function(x){
  print(paste0('started', x))
  features_month <-dc_routes_data_2020 %>%
    # dplyr::filter(!is.na(distance)) %>%
    dplyr::filter(month == x) 
  
  feature_intersect <- features_month %>%
    sf::st_intersection() %>% 
    sf::st_collection_extract('LINESTRING')
  
  for (int in seq_along(feature_intersect$id)) {
    feature_intersect[int,]$tot_trips <- sum(features_month[feature_intersect$origins[[int]], ]$tot_trips, na.rm = T)
    feature_intersect[int,]$tot_time <- sum(features_month[feature_intersect$origins[[int]], ]$tot_time, na.rm = T)
    feature_intersect[int,]$instances <- sum(features_month[feature_intersect$origins[[int]], ]$instances, na.rm = T)
  }
  
  final_features <- feature_intersect %>%
    dplyr::ungroup() %>%
    dplyr::select(year, month, tot_time, tot_trips, instances) %>%
    dplyr::mutate(ave_time = tot_time/instances, ave_trips = tot_trips/instances) %>%
    dplyr::mutate(trip_cut = cut(ave_trips, breaks = 5) %>% as.numeric(),
                  trip_cut_no = ggplot2::cut_number(ave_trips, n = 5) %>% as.numeric() )
  # feature_intersect <- feature_intersect %>% st_collection_extract('LINESTRING')
  
  # dc_single_lines <- single_lines %>%
  #   filter(st_within(., dc_bound, sparse = F))
  
  filename <- paste0('data/dc_2020_', x, '.geojson')
  sf::st_write(final_features, filename)
  print(paste0('finished', x))
})

x <- readr::read_csv('data/dc_overview_summary.csv')
x$perc_trip <- x$ave_trip/max(x$ave_trip)
x$perc_time <- x$ave_time/max(x$ave_time)
x$perc_trip_length <- x$perc_trip * .75 + .25
x$perc_time_length <- x$perc_time * .75 + .25
x$angle <- x$month * (1/12) * 3/2 * -pi+ (.5 * pi )
x$path <- 1

y <- rbind(dplyr::mutate(x, path = 0), x)

dta <- purrr::map(seq_along(y$month), function(i){
  path <- y$path[i]
  angle <- y$angle[i]
  len <- y$perc_trip_length[i]
  year <- y$year[i]
  
  if(path == 1) return(tibble::tibble(x = len * cos(angle), y = len * sin(angle)))
  
  if(year == 'ave' & path == 0) return(tibble::tibble(x = .25 * cos(angle), y = .25 * sin(angle)))
  
  return(tibble::tibble(x = (len - .075) * cos(angle), y = (len - .075) * sin(angle)))
}) %>%
  dplyr::bind_rows()

z <- dplyr::bind_cols(y, dta)

readr::write_csv(z, 'data/dc_overview_summary_calc.csv')

library(ggplot2)

ggplot(z) +
  geom_point(aes(x, y, color = year))

geo_2020 <- dir('data/', full.names = T) %>%
  .[stringr::str_detect(., 'dc_2020_')] %>%
  lapply(sf::read_sf) %>%
  do.call(rbind, .)

geo_2020 <- dplyr::mutate(geo_2020, year = as.character(year))

geo_ave <- sf::read_sf('data/all_dc_summary.geojson')

geo_ave$year <- 'ave'
geo_ave <- dplyr::select(geo_ave, -type)

geo_full <- rbind(geo_ave, geo_2020)

sf::write_sf(geo_full, 'data/full_dc_data.geojson')
  