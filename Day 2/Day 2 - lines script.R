#Day 2 - lines

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(rcartocolor)
library(ggfx)
library(lubridate)
library(ggtext)
library(patchwork)
library(showtext)
library(osmdata)
library(spData)
library(spDataLarge)
library(tmap)
library(sfnetworks)
library(stplanr)

set.seed(11022022)
#Load some fonts----

#from Google


font_add_google(name = "Amatic SC", family = "amatic-sc")

carto_colors <- rcartocolor::carto_pal(12, "Vivid")

colors <- c(carto_colors[[8]], carto_colors[[11]])
#read in bikeshare data. 
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("./Day 1/bikeshare data sept 2022.csv")

dc <- osmdata::getbb("Washington DC", format_out = "sf_polygon", limit = 1)


###Example---Chapter 13 of Geocompr

osmdata::getbb("Bristol", format_out = "sf_polygon", limit = 1)

bristol_region

bristol_ttwa

names(bristol_zones)

bristol_od


nrow(bristol_od)
#> [1] 2910
nrow(bristol_zones)

zones_attr = bristol_od |> 
  group_by(o) |> 
  summarize(across(where(is.numeric), sum)) |> 
  dplyr::rename(geo_code = o)

head(zones_attr)

head(bristol_od)

zones_attr

#This checks if the geo_codes in both objects are the same
summary(zones_attr$geo_code %in% bristol_zones$geo_code)

zones_joined = left_join(bristol_zones, zones_attr, by = "geo_code")
sum(zones_joined$all)

names(zones_joined)

head(zones_joined)

zones_destinations = bristol_od |> 
  group_by(d) |> 
  summarize(across(where(is.numeric), sum)) |> 
  dplyr::select(geo_code = d, all_dest = all)

zones_od = inner_join(zones_joined, zones_destinations, by = "geo_code")


qtm(zones_od, c("all", "all_dest")) +
  tm_layout(panel.labels = c("Origin", "Destination"))

od_top5 = bristol_od |> 
  slice_max(all, n = 5)

bristol_od$Active = (bristol_od$bicycle + bristol_od$foot) /
  bristol_od$all * 100

od_intra = filter(bristol_od, o == d)
od_inter = filter(bristol_od, o != d)

desire_lines = od2line(od_inter, zones_od)

qtm(desire_lines, lines.lwd = "all")

bristol_stations

desire_rail = top_n(desire_lines, n = 3, wt = train)

ncol(desire_rail)
#> [1] 9
desire_rail = line_via(desire_rail, bristol_stations)
ncol(desire_rail)
#> [1] 12
#> 

desire_lines$distance_km = as.numeric(st_length(desire_lines)) / 1000
desire_lines_short = desire_lines |> 
  filter(car_driver >= 100, distance_km <= 5, distance_km >= 2.5)

routes_short = route(l = desire_lines_short, route_fun = route_osrm,
                     osrm.profile = "bike")

mapview::mapview(st_geometry(routes_short))

uptake = function(x) {
  case_when(
    x <= 3 ~ 0.5,
    x >= 8 ~ 0,
    TRUE ~ (8 - x) / (8 - 3) * 0.5
  )
}
routes_short_scenario = routes_short |> 
  mutate(uptake = uptake(distance / 1000)) |> 
  mutate(bicycle = bicycle + car_driver * uptake,
         car_driver = car_driver * (1 - uptake))
sum(routes_short_scenario$bicycle) - sum(routes_short$bicycle)


route_network_scenario = overline(routes_short_scenario, attrib = "bicycle")

mapview::mapview(route_network_scenario)

bristol_ways$lengths = st_length(bristol_ways)
ways_sfn = sfnetworks::as_sfnetwork(bristol_ways)
class(ways_sfn)

ways_sfn

ways_centrality = ways_sfn |> 
  activate("edges") |>  
  mutate(betweenness = tidygraph::centrality_edge_betweenness(lengths)) 


####----The start of my own bike route map!!!---
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("./Day 1/bikeshare data sept 2022.csv")

df <- df %>% 
  filter(!is.na(end_lat)
         , !is.na(end_lng)
         , !is.na(start_lat)
         , !is.na(start_lng)) 


df$started_at <- df$started_at %>% 
  mdy_hm()     

df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S")
         , date = format(started_at, format = "%Y-%m-%d")
         , day = weekdays(started_at))
         #, o = st_as_sf(df, coords = c("start_lng", "start_lat")
          #              , crs = 4326)
         #, d = st_as_sf(df, coords = c("end_lng", "end_lat")
          #              , crs= 4326))


df1 <- df %>% 
  filter(grepl("Saturday", day)
         | grepl("Sunday", day)
         , !is.na(start_station_id)
         , !is.na(end_station_id)) |>
  rename(o = start_station_id, d = end_station_id)


df1_group <- df1 |>
  filter(o != d) |>
  group_by(o, d, start_lng, start_lat, end_lng, end_lat) |>
  summarize(count = n()) |>
  ungroup() |>
  arrange(desc(count)) |>
  slice_max(prop = .6
            , order_by = count
            , with_ties = FALSE)

df1_group1 <- st_as_sf(df1_group, coords = c("start_lng", "start_lat")
                       , crs = 4326)

df1_group1$dest <-  st_as_sf(df1_group1, coords = c("end_lng", "end_lat"), crs = 4326)

df1_start <- df1_group1 |>
  select(o, geometry)

df1_end <- df1_group1 |>
  select(d, dest)

df1_route <- route(from = df1_start
                   , to = df1_end
                   , route_fun = route_osrm
                   , osrm.profile = "bike")

join <- df1_start |> 
  left_join(df1_end, by = "d")

join$geo <- st_as_sf(join, coords = c("end_lng", "end_Lat"), crs = 4326))

line <- st_cast(c(df1$start_lng, df1$start_lat, df1$end_lng, df1$end_lat), "LINESTRING")

line2 <- st_cast(line, "LINESTRING")  
paths <- route(from = st_as_sf(df1, C(coords = "start_lng", "start_lat"), crs = 4326)
               , to = st_as_sf(df1, C(coords = "end_lng", "end_lat"), crs = 4326)
               , route_fun = route_osrm
               , osrm.profile = "bike")  
route(fr)

head(df1_start)
head(df1_end)
df1_end <- st_as_sf(df1_group, coords = c("end_lng", "end_lat"), crs = 4326)  

df_start <- st_as_sf(df, coords = c("start_lng", "start_lat"), crs = 4326)
df_end <- st_as_sf(df, coords = c("end_lng", "end_lat"), crs = 4326)


l <- st_cast(st_multipoint(matrix(c(df1_group$start_lng, df1_group$start_lat, df1_group$end_lng, df1_group$end_lat)))
        , "LINESTRING")

routes <- route(from = df1_start$geometry
                , to = df1_end$geometry
                , route_fun = route_osrm
                , osrm.profile = "bike")


df3 <- df2 |>
  #group_by(start_station_id, end_station_id, start_ln) |>
  count(start_station_id) |>
  ungroup()



join2 <- join |>
  group_by(start_station_id, end_station_id, ...6, d) |>
  summarize(count = n())

summary(df2$end_station_id %in% geo_end$end_station_id)

df2_end <- df2 |> 
  left_join(geo_end) |>
  

test <- route(from = df1$o
           , to = df1$d
           , route_fun = route_osrm
           , osrm.profile = "bike")


mapview::mapview(st_geometry(test))

df$o <- df1 %>%
  sf::st_as_sf(coords = c("start_lng", "start_lat")
                            , crs = 4326)

df$d <- df %>%  sf::st_as_sf(coords = c("end_lng", "end_lat")
                        , crs = 4326)

df_test <- df[1:25,]
df_route <- route(from = df_test$o
                  , to = df_test$d
                  , route_fun = route_osrm
                  , osrm.profile = "bike")

mapview::mapview(st_geometry(df_route))
