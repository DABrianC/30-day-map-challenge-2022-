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

#admin boundaries
dc <- st_read("Day 1/DC/Single_Member_District_from_2023.shp")
boundary <- st_read("Day 1/DC/Washington_DC_Boundary.shp")


df <- df %>% 
  filter(!is.na(end_lat)
         , !is.na(end_lng)
         , !is.na(start_lat)
         , !is.na(start_lng)) 

df$started_at <- df$started_at %>% 
  mdy_hm()


df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S")
         , day = format(started_at, format = "%Y-%m-%d"))


#Eliminate weekdays
df$weekdays <- df$started_at %>% 
  weekdays()

df <- df[grepl("Saturday", df$weekdays) |
           grepl("Sunday", df$weekdays),] 


zones <- df |>
  select(o = start_station_id
         , d = end_station_id
         , start_lng
         , start_lat
         , end_lng
         , end_lat) |>
  filter(!is.na(o)
         , !is.na(d)
         , o != d)

dest <- zones |>
  group_by(o, d) |>
  summarize(count = n())

zones_dest <- left_join(dest, zones)

zone_start <- zones |>
  select(o, d, start_lng, start_lat) |>
  group_by(o,d, start_lng,start_lat) |>
  count() |>          
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326) |>
  select(o, n, geometry) |>
  arrange(desc(n)) 

zone1 <- zone_start[1:500,]

zone_end <- zones |>
  select(o, d, end_lng, end_lat) |>
  group_by(o,d, end_lng,end_lat) |>
  count() |>          
  st_as_sf(coords = c("end_lng", "end_lat"), crs = 4326) |>
  select(d, n, geometry) |>
  arrange(desc(n))

zone2 <- zone_end[1:500,]

#THis works
lines <- stplanr::route(from = zone1
                        , to = zone2
                        , route_fun = route_osrm
                        , osrm.profile = "bike")

#save lines to the files so I don't have to keep downloading it

st_write(obj = lines
         , dsn = "Day 2/lines.shp")

b <- st_read("Day 2/lines.shp")

class(b)

ggplot() +
  with_outer_glow(geom_sf(data = boundary, fill = "#FFFFFF", color = "blue")) +
  with_outer_glow(geom_sf(data = b, aes(geometry = st_geometry(geometry)
                        , color = "#E81B39"))) +
  theme_void() + 
  theme(legend.position = "none")

mapview::mapview(st_geometry(b))

tmap_view("plot")

