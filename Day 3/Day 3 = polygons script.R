#Day 3 - polygons

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(rcartocolor)
library(ggfx)
library(lubridate)
library(ggtext)
library(patchwork)
library(showtext)
library(viridis)
library(viridisLite)
set.seed(11012022)
#Load some fonts----

#from Google


font_add_google(name = "Amatic SC", family = "amatic-sc")

carto_colors <- rcartocolor::carto_pal(7, "SunsetDark")


#read in bikeshare data. 
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("Day 1/bikeshare data sept 2022.csv")


#read in the administrative boundaries
#accessed here: https://opendata.dc.gov/datasets/DCGIS::advisory-neighborhood-commissions-from-2023/about
dc <- st_read("Day 1/DC/Single_Member_District_from_2023.shp")

#remove missing start and end coords
df <- df %>% 
  filter(!is.na(end_lat)
         , !is.na(end_lng)
         , !is.na(start_lat)
         , !is.na(start_lng)) 

df$started_at <- df$started_at %>% 
  mdy_hm()

#create columns for time and day
df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S")
         , day = format(started_at, format = "%Y-%m-%d"))


#add a days of the week column
df$weekdays <- df$started_at %>% 
  weekdays()

#Convert lon and lat to geometry column and then sum by id
#THis is starting points
orig <- df |>
  select(id = start_station_id, lat = start_lat, lon = start_lng) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  group_by(id) |>
  count()

#Join to dc anc map
orig_anc <- dc |> st_join(orig)

#Repeat above steps for ending points
dest <- df |>
  select(id = end_station_id, lat = end_lat, lon = end_lng) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  group_by(id) |>
  count()

#Join to dc anc map
dest_anc <- dc |> st_join(dest)

#Plot the two maps
ggplot() +
  geom_sf(data = orig_anc, aes(geometry = geometry
          , fill = n)
          , color = "white"
          , size = 0) +
  theme_void() +
  scale_fill_viridis_c(option = "inferno"
                       , direction = -1)


scale_fill_carto_c(name = ""
                   , type = "continuous"
                   , palette = "Purp"
                   , direction =1)


ggplot() +
  geom_sf(data = dest_anc, aes(geometry = geometry
                               , fill = n)
          , color = "white"
          , size = 0) +
  theme_void() +
  scale_fill_carto_c(name = ""
                     , type = "continuous"
                     , palette = "PurpOr"
                     , direction =1)
