#Day 1 - points

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(rcartocolor)
library(ggfx)

#read in bikeshare data. 
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("bikeshare data sept 2022.csv")

#remove any missing coordinates
#564 missing end_lat and end_lng coordinates
df <- df %>% 
  filter(!is.na(end_lat)
         , !is.na(end_lng)
         , !is.na(start_lat)
         , !is.na(start_lng)) 

#select one day to work with - 9/23/2022
df <- df %>% 
  filter(grepl("9/23/2022", started_at))

#make some geometry list columns
df1 <- df %>% 
  st_as_sf(coords = c("start_lng", "start_lat")
           ,crs = 4326)

df2 <- df %>% 
  st_as_sf(coords = c("end_lng", "end_lat")
           , crs = 4326)

df3 <- bind_cols(df1, df2$geometry) %>% 
  rename("end" = "...13"
         , "start" = "geometry")

df_group<- df1 %>% 
  group_by(start_station_id, start_station_name) %>% 
  count()

df_group2 <- df2 %>% 
  group_by(end_station_id, end_station_name) %>% 
  count()
#read in the administrative boundaries
dc <- st_read("DC/Single_Member_District_from_2023.shp")

#create a bbox to use in coord_sf below
bbox = st_bbox(dc)

#
ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .5) +
  with_outer_glow(geom_sf(data = df_group, aes(geometry = st_jitter(geometry))
          , color = "yellow"
          , alpha = .3)) +
  with_outer_glow(geom_sf(data = df_group2, aes(geometry = st_jitter(geometry))
          , col = "red"
          , alpha = .3)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
                  , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void()
  
  

