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

#create a bbox to use in coord_sf below.
bbox = st_bbox(dc)

#plot it
p <- ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_group, aes(geometry = st_jitter(geometry))
          , color = "yellow"
          , alpha = .6)) +
  with_outer_glow(geom_sf(data = df_group2, aes(geometry = st_jitter(geometry))
          , col = "red"
          , alpha = .4)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
                  , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void()

#filter just for morning rush hours - 6 am - 9 am on 9/23/22------
library(lubridate)

df$started_at <- df$started_at %>% 
  mdy_hm()

df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S"))

df_sf <- df %>% 
  st_as_sf(coords = c("start_lng", "start_lat")
           ,crs = 4326)

df_sf2 <- df %>% 
  st_as_sf(coords = c("end_lng", "end_lat")
           , crs = 4326)


df_morn <- df_sf %>% 
  filter(time >= "06:00:00" & time <= "09:00:00")

df_morn2 <- left_join(df_morn, df, by = "ride_id")

df_morn2 <- df_morn2 %>% 
  st_as_sf(coors = c("end_lng", "end_lat")
           , crs = 4326)
p_morn <- ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_morn, aes(geometry = st_jitter(geometry))
                          , color = "yellow"
                          , alpha = .6)) +
  with_outer_glow(geom_sf(data = df_morn2, aes(geometry = st_jitter(geometry))
                          , col = "red"
                          , alpha = .2)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void() +
  labs(title = "DC: Morning bike commutes"
       , subtitle = "September 23, 2022, 6 am - 9 am")

p_morn 

#filter just for evening rush hours - 4 pm - 7 pm on 9/23/22------
library(lubridate)

df$started_at <- df$started_at %>% 
  mdy_hm()

df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S"))

df_sf <- df %>% 
  st_as_sf(coords = c("start_lng", "start_lat")
           ,crs = 4326)

#df_sf2 <- df %>% 
 # st_as_sf(coords = c("end_lng", "end_lat")
  #         , crs = 4326)


df_even <- df_sf %>% 
  filter(time >= "16:00:00" & time <= "19:00:00")

df_even2 <- left_join(df_even, df, by = "ride_id")

df_even2 <- df_even2 %>% 
  st_as_sf(coors = c("end_lng", "end_lat")
           , crs = 4326)
p_even <- ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_even, aes(geometry = st_jitter(geometry))
                          , color = "yellow"
                          , alpha = .6)) +
  with_outer_glow(geom_sf(data = df_even2, aes(geometry = st_jitter(geometry))
                          , col = "red"
                          , alpha = .2)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void() +
  labs(title = "DC: Evening bike commutes"
       , subtitle = "September 23, 2022, 4 pm - 7 pm")

p_even

library(patchwork)

p_morn + p_even
