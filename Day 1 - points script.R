#Day 1 - points

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(rcartocolor)
library(ggfx)
library(terra)
library(maptiles)
library(extrafont)
library(extrafontdb)
library(showtext)

#Load some fonts----

#from Google


font_add_google(name = "Amatic SC", family = "amatic-sc")
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

#get dc maptile from maptiles package (requires terra)

tile <- get_tiles(dc
                  , provider = "CartoDB.DarkMatterNoLabels"
                  , crop = TRUE)

plot(tile)


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


###Testing out the monthly morning commute avg., Sept. 2022

#read in bikeshare data. 
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("bikeshare data sept 2022.csv")



df <- df %>% 
  filter(!is.na(start_lat)
         , !is.na(start_lng)) 

df$started_at <- df$started_at %>% 
  mdy_hm()

#create columns for time and day
df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S")
         , day = format(started_at, format = "%Y-%m-%d"))

#create a geometry column out of te lng and lat columns
df_sf <- df %>% 
  st_as_sf(coords = c("start_lng", "start_lat")
           ,crs = 4326)

#filter for just morning rides
df_morn <- df_sf %>% 
  filter(time >= "06:00:00" & time <= "09:00:00")

#group by bike station and avg. rides starting there in the morning
df_morn2 <- df_morn %>% 
  select(ride_id
         , start_station_name
         , start_station_id
         , time
         , day
         , geometry) %>%
  filter(!is.na(start_station_name)) %>% 
  group_by(day, start_station_name, start_station_id) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(start_station_name) %>% 
  summarize(avg = mean(count)) %>% 
  slice_head(n = 100) #taking only the top 40

#Then, repeat for the end rides but have to delete missing lng and lat
df_sf_end <- df %>% 
  filter(!is.na(end_lng)
         , !is.na(end_lat)) %>% 
  st_as_sf(coords = c("end_lng", "end_lat")
           ,crs = 4326)

df_morn_end <- df_sf_end %>% 
  filter(time >= "06:00:00" & time <= "09:00:00")

df_morn_end2 <- df_morn_end %>% 
  select(ride_id
         , end_station_name
         , end_station_id
         , time
         , day
         , geometry) %>%
  filter(!is.na(end_station_name)) %>% 
  group_by(day, end_station_name, end_station_id) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(end_station_name) %>% 
  summarize(avg = mean(count)) %>% 
  slice_head(n = 100)


p <- ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_morn2, aes(geometry = st_jitter(geometry))
                          , color = "yellow"
                          , alpha = .6)) +
  with_outer_glow(geom_sf(data = df_morn_end2, aes(geometry = st_jitter(geometry))
                          , col = "red"
                          , alpha = .2)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void() +
  #labs(subtitle = "Mornings: \n6 am - 9 am") +
  theme(plot.subtitle = element_text(size = 20
                                     , family = "amatic-sc")
        )

p_plus <- p + annotate(geom = "text"
                         , x = bbox[[1]]+.015
                         , y = bbox[[4]]-.02
                         , label = "Mornings"
                         , family = "amatic-sc"
                         , size = 16)


#Evening rides in Sept-----

#create a geometry column out of te lng and lat columns
df_sf <- df %>% 
  st_as_sf(coords = c("start_lng", "start_lat")
           ,crs = 4326)

#filter for just morning rides
df_even <- df_sf %>% 
  filter(time >= "16:00:00" & time <= "19:00:00")

#group by bike station and avg. rides starting there in the morning
df_even2 <- df_even %>% 
  select(ride_id
         , start_station_name
         , start_station_id
         , time
         , day
         , geometry) %>%
  filter(!is.na(start_station_name)) %>% 
  group_by(day, start_station_name, start_station_id) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(start_station_name) %>% 
  summarize(avg = mean(count)) %>% 
  slice_head(n = 100) #taking only the top 40

#Then, repeat for the end rides but have to delete missing lng and lat
df_sf_end <- df %>% 
  filter(!is.na(end_lng)
         , !is.na(end_lat)) %>% 
  st_as_sf(coords = c("end_lng", "end_lat")
           ,crs = 4326)

df_even_end <- df_sf_end %>% 
  filter(time >= "16:00:00" & time <= "19:00:00")

df_even_end2 <- df_even_end %>% 
  select(ride_id
         , end_station_name
         , end_station_id
         , time
         , day
         , geometry) %>%
  filter(!is.na(end_station_name)) %>% 
  group_by(day, end_station_name, end_station_id) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(end_station_name) %>% 
  summarize(avg = mean(count)) %>% 
  slice_head(n = 100)

#plot the evening cycle stations

showtext_auto()
p1 <- ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_even2, aes(geometry = st_jitter(geometry))
                          , color = "yellow"
                          , alpha = .6)) +
  with_outer_glow(geom_sf(data = df_even_end2, aes(geometry = st_jitter(geometry))
                          , col = "red"
                          , alpha = .2)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  #annotate(geom = "text", aes(x = bbox[[1]], y = bbox[[4]]), label = "Evenings: \n4 pm - 7 pm")+
  theme_void() +
  #labs(subtitle = "Evenings) + 
  theme(plot.subtitle = element_text(size = 24
                                     , family = "amatic-sc"))
p1_plus <- p1 + annotate(geom = "text"
             , x = bbox[[1]]+.015
             , y = bbox[[4]]-.02
             , label = "Evenings"
             , family = "amatic-sc"
             , size = 16)
patch <- p_plus + p1_plus # n = 100

#subtitle object to put in the plot_annotation below
subtitle <- ggtext::element_markdown()
  
showtext_auto()
patch + plot_annotation(
  title = "Day 1: DC's top 100 Capital Bikeshare stations, September 2022"
  , subtitle = subtitle
  , caption = "Data: Capital Bikeshare, https://s3.amazonaws.com/capitalbikeshare-data/index.html \nAuthor: @BCalhoon7 \n #30DayMapChallenge"
) & theme(plot.title = element_text(size = 44
                                    , face = "bold"
                                    , family = "amatic-sc")
          , plot.title.position = "panel"
          , plot.caption = element_text(size = 16
                                        , family = "amatic-sc")
) 
