#Day 1 - points

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(rcartocolor)
library(ggfx)
library(lubridate)
library(ggtext)
library(patchwork)
library(showtext)
set.seed(11012022)
#Load some fonts----

#from Google


font_add_google(name = "Amatic SC", family = "amatic-sc")

carto_colors <- rcartocolor::carto_pal(12, "Vivid")

colors <- c(carto_colors[[8]], carto_colors[[11]])
#read in bikeshare data. 
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("Day 1/bikeshare data sept 2022.csv")


#read in the administrative boundaries
#accessed here: https://opendata.dc.gov/datasets/DCGIS::advisory-neighborhood-commissions-from-2023/about
dc <- st_read("Day 1/DC/Single_Member_District_from_2023.shp")
boundary <- st_read("Day 1/DC/Washington_DC_Boundary.shp")

#read in the roads
#https://opendata.dc.gov/datasets/e8299c86b4014f109fedd7e95ae20d52/explore?location=38.893605%2C-77.019147%2C12.00
roads <- st_read("Day 1/DC/Roads.shp")

#remove any missing coordinates
#564 missing end_lat and end_lng coordinates
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


#Eliminate weekends
df$weekdays <- df$started_at %>% 
  weekdays()

df <- df[!grepl("Saturday", df$weekdays) &
            !grepl("Sunday", df$weekdays),] 
         
#make some geometry list columns

#create a bbox to use in coord_sf below.
bbox = st_bbox(boundary)


df_sf <- df %>% 
  st_as_sf(coords = c("start_lng", "start_lat")
           ,crs = 4326)

df_sf2 <- df %>% 
  st_as_sf(coords = c("end_lng", "end_lat")
           , crs = 4326)

#filter for morning rides
df_morn <- df_sf %>% 
  filter(time >= "06:00:00" & time <= "09:00:00")

#df_morn2 <- left_join(df_morn, df, by = "ride_id")

#df_morn2 <- df_morn %>% 
 # st_as_sf(coors = c("end_lng", "end_lat")
  #         , crs = 4326)

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
  arrange(desc(avg)) %>% 
  slice_head(n = 100) #taking only the top 100 stations after averaging by day

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
  arrange(desc(avg)) %>% 
  slice_head(n = 100) #taking only the top 100 stations after averaging by day

p <- ggplot()+
  geom_sf(data = dc, color = "white", fill = "black"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_morn2, aes(geometry = st_jitter(geometry))
                          , color = colors[[1]]
                          , alpha = .5)) +
  with_outer_glow(geom_sf(data = df_morn_end2, aes(geometry = st_jitter(geometry))
                          , color = colors[[2]]
                          , alpha = .5)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void() +
  #scale_color_manual(name = "Start and End\nStations"
   #                   , values = colors)+
  #labs(subtitle = "Mornings: \n6 am - 9 am") +
  theme(plot.subtitle = element_text(size = 20
                                     , family = "amatic-sc")
        )

p_plus <- p + annotate(geom = "text"
                         , x = bbox[[1]]+.019
                         , y = bbox[[4]]-.02
                         , label = "Mornings"
                         , family = "amatic-sc"
                         , size = 16)


#Evening rides in Sept-----

#create a geometry column out of the lng and lat columns
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
  arrange(desc(avg)) %>% 
  slice_head(n = 100) #taking only the top 100

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
  arrange(desc(avg)) %>% 
  slice_head(n = 100) #take only the top 100

#plot the evening cycle stations

showtext_auto()
p1 <- ggplot()+
  geom_sf(data = dc, fill = "black", color = "white"
          , size = .25) +
  with_outer_glow(geom_sf(data = df_even2, aes(geometry = st_jitter(geometry))
                          , color = colors[[1]]
                          , alpha = .5)) +
  with_outer_glow(geom_sf(data = df_even_end2, aes(geometry = st_jitter(geometry))
                          , color = colors[[2]]
                          , alpha = .5)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void() +
  theme(plot.subtitle = element_text(size = 24
                                     , family = "amatic-sc"))
p1_plus <- p1 + annotate(geom = "text"
             , x = bbox[[1]]+.019
             , y = bbox[[4]]-.02
             , label = "Evenings"
             , family = "amatic-sc"
             , size = 16)
patch <- p_plus + p1_plus # n = 100

#subtitle object to put in the plot_annotation below
subtitle <- "Capital Bikeshare weekday commuters' <span style = 'color:#2F8AC4'>**start stations**</span> and <span style = 'color:#CC3A8E'>**end stations**</span>."
  
showtext_auto()
plot <- patch + plot_annotation(
  title = "Day 1: DC's top 100 Capital Bikeshare stations, September 2022"
  , subtitle = subtitle
  , caption = "Data: Capital Bikeshare & Open Data DC \nAuthor: @BCalhoon7, #30DayMapChallenge"
) & theme(plot.title = ggtext::element_markdown(size = 44
                                    , face = "bold"
                                    , family = "amatic-sc")
          , plot.subtitle = ggtext::element_markdown(size = 28
                                         , family = "amatic-sc")
          , plot.title.position = "panel"
          , plot.caption = element_text(size = 18
                                        , family = "amatic-sc")
          , plot.background = element_rect(fill = "#d6bd8d")
) 

ggsave(plot = plot
       , filename = "./Day 1/DC bike commuters.png"
       , height = 4
       , width = 6
       , unit = "in")

