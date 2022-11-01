#Day 2 - lines

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(ggfx)
library(lubridate)
library(ggtext)
library(showtext)
library(osmdata)
library(stplanr)

set.seed(11022022)
#Load some fonts----

#from Google

font_add_google(name = "Amatic SC", family = "amatic-sc")

#read in bikeshare data. 
#accessed here: https://s3.amazonaws.com/capitalbikeshare-data/index.html
df <- read_csv("./Day 1/bikeshare data sept 2022.csv")

#admin boundaries
dc <- st_read("Day 1/DC/Single_Member_District_from_2023.shp")
boundary <- st_read("Day 1/DC/Washington_DC_Boundary.shp")

#filter out the missing longitudes and latitudes
df <- df %>% 
  filter(!is.na(end_lat)
         , !is.na(end_lng)
         , !is.na(start_lat)
         , !is.na(start_lng)) 

#format start_at as a date time column
df$started_at <- df$started_at %>% 
  mdy_hm()

#create a column for time and one for day
df <- df %>% 
  mutate(time = format(started_at, format = "%H:%M:%S")
         , day = format(started_at, format = "%Y-%m-%d"))


#Eliminate weekdays
df$weekdays <- df$started_at %>% 
  weekdays()

#filter just for Saturday and Sunday in the weekdays column
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


#Some aggregating here, but something isn't quite right about how I 
# handled the counting so be careful if copying. Help is always welcome :)
zone_start <- zones |>
  select(o, d, start_lng, start_lat) |>
  group_by(o,d, start_lng,start_lat) |>
  count() |>          
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326) |>
  select(o, n, geometry) |>
  arrange(desc(n)) 

zone_start10 <- zone_start |> filter(n >9)

zone_end <- zones |>
  select(o, d, end_lng, end_lat) |>
  group_by(o,d, end_lng,end_lat) |>
  count() |>          
  st_as_sf(coords = c("end_lng", "end_lat"), crs = 4326) |>
  select(d, n, geometry) |>
  arrange(desc(n))

zone_end10 <- zone_end[1:1161,]

#THis works
lines <- stplanr::route(from = zone_start10
                        , to = zone_end10
                        , route_fun = route_osrm
                        , osrm.profile = "bike")

#save lines to the files so I don't have to keep downloading it
st_write(obj = lines
         , dsn = "Day 2/lines.shp")


#plot it and use colors of the DC flag 
#DC flag red = #E81B39
showtext_auto()
p <- ggplot() +
  with_outer_glow(geom_sf(data = boundary, fill = "#FFFFFF", color = "#000000")) +
  with_outer_glow(geom_sf(data = lines, aes(geometry = geometry)
          , color = "#E81B39"
          , alpha = .4
          , size = .7))+
  labs(title = "<span style = 'color:#E81B39'>Day 2:What routes do weekend cyclists in DC frequent?</span>"
       , subtitle = "<span style = 'color:#E81B39'>Weekends in September 2022</span>"
       , caption = "<span style = 'color:#E81B39'>Data: Capital Bikeshare; OpenStreetMap; Open Source Routing Machine <br>Visualized by: @Bcalhoon7 #30DayMapChallenge<span style = 'color:#E81B39'>")+
  theme_void() + 
  theme(legend.position = "none"
        , plot.title.position = "plot"
        ,plot.title = ggtext::element_markdown(size = 44
                                               , face = "bold"
                                               , family = "amatic-sc")
        , plot.subtitle = ggtext::element_markdown(size = 28
                                                   , face = "bold"
                                                   , family = "amatic-sc")
        , plot.caption = ggtext::element_markdown(size = 24
                                                  , face = "bold"
                                      , family = "amatic-sc")
        , plot.background = element_rect("lightgrey"))

#save the plot
ggsave(plot = p
       , filename = "Day 2/cycle routes.png"
       , height = 5.9
       , width = 4
       , unit = "in")

#small version for readme
ggsave(plot = p
       , filename = "Day 2/cycle routes readme.png"
       , height = 2.45
       , width = 2
       , unit = "in")
