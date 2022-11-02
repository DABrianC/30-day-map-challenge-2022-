#Day 3 - polygons

#bikeshare use in DC - Sept 2022 - Capital bikeshare

library(tidyverse)
library(sf)
library(MetBrewer)
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



#The above plot is ok, but this plot is cleaner

lanes <- st_read("Day 3/Bicycle_Lanes.shp")


#join the bike lanes to ANC polygons 
#and count how many are in each ANC
join <- dc |> st_join(lanes) |>
  group_by(ANC_ID.x) |>
  count() 

hok <- met.brewer(name = "Hokusai2"
                  , type = "continuous")

showtext_auto()
p <- ggplot() +
  geom_sf(data = join, aes(geometry = geometry
                    , fill = n)
          , color = "white"
          , size = 0) +
  geom_sf(data = lanes, alpha = .3, size = .5)+
  theme_void()+
  scale_fill_gradientn(name = "# of bike lanes\nin each ANC"
    , colors=met.brewer("Hokusai2")
    , breaks = c(25, 250)
    , labels = c("Less", "More")) +
  labs(title = "Does your DC neighborhood have enough bike lanes?"
       , subtitle = "Official bike lanes in grey"
       , caption = "Data: Open Data DC\nVisualized by: @bcalhoon7 #30DayMapChallenge") +
  theme(plot.title.position = "plot"
        , plot.title = element_text(family = "amatic-sc"
                                    , size = 40
                                    , face = "bold"
                                    , color = hok[[6]])
        , plot.subtitle = element_text(family = "amatic-sc"
                                       , size = 34
                                       , face = "bold"
                                       , color = "grey")
        , plot.caption = element_text(family = "amatic-sc"
                                      , size = 24
                                      , face = "bold"
                                      , color = hok[[6]]
                                      , hjust = .5)
        , legend.text = element_text(family = "amatic-sc"
                                     , size = 24
                                     , color = hok[[6]])
        , legend.title = element_text(family = "amatic-sc"
                                      , size = 28
                                      , face = "bold"
                                      , color = hok[[6]])
  )
  

ggsave(plot = p
       , filename = "Day 3/cycle routes by ANC.png"
       , height = 4
       , width = 4
       , unit = "in"
       , bg = "white")

###Discarded this plot - it works, but it didn't say much-----
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
porig <- ggplot() +
  geom_sf(data = orig_anc, aes(geometry = geometry
                               , fill = log(n))
          , color = "grey"
          , size = 0) +
  theme_void()

pdest <- ggplot() +
  geom_sf(data = dest_anc, aes(geometry = geometry
                               , fill = log(n))
          , color = "grey"
          , size = 0) +
  theme_void() 

#combine the two plots
plots <- porig + pdest

plots + plot_annotation(
  title = "Great title"
  , subtitle = "informative subtitle"
  , caption = "Data: Open Data DC, Capital Bikeshare\n
  Visualized by: @bcalhoon7 #30DayMapChallenge") +
  plot_layout(guides = "collect") &
  scale_fill_carto_c(limits = range(c(log(orig_anc$n), log(dest_anc$n))
                                    , na.rm = T)
                     , name = "# of bikes hired \nand returned"
                     , type = "continuous"
                     , palette = "Sunset"
                     , direction = -1)
