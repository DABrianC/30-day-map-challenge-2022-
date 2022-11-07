# Day 7 - raster

#libraries
library(tidyverse)
library(crssuggest)
library(osmdata)
library(sf)
library(terra)
library(readxl)
library(lubridate)
library(glue)
library(showtext)


#add some fonts

font_add_google(name = "Fira Code", family = "fira")


#Plotting theme function pulled from previous work
theme.plot <- function(){
  require(extrafont)
  require(extrafontdb)
  require(ggplot2)
  list(  
    ggplot2::theme(legend.position = "right"
                   , legend.background = ggplot2::element_blank()
                   , legend.title = ggplot2::element_blank()
                   , legend.key = ggplot2::element_blank() 
                   , axis.ticks = ggplot2::element_blank()
                   , axis.line = ggplot2::element_blank()
                   , panel.grid.minor = ggplot2::element_blank()
                   , panel.grid.major.y = ggplot2::element_line(color = "#CFCDC9") 
                   , panel.grid.major.x = ggplot2::element_blank()
                   , panel.background = ggplot2::element_blank()
                   , plot.title.position = "plot" #Pushes the title to the very left of the plot window
                   , plot.title = element_text(size = 40, family = "fira", color = "#672044")
                   , plot.subtitle = element_text(size = 26, family = "fira", color = "#672044")
                   , strip.background = ggplot2::element_rect(fill = "white") 
                   , axis.text = ggplot2::element_text(size = 18, family = "fira", hjust = 0, color = "#000000")
                   , plot.caption = ggplot2::element_text(size = 16, family = "fira", color = "#672044"))
    #The colors below are from the cartocolor "Safe" palette plus 3 additional colors pulled from cartocolor.
    , ggplot2::scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                                            "#AA4499", "#44AA99", "#999933", "#882255", "#661100",
                                            "#6699CC", "#888888", "#764E9F", "#ED645A", "#edd9a3"))
    , ggplot2::scale_color_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                                             "#AA4499", "#44AA99", "#999933", "#882255", "#661100",
                                             "#6699CC", "#888888", "#764E9F", "#ED645A", "#edd9a3"))
  )}


#The data
path <- "Y:/Private/brian.calhoon/methods-corner/SouthSudan/"

events <- read_xlsx(glue(path, "data/SSudan.xlsx")) 


events$admin1 <- events$admin1 %>% 
  recode("Western Bahr El Ghazal" = "Western Bahr el Ghazal")

#Group the data by month for animating later
#Add a date column that is formatted as a date column
events$dates <- ymd(events$event_date) 

#group by months
events_month <- events %>% 
  mutate(month = lubridate::month(events$dates)
         , year = lubridate::year(events$dates)
         , myr = lubridate::my(paste0(month, "-", year))) 


#This version has a geometry column added to it
events_month_sf <- st_as_sf(events_month, coords = c("longitude", "latitude")
                      , crs = 4326) %>% 
  st_transform(crs=crs)

#create the summary column
#events_month2 <- events_month_sf %>% 
 # group_by(data_id, year, myr) %>% 
  #count()

#shape files for neighboring countries
neighbors <- rgeoboundaries::geoboundaries(country = c("Uganda", "Kenya", "Sudan", "Ethiopia", "Central African Republic", "Democratic Republic of the Congo", "South Sudan"))

# Shape file for The border of South Sudan
SSudan <- rgeoboundaries::geoboundaries(country = "South Sudan")

#Get the crs
crsuggest::suggest_crs(SSudan) #This is a suggested crs for South Sudan, 21096

crs <- 21096

#shape files for the counties of South Sudan
states <- rgeoboundaries::geoboundaries(country = "South Sudan"
                        , adm_lvl = "adm1") %>% 
  st_transform(crs = crs)

SSudan <- st_transform(SSudan, crs = crs)

bbox <- st_bbox(SSudan) #established the bounding box coordinates

#shape files for neighboring countries
neighbors <- rgeoboundaries::geoboundaries(country = c("Uganda", "Kenya", "Sudan", "Ethiopia", "Central African Republic", "Democratic Republic of the Congo", "South Sudan")) 

neighbors_v1 <- st_transform(neighbors, crs)

events_sf <- st_transform(events_sf, crs = crs)

#The following data come from naturalearth data, but the rnaturalearth::ne_download() call isn't working, so I downloaded the files manually. The hashtagged code that follows should work soon so I'm keeping it here. 

#places <- ne_download(scale = 10
#                      , type = #"populated_places"
#                      , returnclass = #"sf")

#unzip the files and then read them 
#unzip("data/ne_10m_populated_places.z#ip", exdir = "data/places")

places <- st_read(glue(path, "data/places/ne_10m_populated_places.shp"))

#filter for only South Sudan
places_SS <- places %>% 
  filter(SOV0NAME == "South Sudan") %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")
           , crs = crs) 

places_SS$FEATURECLA <- case_when(places_SS$FEATURECLA == "Populated place" ~ "city"
                                  , places_SS$FEATURECLA != "Populated place" ~ "capital") %>%
  as.factor()


#download water
#water <- ne_download(scale = 10
#                     , type = #"rivers_lake_centerlines"
#                     , category = #"physical"
#                    , returnclass = #"sf") 

#unzip rivers file and read it in
#unzip("data/ne_10m_rivers_lake_center#lines.zip", exdir = "data/rivers")

water <- st_read(glue(path, "data/rivers/ne_10m_rivers_lake_centerlines.shp")) %>% 
  st_transform(crs = crs)

#filter for the region
water_SS <- water[neighbors_v1,]

#download lakes
#lakes <- ne_download(scale = 10
#                     , type = "lakes"
#                     , category = #"physical"
#                     , returnclass = #"sf") 

#unzip lakes files and read in
#unzip("data/ne_10m_lakes.zip", exdir #= "data/lakes")

lakes <- st_read(glue(path, "data/lakes/ne_10m_lakes.shp")) %>% 
  st_transform(crs = crs)

#now transform these points into a raster
raster_template <- rast(ext(SSudan), resolution = 20000,
                        crs = st_crs(events_sf)$wkt)

raster1 <- rasterize(vect(events_month_sf)
                     , raster_template
                     ,  fun = "length")

raster2 <- terra::as.data.frame(raster1
                                , xy=TRUE
                                , cells = TRUE) %>% 
  select(cell, x, y, Events = lyr.1) %>%
  filter(Events > 0) %>% 
  mutate(events_logged = log(Events)) %>% 
  filter(events_logged > 0)

nudge_x = runif(15, min = .25, max = .75)
nudge_y = runif(15, min = -.75, max = -.25)

showtext_auto()
map_raster <- ggplot(data = neighbors_v1)+
  geom_sf(fill = "#F8F0E3") +
  geom_sf_label(data = neighbors_v1
                , aes(label = shapeName)) +
  geom_sf(data = states,
          color = "black"
          , fill = "#DEDEDE") +
  ggfx::with_outer_glow(geom_tile(data = raster2 #the raster object
            , aes(x = x
                  , y = y
                  , fill = events_logged)
            , alpha = .8))+
  geom_sf(data = places_SS
          , alpha = .5)+
  geom_sf_text(data = places_SS
               , aes(label = NAME_ID
                     , hjust = nudge_x
                     , vjust = nudge_y)
               #, nudge_x = nudge_x
               #, nudge_y = nudge_y
               , check_overlap = T
               , color = "#000000"
               , size = 8)+
  geom_sf(data = water
          , color = "steel blue") +
  geom_sf(data = lakes
          , color = "steel blue")+
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]]))+
  theme.plot() +
  scale_fill_gradient2(name = "Logged # of \nconflict events"
                        , low = "#e38191"
                        , high = "#672044")+
  labs(title = "Day 7: South Sudan"
       , subtitle = "Prevalence of conflict events (2011-2022)"
       , caption = "Data: ACLED | rgeoboundaries | rnaturalearth\nVisualized by: @DABrianC | #30DayMapChallenge"
       , x = ""
       , y = "")+
  theme(axis.text = element_blank()
        , legend.text = element_text(family = "fira"
                                   , size = 12
                                   , color = "#672044")
        , legend.title = element_text(family = "fira"
                                      , size = 18
                                      , color = "#672044")
        )

map_raster
ggsave(plot = map_raster
       , filename = "./Day 7/South Sudan Conflict.png"
       , height = 5
       , width = 7
       , unit = "in")

