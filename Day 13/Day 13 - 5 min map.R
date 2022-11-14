#Day 13 - 5 min map

library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)


#borders of France
france <- st_read("./Day 13/gadm41_FRA_1.shp")

#train stations in France
gares <- read_excel("./Day 13/Registre gares routieres.xlsx"
                    , sheet = 2
                    , col_names = TRUE)

#Longitude and Latitude are class character in the
#gares object so I coerce them to numeric.
gares$Longitude <- as.numeric(gares$Longitude)
gares$Latitude <-  as.numeric(gares$Latitude)

#convert coordinate columns to sf geometry column
gares_sf <- gares |>
  filter(!is.na(Longitude) & !is.na(Latitude)) |>
  st_as_sf(coords = c(long = "Longitude", lat = "Latitude")
           , crs = 4326)

#extract the bounding box of france
bbox <- st_bbox(france)

#plot
ggplot() +
  geom_sf(data = france, aes(geometry = geometry)) +
  geom_sf(data = gares_sf) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  theme_void() +
  labs(title = "5 min map: Gares de France"
       , subtitle = "Quickly made"
       , caption = "Data: data.gouv.fr | gadm.org\n
       Visualized by: @bcalhoon7")

ggsave("./Day 13/gares de france.png"
       , height = 5
       , width = 5
       , unit = "in")
