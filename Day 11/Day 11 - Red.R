#Day 11 - Red 

library(tanaka)
library(elevatr)
library(terra)
library(tidyverse)
library(sf)

#clicked on Google maps to find Great Falls
xmin <- -77.267301
ymin <-  38.956971
xmax <- -77.180784
ymax <- 39.006611

#center points
long <- (xmin + xmax)/2
lat <- (ymin + ymax)/2

# use elevatr to get elevation data
dem <- get_elev_raster(locations = data.frame(x = c(xmin, xmax)
                                              , y = c(ymin,ymax)),
                z = 14, prj = "EPSG:4326", clip = "locations")

#convert to a spatraster
ras <- rast(dem)

plot(dem)

#set up a circle centered on Great Falls
center <- sf::st_as_sf(data.frame(x=long, y=lat), 
                   coords=(c("x","y")), 
                   crs = sf::st_crs(ras))
center <- sf::st_buffer(center, dist = 2500) 

ras_cropped <- crop(ras, sf::st_bbox(center)[c(1,3,2,4)])

ras_mask <- mask(ras_cropped, mask = vect(center))

# custom color palette in red


pal <- RColorBrewer::brewer.pal(7, "Reds")

# display the map
tanaka(ras_mask
       
       , breaks = seq(from = 15
                       ,to = 122, 15)
       , col = pal
       , legend.pos = "none"
       )

mapsf::mf_export(x = center
                 , filename = "./Day 11/circle.png"
                 , width = 800, res = 100)
tanaka::tanaka(x=ras_mask
               , breaks = seq(from = 15
                              ,to = 122, 15)
               , col = pal
               , legend.pos = "none")
mapsf::mf_map(center, add = TRUE, border = "white", col = NA, lwd = 6)


ggsave("./Day 11/Great Falls - red.png")
