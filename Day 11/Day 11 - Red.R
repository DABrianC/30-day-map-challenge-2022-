#Day 11 - Red 

library(tanaka)
library(elevatr)
library(terra)
library(tidyverse)
library(sf)
library(showtext)

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

terra::plot(ras)

#set up a circle centered on Great Falls
center <- sf::st_as_sf(data.frame(x=long, y=lat), 
                   coords=(c("x","y")), 
                   crs = sf::st_crs(ras))
center <- sf::st_buffer(center, dist = 2700) 

test <- st_simplify(center)

#check to see that it is centered where it should be
terra::plot(center, add = T)

ras_cropped <- crop(ras, sf::st_bbox(center)[c(1,3,2,4)])

ras_mask <- mask(ras_cropped, mask = vect(center))

# custom color palette in red
pal <- RColorBrewer::brewer.pal(9, "Reds")

reds <- colorRampPalette(pal)
# display the map
tan <- tanaka_contour(x = ras_cropped
               , nclass = 9
               , mask = center)

#font
font_add_google(name = "Fredericka the Great"
                , family = "frederika")

#plot it
showtext_auto()
ggplot() +
  geom_sf(data = tan, aes(geometry = geometry
                          , fill = max
                          , col = max))+
  scale_fill_gradientn(colors = pal
                       , name = "Elevation (meters)") +
  scale_color_gradientn(colors = pal
                        , guide= "none") +
  geom_sf(data = center, aes(geometry = geometry)
          , linewidth = 1.5
          , col = pal[[9]]
          , fill = NA
          ) +
  theme_void() +
  labs(title = "Day 11: Great Falls Park, VA"
       , caption = "Data: AWS Terrain Tiles \n Visualized by: @bcalhoon7") +
  theme(plot.title = element_text(color = pal[[9]]
                             , family = "frederika"
                             , size = 38)
        , plot.caption = element_text(color = pal[[9]]
                                 , family = "frederika"
                                 , size = 16)
        , axis.text = element_blank()
        , axis.title = element_blank()
        , axis.ticks = element_blank()
        , plot.background =  element_blank()
        , panel.background = element_rect(fill = pal[[7]]
                                          , color = pal[[7]])
        , legend.title = element_text(color = pal[[9]]
                                      , family = "frederika"
                                      , size = 12)
        , legend.text = element_text(color = pal[[9]]
                                     , family = "frederika"
                                     , size = 12))


ggsave("./Day 11/Great Falls - red.png"
       , device = "png"
       , width = 5
       , height = 5
       , units = "in")
