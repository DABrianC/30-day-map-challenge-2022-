# Day 18 - Blue

library(tidyverse)
library(elevatr)
library(sf)
library(rcartocolor)
library(RColorBrewer)
library(terra)
library(showtext)

#font
font_add_google(name = "Covered By Your Grace"
                , family = "grace")

#river data
rivers <- st_read("./Day 18/HydroRIVERS_v10_eu_shp/HydroRIVERS_v10_eu.shp")

#france boundary
france <- st_read("./Day 13/gadm41_FRA_0.shp")

#bounding box of france
bbox <- st_bbox(france)

france_rivers <- st_join(rivers, france)

plot(france_rivers[[3]])

head(france_rivers)

summary(france_rivers$DIS_AV_CMS)

breaks <- c(0,.05, .191, .879, 8241.003)

hist(france_rivers$DIS_AV_CMS)
france_rivers1 <- france_rivers |>
  #mutate(log_length = log(LENGTH_KM)) |>
  filter(ORD_CLAS == 1)

#blue color palette from RColorBrewer
display.brewer.pal(n = 9, "Blues")

#get elevation data for France
elev <- get_elev_raster(france, z = 4
                        , prj = "EPSG:4326"
                        , clip = "locations")

#convert it to a spatvector
ras <- rast(elev)

#confirm that it's what I expect
plot(ras)

#mask the rivers dataframe to the france shape dataframe
rivers_mask <- mask(vect(france_rivers), mask = vect(france))

#write this to disk because it took a long time to mask
writeVector(rivers_mask, "./Day 18/france rivers.shp")



#Make the plot
showtext_auto()
ggplot()+
  ggfx::with_shadow(geom_sf(data = france, fill = "#F7FBFF")
              , color = "#F6FBFF") +
  tidyterra::geom_spatvector(data = rivers_mask, aes(col = DIS_AV_CMS)) +
  coord_sf(xlim = c(bbox[[1]], bbox[[3]])
           , ylim = c(bbox[[2]], bbox[[4]])) +
  scale_color_gradient(low = "#DEEBF7"
                     , high = "#08306B") +
  labs(title = "Day 18: Les Fleuves de France"
       , caption = "Data: hydrosheds.org | gadm.org | Visualized by: @bcalhoon7") +
  theme_void() +
  theme(legend.position = "none"
        , plot.background = element_rect(fill = "#6699CC"
                                         , color = "#6699CC")
        , plot.title = element_text(size = 46
                                    , color = "#08306B"
                                    , family = "grace")
        , plot.caption = element_text(size = 18
                                      , color = "#08306B"
                                      , family = "grace"))

ggsave("./Day 18/fleuves.png"
       , device = "png"
       , height = 5
       , width = 5
       , units = "in")                      

