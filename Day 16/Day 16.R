#Day 16 - minimal

library(tmap)
library(tmaptools)
library(rcartocolor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(showtext)
#some data from naturalearth
coasts <- ne_coastline(returnclass = "sf")

coasts <- st_transform(coasts, crs = "ESRI:54032")

#some minimal(ish) colors 

pal <- carto_pal(n = 7, "Earth")

#import minimalist font
font_add_google(name = "Montserrat",
                family = "montserrat")



showtext::showtext_auto()
tmap_mode(mode = "plot") 
map <- tm_shape(coasts, projection = "ESRI:54032") +
  tm_lines(col = pal[[4]]
           , lwd = .25)

mapped <- map +  tm_layout(main.title = "Day 16: Global coastlines"
                 , main.title.position = "center"
                 , main.title.size = 2
                 , bg.color = pal[[7]]
                 , main.title.fontfamily = "montserrat"
                 , outer.bg.color = pal[[7]]
          , frame = pal[[7]]
          , main.title.color = pal[[4]]
          ) +
  tm_credits("Data: naturalearthdata.com\nVisualized by: @bcalhoon7"
             , size = 1.5
             , col = pal[[4]]
             , position = c(0, 0)
             , fontfamily = "montserrat"             ) 

mapped

tmap_save(mapped, "./Day 16/minimal map.png"
          , height = 5
          , width = 5
          , unit = "in")
