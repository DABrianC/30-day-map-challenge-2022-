#Day 8 - OpenStreetMap

library(osmdata)
library(tidyverse)
library(rjson)
library(jsonlite)
library(sf)


#read in data from https://overpass-turbo.eu/
dat_sf <- st_read("./Day 8/annecy.geojson", drivers = "GeoJSON")

plot(dat_sf)
