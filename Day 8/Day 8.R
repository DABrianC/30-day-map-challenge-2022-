#Day 8 - OpenStreetMap

library(osmdata)
library(tidyverse)
library(rjson)
library(jsonlite)
library(sf)
library(lidR)
library(elevatr)
library(rayshader)
library(osmplotr)


#read in data from https://overpass-turbo.eu/
dat_sf <- st_read("./Day 8/annecy.geojson", drivers = "GeoJSON")

crs <- st_crs(dat_sf)
bbox <- st_bbox(dat_sf) 

elev <- get_elev_raster(dat_A, z = 5, crs = crs)

mat <- raster_to_matrix(elev)

w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

#pal <- "ukraine"


#c1 <- natparks.pals("Glacier")
#c2 <- rcartocolor::carto_pal(7, "PinkYl")
#yellows <- c("#f1ee8e", "#e8e337", "#ffd700", "#ffd700")

colors <- rcartocolor::carto_pal(7, "Burg")


rgl::rgl.close()
mat %>%
  #height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  height_shade() |>
  plot_3d(heightmap = mat, 
          solid = FALSE, 
          z = 8,
          shadowdepth = 50,
          windowsize = c(800*wr,800*hr), 
          #add_water(detect_water(mat)),
          phi = 90, 
          zoom = 1, 
          theta = 0, 
          background = "white") 

# Use this to adjust the view after building the window object
render_camera(phi = 90, zoom = .7, theta = 0)



bbox <- get_bbox(c(bbox[[1]], bbox[[3]], bbox[[2]], bbox[[4]]))

df <- opq(bbox = bbox) |>
      add_osm_feature(key = "route"
                      , value = "bicycle") |>
      osmdata_sf()

df1 <- opq(bbox = bbox) |>
  add_osm_feature(key = "water"
                  , value = "lake") |>
  osmdata_sf()

plot(df1$osm_multipolygons)

basemap <- osmplotr::osm_basemap(bbox = bbox
                                 , bg = "gray20")
dat_B <- extract_osm_objects (key = "water", bbox = bbox)
dat_A <- extract_osm_objects(key = "building", bbox = bbox)


map <- basemap
map <- add_osm_objects(map, dat_B, col = "blue")
map <- add_osm_objects(map, dat_A, col = "green")
osmplotr::print_osm_map(map)


bbox <- get_bbox (c(-0.15, 51.5, -0.10, 51.52))
dat_B <- extract_osm_objects (key = "building", bbox = bbox)
map <- osm_basemap (bbox = bbox, bg = "gray20")
map <- add_osm_objects (map, dat_B, col = "gray40")
print_osm_map (map)
