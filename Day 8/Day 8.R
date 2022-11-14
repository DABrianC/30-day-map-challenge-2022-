#Day 8 - OpenStreetMap

library(osmdata)
library(tidyverse)
library(rjson)
library(jsonlite)
library(sf)
library(lidR)
library(elevatr)
library(rayshader)
library(nationalparkcolors)
library(glue)

#bounding box for Annecy Lake more or less
bbox <- get_bbox(c(6.1081, 45.7795, 6.2533, 45.9148))

buildings <- st_read("./Day 8/annecy.geojson", drivers = "geojson")

#download osm data
cycle <- opq(bbox) |>
  add_osm_feature(key = "route"
                  , value = "bicycle") |>
  osmdata_sf()

water <- opq(bbox) |>
  add_osm_feature(key = "water"
                  , value = c("canal", "lake", "river")) |>
  osmdata_sf()

z<- 14
map <- "annecy"

elev <- get_elev_raster(water, z = z, crs = crs)

elev <- crop(elev, bbox)

#set crs to same as elev
cycle <- sf::st_transform(cycle$osm_multilines, crs = crs(elev))

water <- sf::st_transform(water$osm_multipolygons, crs = crs(elev))


buildings <- sf::st_transform(buildings, crs = crs(elev))
#test to see if all shapes align now that they are transformed


plot(elev)
plot(st_geometry(water), add = T, col = "blue")
plot(st_geometry(cycle), add = T, col = "red", ext = extent(water))
#convert elev raster to a matrix
mat <- raster_to_matrix(elev)


w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))


colors <- park_palette("ArcticGates", 6)

pal <- "ArcticGates"

#plot and work through the kinks here
rgl::rgl.close()
mat |>
  height_shade() |>
  add_overlay(generate_polygon_overlay(geometry = water
                                       , extent = attr(elev, "extent")
                                       , heightmap = elev
                                      , palette = colors[[2]]
                                       , linewidth = NA)
                                       ) |>
  add_overlay(generate_line_overlay(geometry = cycle
                                    , extent = attr(elev, "extent")
                                    , heightmap = elev
                                    , color = colors[[6]]
                                    , linewidth = 8)
                                    ) |>
 plot_3d(heightmap = mat 
          , solid = FALSE 
          , z = 2
          , shadowdepth = 50
          , windowsize = c(800*wr,800*hr) 
          , phi = 90
          , zoom = 1 
          , theta = 0 
          , background = "white") 

render_label(mat, x = 1475, y = 1400, z = 12000,
              zscale = 50, text = "Lac"
             , textsize = 1, textcolor = "darkblue"
             , family = "sans", alpha = .6, 
             linecolor = colors[[2]], clear_previous = T)
render_label(mat, x = 1550, y = 1500, z = 12000,
             zscale = 50, text = "d'Annecy"
             , textsize = 1, textcolor = "darkblue"
             , family = "sans", alpha = .6
             , linecolor = colors[[2]])
render_label(mat, x = 525, y = 450, z = 20000
             , zscale = 50, text = "Annecy"
             , textsize = 1, textcolor = "black"
             , family = "sans", alpha = .6
             , linewidth = 1, linecolor = "black")
render_label(mat, x = 1800, y = 1200, z = 20000
             , zscale = 50, text = "Veyrier-du-lac"
             , textsize = 1, textcolor = "black"
             , family = "sans", alpha = .6
             , linewidth = 1, linecolor = "black")

# Use this to adjust the view after building the window object
render_camera(phi = 45, zoom = .7, theta = 25, fov = 60)
render_snapshot()

###---render high quality
if (!dir.exists(glue("Day 8/{map}"))) {
  dir.create(glue("Day 8/{map}"))
}

outfile <- stringr::str_to_lower(glue("./Day 8/{map}/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile
), "Day 8/annecy/annecy.rds")

{
  png::writePNG(matrix(1), outfile)
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    outfile, 
    parallel = TRUE,
    samples = 300, 
    light = FALSE, 
    interactive = FALSE,
    environment_light = "./Day 5/phalzer_forest_01_4k.hdr",
    intensity_env = 1.75,
    rotate_env = 90,
    clamp_value = 10,
    line_radius = 1,
    text_size = 18,
    text_offset = c(0,12,0),
    clear = TRUE,
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}


ggplot()+
  geom_sf(data = water
          , fill = "steelblue") +
  geom_sf(data = cycle
          , col = "red")

