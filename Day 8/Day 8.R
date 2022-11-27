options(rgl.useNULL = FALSE)

library(tidyverse)
library(sf)
library(raster)
library(rayshader)
library(rayimage)
library(osmdata)
library(elevatr)
library(glue)
library(magick)
library(nationalparkcolors)

###Annecy Lake------

#topography files downloaded from https://dwtkns.com/srtm30m/
background <- raster("./data/N45E006.hgt")

mat <- raster_to_matrix(background)


#colors 
#set text color

palette <- park_palette("GeneralGrant", n = 8)

colors <- "grant"
pal <- "grant"
text_color <- "#802729"
#Testing what I've downloaded to see that it includes the correct 
# location.
mat|>
  height_shade() %>% 
  add_overlay(sphere_shade(mat, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(mat,zscale = 6),0) %>%
  add_shadow(ambient_shade(mat), 0) %>%
  add_shadow(texture_shade(mat,detail=8/10
                           ,contrast=9
                           ,brightness = 11), 0.1) %>%
  plot_map()


bbox <- c(6.1081, 45.7795, 6.2533, 45.9148)

extent_zoomed = extent(bbox[1], bbox[3], bbox[2], bbox[4])

annecy_zoomed = crop(background, extent_zoomed)

annecy_zoomed_mat = raster_to_matrix(annecy_zoomed)

#dimensions for the window
w <- nrow(annecy_zoomed_mat)
h <- ncol(annecy_zoomed_mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))


rgl::rgl.close()
base_map = annecy_zoomed_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(annecy_zoomed_mat, texture = "desert", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(annecy_zoomed_mat), 0.5) %>%
  add_shadow(ambient_shade(annecy_zoomed_mat),0.5) %>% 
  add_shadow(texture_shade(annecy_zoomed_mat,detail=8/10,contrast=9,brightness = 12), 0.1) #%>% 

#download osm data
#bicycle paths
cycle <- opq(bbox) |>
  add_osm_feature(key = "route"
                  , value = "bicycle") |>
  osmdata_sf()

#foot paths
foot <- opq(bbox) |>
  add_osm_feature(key = "route"
                  , value = c("foot", "hiking", "running")) |>
  osmdata_sf()

#water
water <- opq(bbox) |>
  add_osm_feature(key = "water"
                  , value = c("canal", "lake", "river")) |>
  osmdata_sf()

#buildings
buildings <- opq(bbox) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

#transforming the relevant parts of the OSM data
annecy_cycle <- st_transform(cycle$osm_multilines, crs = crs(background))

annecy_lake <- st_transform(water$osm_multipolygons, crs = crs(background))

annecy_river <- st_transform(water$osm_lines, crs = crs(background))

annecy_foot <- st_transform(foot$osm_multilines, crs = crs(background))

annecy_building <- st_transform(buildings$osm_multipolygons
                                , crs = crs(background))
annecy_building2 <- st_transform(buildings$osm_polygons
                                 , crs = crs(background))

#testing the different objects for cycle paths, hiking paths, water, and buildings
ggplot() +
  geom_sf(data = annecy_cycle, color = "red", alpha = .6) +
  geom_sf(data = annecy_lake, fill = "blue", alpha = .6) +
  geom_sf(data = annecy_river, color = "blue", alpha = .6) +
  geom_sf(data = annecy_foot, color = "brown", alpha = .6) +
  geom_sf(data = annecy_building, color = "grey", alpha = .6) +
  geom_sf(data = annecy_building2, color = "darkgrey", alpha = .6)+
  theme(legend.position = "none") +
  labs(title = "Open Street Map `route/bicycle` attribute in Annecy region")

rgl::rgl.close()
base_map |>
  add_overlay(generate_polygon_overlay(annecy_lake, extent = extent_zoomed
                                       , heightmap = annecy_zoomed_mat
                                       , palette = "skyblue2"), alphalayer = .7) |>
  add_overlay(generate_polygon_overlay(annecy_building, extent = extent_zoomed
                                       , heightmap = annecy_zoomed_mat
                                       , palette = "lightgrey"), alphalayer = .6) |>
  add_overlay(generate_polygon_overlay(annecy_building2, extent = extent_zoomed
                                       , heightmap = annecy_zoomed_mat
                                       , palette = "lightgrey"), alphalayer = .6) |>
  add_overlay(generate_line_overlay(annecy_river, extent = extent_zoomed
                                    , heightmap = annecy_zoomed_mat
                                    , color = "skyblue2"), alphalayer = .7)|> 
  add_overlay(generate_line_overlay(annecy_cycle,extent = extent_zoomed,
                                    linewidth = 3, color="black"
                                    , heightmap = annecy_zoomed_mat)) |>
  add_overlay(generate_line_overlay(annecy_cycle,extent = extent_zoomed,
                                    linewidth = 2, color= "#F3AE6D",
                                    heightmap = annecy_zoomed_mat)) |>
  add_overlay(generate_line_overlay(annecy_foot, extent = extent_zoomed
                                    , heightmap = annecy_zoomed_mat
                                    , color = "#C9DACA", lty = 3
                                    , linewidth = 1)) |>
  plot_3d(heightmap = annecy_zoomed_mat, 
          solid = FALSE, 
          z = 12,
          shadowdepth = 50,
          windowsize = c(800*wr,800*hr), 
          phi = 90, 
          zoom = 1, 
          theta = 0, 
          background = "#516888") 

#adjust the view
render_camera(phi = 35, zoom = .8, theta = 20)

map <- "annecy"

z <- 17
#Now to save it as a highquality image
###---render high quality
if (!dir.exists(glue("./Annecy/{map}"))) {
  dir.create(glue("./Annecy/{map}"))
}

outfile <- stringr::str_to_lower(glue("./annecy/{map}/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use them in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile
), "Annecy/annecy/header.rds")


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
    environment_light = "./phalzer_forest_01_4k.hdr",
    intensity_env = 1.75,
    rotate_env = 90,
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}


#Annotate the img
img <- image_read("./Day 8/annecy/annecy/annecy_grant_z17.png")

#set text color

pal <- park_palette("GeneralGrant", n = 8)

text_color <- pal[[8]]

#set the font
font <- "Bradley Hand ITC"

img_ <- image_annotate(img, "Day 8: Lac d'Annecy"
                       , font = font
                       , color = text_color
                       , size = 300
                       , gravity = "north"
                       , location = "+0+200"
                       )

img_title2 <- image_annotate(img_, "Cycling around the lake"
                             , weight = 700
                             , font = font
                             , location = "+0+550"
                             , color = text_color
                             , size = 200
                             , gravity = "north")

img_title3 <- image_annotate(img_title2, "or"
                             , font = font
                             , location = "+0+750"
                             , color = text_color
                             , size = 200
                             , gravity = "north")

img_title4 <- image_annotate(img_title3, "hiking in the mountains?"
                             , font = font
                             , color = text_color
                             , location = "+0+950"
                             , size = 200
                             , gravity = "north")

#inset map
france <- st_read("./Day 13/gadm41_FRA_1.shp")

point = tibble(x = 6.10
              , y = 45.51)

spot <- st_buffer(st_as_sf(point, coords = c("x", "y")
                         , crs = 4326), 15000)



loc_plot <- ggplot() + 
 ggfx::with_shadow(geom_sf(data = france, fill = pal[[7]], color = "white", size = 0.2)) + 
ggfx::with_outer_glow(geom_sf(data = spot, fill = pal[[1]], color = pal[1])) +
theme_void() + 
coord_sf(crs = 4326)


ggsave(loc_plot, filename = glue("Day 8/annecy_inset.png"), w = 4*1.5, h = 3*1.5)

inset <- image_read("Day 8/annecy_inset.png")

img_comp <- image_composite(img_title4, inset
                           , offset = "+0+1800"
                          , gravity = "west"
                          )


# Caption
img_comp2 <- image_annotate(img_comp, glue("Visualized by @bcalhoon7 | #30DayMapChallenge", 
                                          " | Made with : #Rayshader & #SRTM tiles & OpenStreetMap Contributors") 
                          , font = font
                         , location = "+0+50"
                        , color = "#802729"
                        , size = 75
                      , gravity = "south")

#Full size image not tracked - a condensed version is saved instead
image_write(img_comp2, glue("Day 8/annecy/annecy/annecy_fully_annotated.png"))

