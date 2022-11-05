#Day 5 - Ukraine


library(rayshader)
library(rayvista)
library(rayrender)
library(terrainr)
library(tidyverse)
library(sf)
library(NatParksPalettes)
library(glue)
library(magick)


#Hoverla mountain in Ukraine
#thank you Spencer Schien, https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
#I also forked his rayshader_portraits github repo
#https://github.com/DABrianC/rayshader_portraits/blob/main/R/portraits/bryce_canyon/render_graphic.R

map <- "hoverla"

df <- data.frame(id = seq(1, 100, 1)
                              , x = runif(100, 24.480764, 24.523336) 
                              , y = runif(100, 48.14390, 48.172607))


df_sf <- df |> st_as_sf(coords = c("x", "y"), crs = 4326)

z <- 10  
dem <- elevatr::get_elev_raster(df_sf, z = 10)


mat <- raster_to_matrix(dem)

w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

pal <- "ukraine"


c1 <- natparks.pals("Glacier")
c2 <- rcartocolor::carto_pal(7, "PinkYl")
yellows <- c("#f1ee8e", "#e8e337", "#ffd700", "#ffd700")

colors <- c(rev(c1[2:4]), yellows)


rgl::rgl.close()
mat %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = mat, 
          solid = FALSE, 
          z = 12,
          shadowdepth = 50,
          windowsize = c(800*wr,800*hr), 
          phi = 90, 
          zoom = 1, 
          theta = 0, 
          background = "white") 

# Use this to adjust the view after building the window object
render_camera(phi = 10, zoom = .7, theta = 120)

###---render high quality
if (!dir.exists(glue("Day 5/{map}"))) {
  dir.create(glue("Day 5/{map}"))
}

outfile <- stringr::str_to_lower(glue("Day 5/{map}/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile
), "Day 5/hoverla/header.rds")

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
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}

#Annotate the img
img <- image_read("./Day 5/hoverla/hoverla_ukraine_z10.png")

#set text color
text_color <- colors[[3]]

#set the font
font <- "Copperplate Gothic"
{
img_ <- image_annotate(img, "Day 5: Ukraine's Highest Point"
                       , font = font
                       , color = text_color
                       , size = 125
                       , gravity = "north"
                       , location = "+0+200")

img_ <- image_annotate(img_, "Mount Hoverla", weight = 700
                       , font = font
                       , location = "+0+400"
                       , color = text_color
                       , size = 200
                       , gravity = "north")

elevation <- max(mat, na.rm = T)

img_ <- image_annotate(img_, glue("Elevation: {scales::label_comma()(elevation)} m")
                       , font = font 
                       , location = "+1200-1300"
                       , color = text_color
                       , size = 110
                       , gravity = "west")


#inset plot
countries <- spData::world 

ukraine <- countries[countries$name_long=="Ukraine",]

point = tibble(x = 24.500278
               , y = 48.16)

spot <- st_buffer(st_as_sf(point, coords = c("x", "y")
                           , crs = 4326), 50000)



loc_plot <- ggplot() + 
  geom_sf(data = ukraine, fill = text_color, color = colors[[3]], size = 0.2) + 
  ggfx::with_outer_glow(geom_sf(data = spot, fill = yellows[[3]], color = yellows[3])) +
  theme_void() + 
  coord_sf(crs = 4326)

loc_plot
ggsave(loc_plot, filename = glue("Day 5/hoverla_inset.png"), w = 4*1.5, h = 3*1.5)

inset <- image_read("Day 5/hoverla_inset.png")

img_comp <- image_composite(img_, inset
                , offset = "+200+1200"
                , gravity = "east")
# Caption
img_comp2 <- image_annotate(img_comp, glue("Visualized by @DABrianC | #30DayMapChallenge", 
                                  " | Made with : #Rayshader & #Elevatr") 
                       , font = font
                       , location = "+0+50"
                       , color = text_color
                       , size = 75
                       , gravity = "south")

image_write(img_comp2, glue("Day 5/hoverla/hoverla_fully_annotated.png"))
}
