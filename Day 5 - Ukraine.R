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
  # This adds the coloring, we're passing in our `colors` object
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = mat, 
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE, 
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = 12,
          # Set the location of the shadow, i.e. where the floor is.
          # This is on the same scale as your data, so call `zelev` to see the
          # min/max, and set it however far below min as you like.
          shadowdepth = 50,
          # Set the window size relatively small with the dimensions of our data.
          # Don't make this too big because it will just take longer to build,
          # and we're going to resize with `render_highquality()` below.
          windowsize = c(800*wr,800*hr), 
          # This is the azimuth, like the angle of the sun.
          # 90 degrees is directly above, 0 degrees is a profile view.
          phi = 90, 
          zoom = 1, 
          # `theta` is the rotations of the map. Keeping it at 0 will preserve
          # the standard (i.e. north is up) orientation of a plot
          theta = 0, 
          background = "white") 

# Use this to adjust the view after building the window object
render_camera(phi = 10, zoom = .7, theta = 120)

###---render high quality
# Ensure dir exists for these graphics
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

# Wrap this in brackets so it runs as chunk
{
  # Test write a PNG to ensure the file path is good.
  # You don't want `render_highquality()` to fail after it's 
  # taken hours to render.
  png::writePNG(matrix(1), outfile)
  # I like to track when I start the render
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    # We test-wrote to this file above, so we know it's good
    outfile, 
    # See rayrender::render_scene for more info, but best
    # sample method ('sobol') works best with values over 256
    parallel = TRUE,
    samples = 300, 
    # Turn light off because we're using environment_light
    light = FALSE, 
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    # HDR lighting used to light the scene
    environment_light = "./Day 5/phalzer_forest_01_4k.hdr",
    # Adjust this value to brighten or darken lighting
    intensity_env = 1.75,
    # Rotate the light -- positive values move it counter-clockwise
    rotate_env = 90,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}

#Annotate the img
img <- image_read("./Day 5/hoverla/hoverla_ukraine_z10.png")

#set text color
text_color <- colors[[3]]

#fonts
library(showtext)
library(extrafont)

font_import(paths = "C:/USERS/BRIAN.CALHOON/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS")
load_showtext_fonts()
font_add_google(name = "Fredericka the Great", family = "frederika")

font_add(family = "frederika"
         , regular = "C:/USERS/BRIAN.CALHOON/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/FREDERICKATHEGREAT-REGULAR.ttf")
loadfonts()
showtext_auto()
img_ <- image_annotate(img, "Ukraine's Highest Point"
                       , font = "Castellar"
                       , color = text_color
                       , size = 125
                       , gravity = "north"
                       , location = "+0+200")

img_ <- image_annotate(img_, "Mount Hoverla", weight = 700
                       , font = "Castellar"
                       , location = "+0+400"
                       , color = text_color
                       , size = 200
                       , gravity = "north")

elevation <- max(mat, na.rm = T)

img_ <- image_annotate(img_, glue("Elevation: {scales::label_comma()(elevation)} m")
                       , font = "Castellar" 
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
                       , font = "Castellar"
                       , location = "+0+50"
                       , color = text_color
                       , size = 75
                       , gravity = "south")

# Twitter
#twitter <- fontawesome::fa("twitter", fill = text_color, fill_opacity = .5)
#grid::grid.newpage()

#tmp <- tempfile()
#png(tmp, bg = "transparent")
#grid::grid.draw(svgtools::read_svg(twitter))
#dev.off()

#tw <- image_read(tmp)
#tw <- image_scale(tw, "x75")

#img_ <- image_composite(img_, tw, gravity = "south",
#                        offset = "-530+65")


image_write(img_comp2, glue("Day 5/hoverla/hoverla_fully_annotated.png"))
