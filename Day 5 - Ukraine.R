#Day 5 - Ukraine


library(rayshader)
library(rayvista)
library(rayrender)
library(terrainr)
library(tidyverse)
library(sf)
library(NatParksPalettes)


#Hoverla mountain in Ukraine
#thank you https://github.com/DABrianC/rayshader_portraits/blob/main/R/portraits/bryce_canyon/render_graphic.R

df <- data.frame(id = seq(1, 100, 1)
                              , x = runif(100, 24.480764, 24.523336) 
                              , y = runif(100, 48.14390, 48.172607))


df_sf <- df |> st_as_sf(coords = c("x", "y"), crs = 4326)
  
dem <- elevatr::get_elev_raster(df_sf, z = 8)


mat <- raster_to_matrix(dem)

w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

pal <- "glacier_arches2"

c1 <- natparks.pals("Glacier")
c2 <- rcartocolor::carto_pal(7, "PinkYl")

colors <- c(rev(c1[2:5]), c2[2:5])

yellows <- c("#ffd700")
rgl::rgl.close()
mat %>%
  # This adds the coloring, we're passing in our `colors` object
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = mat, 
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE, 
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = 8,
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
render_camera(phi = 30, zoom = .7, theta = 120)


