


# Development version of ggspatial
library(ggspatial)
library(ggplot2)
library(giscoR)
library(dplyr)
library(rasterpic)

# For country names
library(countrycode)

world <- gisco_get_countries(epsg = 3857)
europe <- gisco_get_countries(region = "Europe", epsg = 3857) %>% 
  filter(CNTR_ID == "UA")

bbox <- sf::st_bbox(europe)
# Base map of europe
plot <- ggplot(world) +
  geom_sf(fill = "grey90") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue"))

plot +
  # Zoom on Europe
  coord_sf(
    xlim = c(bbox[[1]], bbox[[3]]),
    ylim = c(bbox[[2]], bbox[[4]])
  )


# We paste the ISO2 code to each african country
europe$iso2 <- countrycode(europe$ISO3_CODE, "iso3c", "iso2c")

# Get flags from repo - low quality to speed up the code
flagrepo <- "https://raw.githubusercontent.com/hjnilsson/country-flags/master/png250px/"

# Loop and add
for (iso in europe$iso2) {
  # Download pic and plot
  imgurl <- paste0(flagrepo, tolower(iso), ".png")
  tmpfile <- tempfile(fileext = ".png")
  download.file(imgurl, tmpfile, quiet = TRUE, mode = "wb")
  
  # Raster
  x <- europe %>% filter(iso2 == iso)
  x_rast <- rasterpic_img(x, tmpfile, crop = TRUE, mask = TRUE)
  plot <- plot + layer_spatial(x_rast)
}

plot +
  geom_sf(data = europe, fill = NA) +
  # Zoom on europe
  coord_sf(
    xlim = c(bbox[[1]], bbox[[3]]),
    ylim = c(bbox[[2]], bbox[[4]])
  ) +
  theme_void()

library(rayshader)
library(rayvista)
library(rayrender)
library(terrainr)
library(tidyverse)
library(sf)
library(NatParksPalettes)


#Hoverla mountain in Ukraine
#thank you https://github.com/DABrianC/rayshader_portraits/blob/main/R/portraits/bryce_canyon/render_graphic.R
buh <- data.frame(id = seq(1, 100, 1)
                              , x = runif(100, 24.0335, 24.3735) 
                              , y = runif(100, 47.0606, 48.1441))


buh_sf <- buh |> st_as_sf(coords = c("x", "y"), crs = 4326)
  
dem <- elevatr::get_elev_raster(buh_sf, z = 8)


mat <- raster_to_matrix(dem)

w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))



pal <- "glacier_arches2"

c1 <- natparks.pals("Glacier")
c2 <- natparks.pals("Arches2")

colors <- c(rev(c1[2:5]), c2[2:5])

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
          shadowdepth = 100,
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
render_camera(phi = 45, zoom = .7, theta = 0)


