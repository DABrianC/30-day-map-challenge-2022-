#Day 11 - Red 

library(tanaka)
library(elevatr)
library(terra)


#clicked on Google maps to find Great Falls

# use elevatr to get elevation data
dem <- get_elev_raster(locations = data.frame(x = c(-77.260813, -77.209486)
                                              , y = c(38.968335,39.004162)),
                z = 14, prj = "EPSG:4326", clip = "locations")

mat <- rayshader::raster_to_matrix(dem)

#check the min and max heights
min(mat) 
max(mat) 

ras <- rast(dem)
# custom color palette
pal <- rcartocolor::carto_pal(7, "Burg")


# display the map
tanaka(ras, breaks = seq(from = min(mat)
                         ,to = max(mat), 15), col = pal)
