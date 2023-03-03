# Creating a species distribution map with raster data 

# Import all needed libraries: the function used here is courtesy of Tyler Hampton
# Tyler Hampton's workshop is available here:https://github-com.subzero.lib.uoguelph.ca/tylerbhampton/R-GIS-Workshop

loadpackages=function(packages){  for(p in packages){
  if(!require(p,character.only=T)){install.packages(p)}
  # IF require returns FALSE, the package is missing and will be installed
  library(p,character.only=T,quietly=T,verbose=F)}} # next, it calls the package with library

loadpackages(c(
  "ggplot2", # Makes ggplots!
  "sf",   # "Simple Features", handles vector data
  "raster", # For working with Raster Data
  "rgdal", # 'Geospatial' Data Abstraction Library
  # note: rgdal will be retired by the end of 2023
  "tmap", # Thematic Map Visualization
  "ggsn", # Several scale bars for maps
  "ggrepel", # Labels on ggplots
  "maptools", # Manipulating geographic data
  # note: maptools will be retired by the end of 2023
  "plyr", # The split-apply-combine paradigm
  "data.table", # Works with data.frames
  "dplyr", # Data manipulation
  "purrr", # Functional Programming Tools
  "devtools", # Download custom R packages
  "spData", # Spatial Datasets 
  "terra" # methods for spatial data analysis with vectors and rasters (replaces raster package)
))


# The distribution data can be download from here
# https://gapanalysis.usgs.gov/apps/species-data-download/ 

# import the required bird distribution data from its location in your computer
# you have to import the .shp file

prwa.data <- st_read("C:/Users/Jelan/OneDrive/Desktop/University/University of Guelph/R_study_group/R_Show_&_tell_2023_03_02/bPROWx_CONUS_Range_2001v1/bPROWx_CONUS_Range_2001v1.shp")

# Map of the species range with tmap #########################################

# We can have a look at the shape of the species's range, and at the different
# properties of the shape file

plot(prwa.data)

# we need a basemap for our range data 

plot(world) # world is an sf object from the package spData 

# we can use tmap to plot the two layers together

tm_shape(shp = world[(world$name_long == "United States"),], 
         bbox = c(-140, 15, -50, 55)) +  # need to use a bounding box to hide Alaska and Hawaii
  tm_borders(lwd = 1) +
  tm_shape(shp = prwa.data, crs = projection(world)) +
  tm_fill(col = "lightgreen") +
  tm_borders(col = "darkgreen", lwd = 1)
  
# We can use the properties of the species range layer in our map 

tm_shape(shp = world[(world$name_long == "United States"),], 
           bbox = c(-140, 15, -50, 55)) +  # need to use a bounding box to hide Alaska and Hawaii
  tm_borders(lwd = 1) +
  tm_shape(shp = prwa.data, crs = projection(world)) +
  tm_fill(title = "Season", col = "SeasonName", palette = c("orange", "lightblue")) +
  tm_layout("Range of the Prothonotary warbler in the US", legend.position = c("left","bottom"))


# Map of the species range with plot() #########################################

data(wrld_simpl) # This time we will use wrld_simpl, which is a dataset part of the maptools package
# note that wrld_simpl is a spatial polygon dataframe, while world is a simple feature multipolygon 

plot(wrld_simpl[(wrld_simpl$NAME == "United States"),], col = "grey95", ylim = c(15, 55), xlim = c(-140, -55))
prwa.data.adj = st_transform(prwa.data, crs = crs(wrld_simpl))
plot(prwa.data.adj["SeasonName"], col = c("orange", "lightblue"), add = TRUE)
legend(-70, 30, legend = c("Summer", "Winter"), fill = c("orange", "lightblue"))
title(main = "Range of the Prothonotary warbler in the US")

# Now we'll plot an abundance raster for the prothonothary warbler ##############

# We import the raster first (it is from eBird)
ptwar_abundance <- raster("C:/Users/Jelan/OneDrive/Desktop/University/University of Guelph/R_study_group/R_Show_&_tell_2023_03_02/prowar_abundance_seasonal_breeding_mean_2021.tif")

# we can have a look at this abundance raster

plot(ptwar_abundance)

#we will need to change it's projection to be the same as that of world

crs(ptwar_abundance) <- crs(world)

# now we can plot it over our polygon of the US 
tm_shape(ptwar_abundance, bbox = c(-140, 15, -50, 55), raster.warp = TRUE) +
  tm_raster(palette = "Reds", alpha = 1, title = "relative abundance", style = "cont") +
  tm_shape(shp = world[(world$name_long == "United States"),]) +
  tm_borders(lwd = 1)+
  tm_layout(legend.position = c("right","TOP"))

# We make some modifications to make the map look better 
# Here we remove 0 from the raster 
ptwar_abundance[ptwar_abundance == 0] <- NA

plot(ptwar_abundance)

# # We can make our plot again 
 tm_shape(ptwar_abundance, bbox = c(-140, 15, -50, 55), raster.warp = TRUE) +
   tm_raster(palette = "Reds", alpha = 1, title = "relative abundance", style = "cont") +
   tm_shape(shp = world[(world$name_long == "United States"),]) +
   tm_borders(lwd = 1)+
   tm_layout(legend.position = c("right","TOP"))

# We can also use plot() for the abundance raster #############################

crs(ptwar_abundance) <- proj4string(wrld_simpl)

plot(ptwar_abundance, xlim = c(-1e7,-5e6), ylim = c(2e6,6e6))
plot(wrld_simpl[(wrld_simpl$NAME == "United States"),], col = "grey95", add = TRUE)

