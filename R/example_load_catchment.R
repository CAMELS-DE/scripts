# Example script to load and plot a catchment
# for the CAMELS-DE data set
#
# @author: Pia Ebeling 
# @created: 2022/07/21

library(sf)
library(ggplot2)

# Prepare workspace ####
path <- "Y:/Home/ebelingp/Projects/camels/scripts/R" # ADAPT (location of your scripts)
#path <- dirname(rstudioapi::getSourceEditorContext()$path) # get path of script, works for Rstudio
setwd(path)
source("functions_camels.R")
ezg_file = '../ezgs/ezgs_bwby_test3.gpkg' # Example file with 3 catchments (layers)

# Select catchment (Layer) from ezg_file (gpkg)
ezg_id <- 3
ezg_layer <- st_layers(ezg_file)$name[ezg_id]
print(ezg_layer)

# Load catchment
ezg <- readGpkgLayer_dissolvePolygon_calcArea(dsn=file, layer=ezg_layer, objectid=ezg_id,
                                              calcArea = T, returnMulti = T)
ezg_union <- ezg[[1]]
ezg_multi <- ezg[[2]]

# Plot catchment unified polygon
map <- ggplot() +  
  geom_sf(data=ezg_union, fill="grey70", size=2, color="pink") +
  geom_sf_label(data=ezg_union, aes(label=paste(round(areakm2),"km2"))) 
map
map + geom_sf(data=ezg_multi, fill=NA)
