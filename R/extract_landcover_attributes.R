# Calculate fractions of land use classes for catchment polygons (from gpkg)
# @author: Pia Ebeling and
#          land use group of the CAMELS-DE team
#

# Load packages ####
library(sf) # spatial features
library(tidyverse)
# Raster extraction
library(stars) # to read raster data
library(exactextractr) # to extract raster data including covered fractions
library(terra) # for raster formatting

# Prepare workspace ####
path <- "Y:/Home/ebelingp/Projects/camels/scripts/R"
setwd(path)
source("functions_camels.R")
ezg_file = '../ezgs/ezgs_bwby_test3.gpkg' # Example file with 3 catchments (layers)

# Load raster data (Landuse) to extract
clc.tif <- read_stars("../input_data/ger_corine_ger100_2012_level1.tif")
#st_crs(clc.tif)
class_values <- sort(unique(c(clc.tif[[1]])))
print(class_values)


# Extract data for each catchment and calculate areal fractions per class ####
rm(results) # Clear workspace
ezg_id <- 3
# Loop through all layers (catchments)
for (ezg_id in seq(1,3,1)){
  # Load catchment
  print(ezg_id)
  ezg_name <- st_layers(ezg_file)$name[ezg_id]
  ezg <- readGpkgLayer_dissolvePolygon_calcArea(dsn=ezg_file,
                                                layer=ezg_name, returnMulti = T)
  
  ezg_union <- ezg[[1]]
  ezg_polygons <- ezg[[2]]
  
  # Check/Plot EZG unified polygon
  glimpse(ezg_union)
  ggplot() + 
    geom_sf(data=ezg_polygons) + 
    geom_sf(data=ezg_union, fill=NA, size=2, color="pink") +
    geom_sf_label(data=ezg_union, aes(label=paste(round(areakm2),"km2"))) +
    geom_sf_label(data=ezg_polygons, aes(label=river2), size=2)
  
  # Mask and crop the raster data ####
  # Transform EZG polygon to match the raster coordinate system
  ezg_union_trans <- st_transform(ezg_union, st_crs(clc.tif))
  crs(ezg_union_trans)
  
  # Mask raster to EZG area (and plot)
  raster_ezg <- st_crop(clc.tif, ezg_union_trans, as_points = FALSE)
  class_estimate <- table(raster_ezg[[1]])
  sum(class_estimate)
  error_estimate <- sum(class_estimate)*100*100/ezg_union_trans$area
  ggplot() +
    geom_stars(data=raster_ezg) + 
    geom_sf(data=ezg_union_trans, color="pink", size=1,fill=NA) +
    geom_sf_label(data=ezg_union_trans, aes(label=paste(round(areakm2),"km2"))) +
    scale_fill_continuous(name="class",na.value ="transparent")
  
  # Extract raster values
  raster_frac <- exact_extract(rast(clc.tif), ezg_union_trans, include_area=T)
  
  # Calculate areas for each class
  class_sum <- raster_frac[[1]] %>% 
    group_by(value) %>%
    summarise(area=sum(area*coverage_fraction))
  sum(class_sum$area)/1000000
  class_sum$frac <- class_sum$area/ sum(class_sum$area)
  class_sum
  
  # Summarise results
  class_data <- data.frame(t(class_sum$frac))
  colnames(class_data) <- paste0("f_",class_sum$value)
  
  # Initiate results variable (NA for missing classes)
  result <- data.frame(t(rep(NA, length(class_values))))
  colnames(result) <- c(paste0("f_",class_values))
  result[match(colnames(class_data),colnames(result))] <- class_data
  
  # add metadata
  result <- data.frame("OBJECTID"=ezg_id, 
                       "areakm2"=sum(class_sum$area)/1000000, 
                       result)
  print(result)
  
  # bind to results dataframe (all EZG)
  if(exists("results")){
    results <- rbind(results, result)
  } else {
    results <- result
  }
  
} #  end loop for this layer/catchment
