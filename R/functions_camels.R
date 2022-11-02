# Common functions to create the CAMELS-DE data set
#
# Includes:
#    - GIS functions
#
# @author: Pia Ebeling (FILL IN HERE)
# @created: 2022/07/21
# @last modified: ...


#### GIS functions ####

#' Read Layer from Geopackage, dissolve (union) and calculate area
#'
#' @description Function to read a layer from a geopackage, 
#' dissolve multipolygon structure into a single polygon and 
#' recalculate the area. 
#' This can e.g. serve for read the catchment boundaries of one catchment 
#' saved in one layer of the geopackage
#'
#' @param dsn data source name, e.g. the file name fo the geopackage
#' @param layer layer name of the geopackage
#' @param calcArea logical; if TRUE the attributes area [m2] and areakm2 [km2] 
#' will be calculated; default TRUE
#' @param returnMulti logical; if TRUE multipolygon object is also returned 
#' together with the single polygon in a list of 2 objects; default FALSE
#'
#' @return Catchment polygon 
#' @export
#'
#' @examples
readGpkgLayer_dissolvePolygon_calcArea <- function(dsn, layer, objectid=NA,
                                                   calcArea=TRUE, returnMulti=FALSE){
  # Read data
  print(paste(layer, "is read from", dsn))
  ezg_polygons <- sf::st_read(dsn = dsn, layer=layer)
  
  # EZG union
  ezg_union <- ezg_polygons %>% sf::st_union()
  class(ezg_union)
  ezg_union <- sf::st_as_sf(ezg_union)
  ezg_union$ezg_id <- objectid
  
  # Calculate Area
  if(calcArea==TRUE){
    print("Polygon area is calculated")
    ezg_union  <- ezg_union %>% 
      dplyr::mutate(
        #.before=2,
        area=st_area(.),
        areakm2=units::set_units(st_area(.),km^2))
  }
  
  # Return
  if (returnMulti==TRUE){
    return(list(ezg_union, ezg_polygons))
  } else {
    return(ezg_union)
  }
}
