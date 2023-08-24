g <- gc(reset = T); rm(list = ls()); options(scipen = 999)
require(pacman)
pacman::p_load(terra,tidyverse)

soils <- function(reg,reg2,soil){
  root <- "//catalogue/workspace_cluster_2/forrajes/"
  fls  <- paste0(root,"1.Data/soil_general/",soil,"/",reg2,"/",soil,".tif" ) 
  obj  <- terra::rast(fls)
  shp <- terra::vect(paste0(root,"1.Data/Base/shp/_",reg,"/",reg,".shp"))
  obj <- terra::crop(obj, shp)
  obj <- terra::mask(obj, shp)
  dir <- paste0(root, "1.Data/",reg,"/soil_data/")
  dir.create(dir, FALSE, TRUE)
  writeRaster(obj, paste0(dir,soil,".tif"))
  }

soil <- c("BLDFIE","CECSOL","ORCDRC","PHIHOX","Texture")

lapply(1:length(soil),function(i){
  cat(paste0("Procesando variable :: " ,soil[i], "\n" ))
  soils(reg= "colombia", reg2 = "South America", soil = soil[i])
  
} )

#reg2 : Africa - South America
#reg  : africa - colombia


