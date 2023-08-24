
###################################################
######### Datos de precipitacion    ###############
###################################################
g <- gc(reset = T); rm(list = ls()); options(scipen = 999)
require(pacman)
pacman::p_load(terra,tidyverse)


prec <- function(reg,year){
  
  root <- "//catalogue/workspace_cluster_2/forrajes/"
  rout_files <- "//CATALOGUE/WFP_ClimateRiskPr1/1.Data/Chirps/"
  
  mnt  <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  path <- paste0(year,".",mnt)
  
  ruta <- lapply(1:length(path), function(i){
    fls <- list.files(rout_files, pattern = path[i])
    fls_c <- paste0(rout_files,fls)
    return(fls_c)
  })
  
  data <- lapply(1:length(ruta), function(j){
    
    rout_ras <- ruta[[j]]
    ras <- lapply(1 : length(rout_ras), function(k){
            obj <- terra::rast(rout_ras[k])
            shp <- terra::vect(paste0(root,"1.Data/Base/shp/_",reg,"/",reg,".shp"))
            obj <- terra::crop(obj, shp)
            obj <- raster::mask(obj, shp)
            obj[obj == -9999] <- NA
            dir <- paste0(root, "1.Data/",reg,"/prec/",year,"/month_",j,"/")
            dir.create(dir, FALSE, TRUE)
            writeRaster(obj, paste0(dir,"0",j,"-",k,"-",year,".tif"))
            })
  })
}


year <- 1984:2016

lapply(1:length(year),function(i){
  cat(paste0("Procesando year :: " ,year[i], "\n" ))
  prec(reg= "africa", year = year[i])
  
} )







