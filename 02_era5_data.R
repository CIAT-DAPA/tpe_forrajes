
g <- gc(reset = T); rm(list = ls()); options(scipen = 999)
require(pacman)
pacman::p_load(terra,tidyverse)

era5 <- function(reg,year, var,var2 ){
  
  root <- "//catalogue/workspace_cluster_2/forrajes/"
  rout_files <- '//CATALOGUE/WFP_ClimateRiskPr1/1.Data' 
  dirs <- c(paste0(rout_files, '/ERA5/', var))
  fls <- list.files(dirs, pattern = '.nc$') %>% as.character()
  fls <- fls[grep(pattern = year,fls)]
  
  mnt  <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  path <- paste0(year,"",mnt)
  
  ruta <- lapply(1:length(path), function(i){
    fls_c <- fls[grep(pattern = path[i] , fls)]
    return(fls_c)
  })
  
  data <- lapply(1:length(ruta), function(j){
    rout_ras <- ruta[[j]]
    base <-terra::rast(paste0(root,"1.Data/Base/raster/base_",reg,".tif"))
    ras <- lapply(1 : length(rout_ras), function(k){
      obj <- terra::rast(paste0(rout_files,"/ERA5/",var,"/",rout_ras[k]))
      shp <- terra::vect(paste0(root,"1.Data/Base/shp/_",reg,"/",reg,".shp"))
      obj <- terra::crop(obj, shp)
      obj <- terra::mask(obj, shp)
      obj <- terra::resample(obj,base)
      obj <- obj - 273.15
      dir <- paste0(root, "1.Data/",reg,"/",var2,"/",year,"/month_",j,"/")
      dir.create(dir, FALSE, TRUE)
      writeRaster(obj, paste0(dir,"0",j,"-",k,"-",year,".tif"))
    })
  })
}


year <- 1983:2016
lapply(1:length(year),function(i){
  cat(paste0("Procesando year :: " ,year[i], "\n" ))
  era5(reg= "africa", year = year[i], var = '2m_temperature-24_hour_maximum', var2 ="tmax" )
  
} )
