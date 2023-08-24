g=gc();rm(list=ls())
library(pacman)
p_load(tidyverse,terra,fields,future,doSNOW,foreach,parallel,doParallel)
root <- "//catalogue/workspace_cluster_2/forrajes/"



ind_temp <- function(year,reg, var ){
  library(pacman)
  p_load(tidyverse,terra,fields,future,doSNOW,foreach,parallel,doParallel)
  root <-"//catalogue/workspace_cluster_2/forrajes/"
  
  ruta <- paste0(root,"1.Data/",reg,"/",var,"/",year,"/")
  mes <- 1:12
  path <- paste0("month_",mes)
  
  ruta <- lapply(1:length(path), function(i){
    fls <- list.files(paste0(ruta,path[i]), pattern = ".tif$", full.names = T)
    return(fls)
  })
  
  
  data <- lapply(1:length(ruta), function(j){
    cat(paste0("procesando mes ::: " , j, " de : " , length(ruta)), "\n")
    rout_ras <- ruta[[j]]
    
    ras <- lapply(1 : length(rout_ras), function(k){
      obj <- terra::rast(rout_ras[k])
      return(obj)
    })
    
    raster    <- terra::rast(ras)
    minimo <- min(raster,na.rm = T)
    maximo  <- max(raster,na.rm = T)
    average <- mean(raster,na.rm = T)
    
    output_min  <- paste0(root,"2.Result/indices/",reg,"/min_",var,"/",year,"/")
    output_max  <- paste0(root,"2.Result/indices/",reg,"/max_",var,"/",year,"/")
    output_mean <- paste0(root,"2.Result/indices/",reg,"/avg_",var,"/",year,"/")
    
    dir.create(output_min, FALSE, TRUE)
    dir.create(output_max, FALSE, TRUE)
    dir.create(output_mean, FALSE, TRUE)
    
    writeRaster(minimo, paste0(output_min,"/min_",var,"_",year,"_month_",j,".tif"))
    writeRaster(maximo, paste0(output_max,"/max_",var,"_",year,"_month_",j,".tif"))
    writeRaster(average, paste0(output_mean,"/avg_",var,"_",year,"_month_",j,".tif"))
    
    
  })
}


##########################################


cores<- detectCores()
cl<- makeCluster(cores-30)
registerDoParallel(cl)
year <- 2017:2022

system.time(l <- foreach(i=1:length(year)) %dopar% {
  ind_temp(year= year[i],
        reg = "colombia",
        var = "tmin")
})
stopCluster(cl)

