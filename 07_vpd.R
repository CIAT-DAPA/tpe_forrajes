g=gc();rm(list=ls())
library(pacman)
p_load(tidyverse,terra,fields,future,doSNOW,foreach,parallel,doParallel)
root <- "//catalogue/workspace_cluster_2/forrajes/"

vpd  <- function(year,reg){
  library(pacman)
  p_load(tidyverse,terra,fields,future,doSNOW,foreach,parallel,doParallel)
  root <- "//catalogue/workspace_cluster_2/forrajes/"
  
  ruta_tmax <-paste0(root,"1.Data/",reg,"/tmax/",year,"/")
  ruta_tmin <- paste0(root,"1.Data/",reg,"/tmin/",year,"/")
  
  mes <- 1:12
  path <- paste0("month_",mes)
  
  ruta_tmax <- lapply(1:length(path), function(i){
    fls <- list.files(paste0(ruta_tmax,path[i]), pattern = ".tif$", full.names = T)
    return(fls)
  })
  
  ruta_tmin <- lapply(1:length(path), function(i){
    fls <- list.files(paste0(ruta_tmin,path[i]), pattern = ".tif$", full.names = T)
    return(fls)
  })
  
  data_tmax_tmin <- lapply(1:length(ruta_tmax), function(j){
    cat(paste0("procesando mes ::: " , j, " de : " , length(ruta_tmax)), "\n")
    rout_ras_tmax <- ruta_tmax[[j]]
    rout_ras_tmin <- ruta_tmin[[j]]
    
    ras_tmax <- lapply(1 : length(rout_ras_tmax), function(k){
      obj <- terra::rast(rout_ras_tmax[k])
      return(obj)
    })
    
    
    ras_tmin <- lapply(1 : length(rout_ras_tmin), function(k){
      obj <- terra::rast(rout_ras_tmin[k])
      return(obj)
    })
    
    calc_vpd <- function(tmin, tmax){
      
      #constants
      albedo <- 0.2
      vpd_cte <- 0.7
      
      #soil heat flux parameters
      a_eslope=611.2
      b_eslope=17.67
      c_eslope=243.5
      
      #input parameters
      tmean <- (tmin+tmax)/2
      
      #soil heat flux
      eslope=a_eslope*b_eslope*c_eslope/(tmean+c_eslope)^2*exp(b_eslope*tmean/(tmean+c_eslope))
      
      #estimate vpd
      esat_min=0.61120*exp((17.67*tmin)/(tmin+243.5))
      esat_max=0.61120*exp((17.67*tmax)/(tmax+243.5))
      vpd=vpd_cte*(esat_max-esat_min) #kPa
      return(vpd)
    } 
    
    vpd <- lapply(1:length(ras_tmin), function(b){
      cat(paste0("Procesando dia ::",b, "  de: ",length(ras_tmin), "\n"))
      tmax <- ras_tmax[[b]]
      tmin <- ras_tmin[[b]]
      res <- calc_vpd(tmin = tmin, tmax = tmax)
      return(res)
    })
    
    raster_vpd <- terra::rast(vpd)
    raster_vpd <- mean(raster_vpd,na.rm =T)
    output <- paste0(root,"2.Result/indices/",reg,"/vpd/",year,"/")
    dir.create(output, FALSE, TRUE)
    writeRaster(raster_vpd, paste0(output,"/vpd_",year,"_month_",j,".tif"))
    
  })
}


##########################################
cores<- detectCores()
cl<- makeCluster(cores-30)
registerDoParallel(cl)


year <- 1983:2016

system.time(l <- foreach(i=1:length(year)) %dopar% {
  vpd(year= year[i],
        reg = "colombia")
})
stopCluster(cl)

