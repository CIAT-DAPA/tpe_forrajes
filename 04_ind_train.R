g=gc();rm(list=ls())
library(pacman)
p_load(tidyverse,terra,fields,future,doSNOW,foreach,parallel,doParallel)
root <- "//catalogue/workspace_cluster_2/forrajes/"



train <- function(year,reg){
  library(pacman)
  p_load(tidyverse,terra,fields,future,doSNOW,foreach,parallel,doParallel)
  root <-"//catalogue/workspace_cluster_2/forrajes/"
  
  ruta <- paste0(root,"1.Data/",reg,"/prec/",year,"/")
  mes <- 1:12
  path <- paste0("month_",mes)
 
  ruta <- lapply(1:length(path), function(i){
    fls <- list.files(paste0(ruta,path[i]), pattern = ".tif", full.names = T)
    return(fls)
  })
  
  
  data <- lapply(1:length(ruta), function(j){
    cat(paste0("procesando mes ::: " , j, " de : " , length(ruta)), "\n")
    rout_ras <- ruta[[j]]
   
     ras <- lapply(1 : length(rout_ras), function(k){
        obj <- terra::rast(rout_ras[k])
        return(obj)
    })
    
    raster <- terra::rast(ras)
    system.time(result <- sum(raster,na.rm = T))
    
    output <- paste0(root,"2.Result/indices/",reg,"/train/",year,"/")
    dir.create(output, FALSE, TRUE)
    writeRaster(result, paste0(output,"/train_",year,"_month_",j,".tif"))
    
  })
}


##########################################

cores<- detectCores()
cl<- makeCluster(cores-30)
registerDoParallel(cl)


year <- 1983:2016

system.time(l <- foreach(i=1:length(year)) %dopar% {
  train(year= year[i],
        reg = "colombia")
})
stopCluster(cl)







