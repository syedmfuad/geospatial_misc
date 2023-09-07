#get lower admin units
#irrigated vs rainfed
#closeness to water
#percentage of land for each crop in prior data

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

library(sf)
library(raster)
library(here)
library(ggplot2)
library(viridis)
library(dplyr)
library(rayshader)
library(rasterVis)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(nnet)

rm(list=ls())

countries <- c("BRN")

bd <- getData('GADM', country="BRN", level=1)
ad <- length(bd)

x <- paste0("C:/Users/syedm/Desktop/FAO-GAEZ/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Banana_Total.tif")

raster1 <- raster(here::here(x)) # load file
# apply function
    
bd_raster <- crop(raster1, extent(bd))
#plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)
    
d <- raster::extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)

thetahat <- matrix(, nrow = length(d@data[,11]), ncol = 4)

thetahat[,4]=d@data[,14] #was 13 before
thetahat[,1]=d@data[,2]
thetahat[,2]=d@data[,4]
thetahat[,3]=d@data[,7]

