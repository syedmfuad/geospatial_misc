#cool
#https://opengeohub.org/
#https://www.isric.org/explore/soil-geographic-databases#eurasia
#https://earthexplorer.usgs.gov/

#africa
#https://gitlab.com/openlandmap
#https://medium.com/nerd-for-tech/soil-and-agronomy-data-cube-for-africa-at-30-m-spatial-resolution-591e04bfc372

#climate
#https://chelsa-climate.org/downloads/

#global
#https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247 #soil content
#http://globalchange.bnu.edu.cn/research/soilw #updated soil content
#https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/use/?cid=nrcs142p2_054013 #soil type
#https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ #soil type
#http://globalchange.bnu.edu.cn/research/data #soil type
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1PEEY0 #harvard soil type
#https://cgiarcsi.community/data/global-high-resolution-soil-water-balance/ #soil water balance
#https://daac.ornl.gov/cgi-bin/dataset_lister.pl?p=19 #lots of stuff
#https://gitlab.com/openlandmap/compiled-ess-point-data-sets#soil-properties-and-classes

#socioeconomic
#https://sedac.ciesin.columbia.edu/data/sets/browse?facets=theme:population&facets=data-type:raster
#https://www.worldpop.org/project/list


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

#nigeria
#cameroon
#kenya
#tanzania

countries <- c("NGA", "CMR", "TZA", "COD", "AGO", "COG", "GAB", "SOM", "ETH", "KEN", "UGA", "SSD", "CAF", "BEN", "TGO", "GHA", "CIV",
               "LBR", "SLE", "GIN", "TCD")
files <- list.files(path="C:/Users/syedm/Desktop/soil/HWSD_1247/data", pattern="*.nc4", full.names=TRUE, recursive=FALSE)

list_estimate<-list()

for(i in 1:length(countries)){ #change to 1
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,28)
  names=matrix(NA,ad,2)
  
  for(j in 1:length(files)){
    x <- files[j]
    raster1 <- raster(here::here(x)) # load file
    # apply function
    
    bd_raster <- crop(raster1, extent(bd))
    #plot(bd_raster)
    bd_raster2 <- mask(bd_raster, bd)
    #plot(bd_raster2)
    #plot(bd, add=TRUE)
    
    d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
    thetahat[,j]=d@data[,11] #was 13 before
    names[,1]=d@data[,2]
    names[,2]=d@data[,4]
    mat <- cbind(names, thetahat)
  }
  
  list_estimate[[i]] <- mat
  
}

crops <- c("WHEA", "RICE", "MAIZ", "BARL", "PMIL", "SMIL", "SORG", "OCER", "POTA", "SWPO", "YAMS", "CASS", "ORTS", "BEAN", "CHIC", 
           "COWP", "PIGE", "LENT", "OPUL", "SOYB", "GROU", "CNUT", "OILP", "SUNF", "RAPE", "SESA", "OOIL", "SUGC", "SUGB", "COTT",
           "OFIB", "ACOF", "RCOF", "COCO", "TEAS", "TOBA", "BANA", "PLNT", "TROF", "TEMF", "VEGE", "REST")

list_crop<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,42)
  names=matrix(NA,ad,2)
  
  for(j in 1:length(crops)){
    
    #harvested area
    #x <- paste0("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_harv_area.geotiff/spam2017V2r1_SSA_H_", crops[j], "_A.tif")
    
    #physical area
    #x <- paste0("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_phys_area.geotiff/spam2017V2r1_SSA_A_", crops[j], "_A.tif")
    
    #yield
    #x <- paste0("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_yield.geotiff/spam2017V2r1_SSA_Y_", crops[j], "_A.tif")
    
    #value of production
    #x <- paste0("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_val_prod.geotiff/spam2017V2r1_SSA_V_", crops[j], "_A.tif")
    
    #production
    #x <- paste0("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_prod.geotiff/spam2017V2r1_SSA_P_", crops[j], "_A.tif")
    
    x <- paste0("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_prod.geotiff/spam2017V2r1_SSA_P_", crops[j], "_A.tif")
    raster1 <- raster(here::here(x)) # load file
    # apply function
    
    bd_raster <- crop(raster1, extent(bd))
    #plot(bd_raster)
    bd_raster2 <- mask(bd_raster, bd)
    #plot(bd_raster2)
    #plot(bd, add=TRUE)
    
    d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
    thetahat[,j]=d@data[,11] #was 13 before
    names[,1]=d@data[,2]
    names[,2]=d@data[,4]
    mat <- cbind(names, thetahat)
  }
  
  list_crop[[i]] <- mat
  
}

list_elevation<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,1)
  names=matrix(NA,ad,2)
  
  raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/GDEM-10km-colorized.tif"))
  
  bd_raster <- crop(raster1, extent(bd))
  #plot(bd_raster)
  bd_raster2 <- mask(bd_raster, bd)
  #plot(bd_raster2)
  #plot(bd, add=TRUE)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  thetahat[,1]=d@data[,11] #was 13 before
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_elevation[[i]] <- mat
  
}

climate <- getData('worldclim', var='bio', res=2.5)
plot(climate$bio1, main="Annual Mean Temperature")

list_bio<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,19)
  names=matrix(NA,ad,2)
  
  for (j in 1:19) {
    bio <- paste0("bio", j)
    bd_raster <- crop(climate[[bio]], extent(bd))
    bd_raster2 <- mask(bd_raster, bd)
    d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
    thetahat[,j]=d@data[,11]
  }
  
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_bio[[i]] <- mat
  
}


list_fertilizer<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,4)
  names=matrix(NA,ad,2)
  
  raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/nfertilizer_global.tif"))
  raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/pfertilizer_global.tif"))
  raster3 <- raster(here::here("C:/Users/syedm/Desktop/soil/nmanure_global.tif"))
  raster4 <- raster(here::here("C:/Users/syedm/Desktop/soil/pmanure_global.tif"))
  
  bd_raster <- crop(raster1, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  thetahat[,1]=d@data[,11]
  
  bd_raster <- crop(raster2, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  thetahat[,2]=d@data[,11]
  
  bd_raster <- crop(raster3, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  thetahat[,3]=d@data[,11]
  
  bd_raster <- crop(raster4, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  thetahat[,4]=d@data[,11]
  
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_fertilizer[[i]] <- mat
  
}

list_population<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,1)
  names=matrix(NA,ad,2)
  
  raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/gpw_v4_population_density_rev11_2015_2pt5_min.tif"))
  
  bd_raster <- crop(raster1, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  
  thetahat[,1]=d@data[,11]
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_population[[i]] <- mat
  
}

list_air<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,1)
  names=matrix(NA,ad,2)
  
  raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2016.tif"))
  
  bd_raster <- crop(raster1, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  
  thetahat[,1]=d@data[,11]
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_air[[i]] <- mat
  
}

list_ho<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,2)
  names=matrix(NA,ad,2)
  
  raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/GlobalPrecipGS/d2h_GS.tif"))
  raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/GlobalPrecipGS/d18o_GS.tif"))
  
  bd_raster <- crop(raster1, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  thetahat[,1]=d@data[,11]
  
  bd_raster <- crop(raster2, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  
  thetahat[,2]=d@data[,11]
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_ho[[i]] <- mat
  
}

list_area<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,1)
  names=matrix(NA,ad,2)
  
  thetahat=area(bd)/1000000
  names[,1]=bd@data[,2]
  names[,2]=bd@data[,4]
  mat <- cbind(names, thetahat)
  
  list_area[[i]] <- mat
  
}

list_imr<-list()

for(i in 1:length(countries)){
  
  bd <- getData('GADM', country=countries[i], level=1)
  ad <- length(bd)
  
  thetahat=matrix(NA,ad,1)
  names=matrix(NA,ad,2)
  
  raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/povmap_global_subnational_infant_mortality_rates_v2_01.tif"))
  
  bd_raster <- crop(raster1, extent(bd))
  bd_raster2 <- mask(bd_raster, bd)
  bd_raster2 <- clamp(bd_raster2, lower=0, useValues=FALSE)
  #x <- clamp(r, lower=3, useValues=FALSE)
  #y <- reclassify(r, cbind(-Inf, 0, NA), right=FALSE)
  
  d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
  
  thetahat[,1]=d@data[,11]
  names[,1]=d@data[,2]
  names[,2]=d@data[,4]
  mat <- cbind(names, thetahat)
  
  list_imr[[i]] <- mat
  
}





df_soil <- do.call(rbind.data.frame, list_estimate)

df_air <- do.call(rbind.data.frame, list_air)
df_air <- df_air[,-c(1:2)]
df_air <- as.data.frame(df_air)
colnames(df_air) <- c("pm2_5")

df_bio <- do.call(rbind.data.frame, list_bio)
df_bio <- df_bio[,-c(1:2)]
colnames(df_bio) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15",
                      "bio16", "bio17", "bio18", "bio19")

df_crop <- do.call(rbind.data.frame, list_crop)
df_crop <- df_crop[,-c(1:2)]
colnames(df_crop) <- c("WHEA", "RICE", "MAIZ", "BARL", "PMIL", "SMIL", "SORG", "OCER", "POTA", "SWPO", "YAMS", "CASS", "ORTS", "BEAN", "CHIC", 
                       "COWP", "PIGE", "LENT", "OPUL", "SOYB", "GROU", "CNUT", "OILP", "SUNF", "RAPE", "SESA", "OOIL", "SUGC", "SUGB", "COTT",
                       "OFIB", "ACOF", "RCOF", "COCO", "TEAS", "TOBA", "BANA", "PLNT", "TROF", "TEMF", "VEGE", "REST")

df_elevate <- do.call(rbind.data.frame, list_elevation)
df_elevate <- as.data.frame(df_elevate)
colnames(df_elevate) <- c("c1", "c2", "elevation")
df_elevate$c1 <- NULL
df_elevate$c2 <- NULL

df_fertilizer <- do.call(rbind.data.frame, list_fertilizer)
df_fertilizer <- df_fertilizer[,-c(1:2)]
colnames(df_fertilizer) <- c("nfert", "pfert", "nman", "pman")

df_ho <- do.call(rbind.data.frame, list_ho)
df_ho <- df_ho[,-c(1:2)]
colnames(df_ho) <- c("hydro", "oxy")

df_pop <- do.call(rbind.data.frame, list_population)
colnames(df_pop) <- c("c1", "c2", "pop")
df_pop$c1 <- NULL
df_pop$c2 <- NULL

df_area <- do.call(rbind.data.frame, list_area)
colnames(df_area) <- c("c1", "c2", "area")
df_area$c1 <- NULL
df_area$c2 <- NULL

df_imr <- do.call(rbind.data.frame, list_imr)
colnames(df_imr) <- c("c1", "c2", "imr")
df_imr$c1 <- NULL
df_imr$c2 <- NULL

df <- cbind(df_soil, df_air, df_bio, df_crop, df_elevate, df_fertilizer, df_ho, df_pop)






crops <- c("WHEA", "RICE", "MAIZ", "BARL", "PMIL", "SMIL", "SORG", "OCER", "POTA", "SWPO", "YAMS", "CASS", "ORTS", "BEAN", "CHIC", 
           "COWP", "PIGE", "LENT", "OPUL", "SOYB", "GROU", "CNUT", "OILP", "SUNF", "RAPE", "SESA", "OOIL", "SUGC", "SUGB", "COTT",
           "OFIB", "ACOF", "RCOF", "COCO", "TEAS", "TOBA", "BANA", "PLNT", "TROF", "TEMF", "VEGE", "REST")

df1 <- read.csv("df_yield.csv")
mod1 <- lm(I(VEGE) ~ V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+
                  bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+
                  pm2_5 + elevation	+ nfert	+ pfert	+ nman + pman	+ hydro	+ oxy	+ pop + area + prop_area + as.factor(V1), data=df1)
summary(mod1)

data.frame(summary(mod1)$coef[summary(mod1)$coef[,4] <= 0.10, 1])

df1 <- df1[complete.cases(df1), ]

df1 %>%
  mutate(quantile = ntile(MAIZ, 4)) -> data1

data1$quantile <- as.factor(data1$quantile)

levels(data1$quantile)

col_to_aggregate <- c("V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", 
                      "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                      "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "pm2_5", "elevation", "nfert", 
                      "pfert", "nman", "pman", "hydro", "oxy", "pop")

group_by(data1, quantile) %>%
  summarise_at(vars(col_to_aggregate), list(name = mean)) -> data2

write.csv(data2, file="test.csv")
summary(as.factor(data1$V1))
data1_1 <- subset(data1, quantile==1)
summary(as.factor(data1_1$V1))
data1_2 <- subset(data1, quantile==2)
summary(as.factor(data1_2$V1))
data1_3 <- subset(data1, quantile==3)
summary(as.factor(data1_3$V1))
data1_4 <- subset(data1, quantile==4)
summary(as.factor(data1_4$V1))

res <- aov(MAIZ ~ quantile, data=data1)
summary(res)

for(i in 4:length(data1)){
  
  res <- aov(data1[,i] ~ quantile, data=data1)
  print(summary(res))
  print(colnames(data1[i]))
  
}





crops <- c("WHEA", "RICE", "MAIZ", "BARL", "PMIL", "SMIL", "SORG", "OCER", "POTA", "SWPO", "YAMS", "CASS", "ORTS", "BEAN", "CHIC", 
           "COWP", "PIGE", "LENT", "OPUL", "SOYB", "GROU", "CNUT", "OILP", "SUNF", "RAPE", "SESA", "OOIL", "SUGC", "SUGB", "COTT",
           "OFIB", "ACOF", "RCOF", "COCO", "TEAS", "TOBA", "BANA", "PLNT", "TROF", "TEMF", "VEGE", "REST")

df1 <- read.csv("df_yield.csv")
df1 <- df1[complete.cases(df1), ]
#df1 <- df1[-c(65, 146, 154, 161), ]

df1[,c(crops)] %>%
  mutate(row = row_number()) %>%
  gather(col, val, -row) %>%
  group_by(row) %>%
  arrange(val) %>%
  top_n(1) %>%
  do(head(., 1)) -> df2

summary(as.factor(df2$col))

#c <- colnames(df1[,c(52:93)])[apply(df1[,c(52:93)],1,which.max)]

#c <- apply(df1[,c(52:93)],1,which.max)
length(unique(c))
summary(as.factor(c))
df1$c <- df2$col

mod1 <- multinom(c ~ V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+
             bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+
             pm2_5 + elevation	+ nfert	+ pfert	+ nman + pman	+ hydro	+ oxy	+ pop + area + prop_area + as.factor(V1), MaxNWts =10000000, data=df1)
summary(mod1)
multi1.rrr = exp(coef(mod1))
stargazer(mod1, type="html", out="multi1.htm")
stargazer(mod1, type="html", coef=list(multi1.rrr), p.auto=FALSE, out="multi1rrr.htm")
df3 <- as.data.frame(round(fitted(mod1), 2), 10)

write.csv(df1, file="test.csv")
write.csv(df2, file="test2.csv")
write.csv(df4, file="test3.csv")

data <- read.csv("test.csv")

df3 %>%
  mutate(row = row_number()) %>%
  gather(col, val, -row) %>%
  group_by(row) %>%
  arrange(val) %>%
  top_n(1) %>%
  do(head(., 1)) -> df4


mod1 <- lm(imr ~ y + V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+
             bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+
             pm2_5 + elevation	+ nfert	+ pfert	+ nman + pman	+ hydro	+ oxy	+ pop + area + prop_area + as.factor(V1), data=data)
summary(mod1)
data.frame(summary(mod1)$coef[summary(mod1)$coef[,4] <= 0.10, 1])



crops_to_remove <- c("WHEA", "BARL", "PMIL", "SMIL", "SORG", "OCER", "POTA", "SWPO", "YAMS", "ORTS", "CHIC", 
                     "COWP", "PIGE", "LENT", "OPUL", "SOYB", "GROU", "CNUT", "OILP", "SUNF", "RAPE", "SESA", "OOIL", "SUGC", "SUGB", "COTT",
                     "OFIB", "ACOF", "RCOF", "TEAS", "TOBA", "BANA", "TROF", "TEMF", "VEGE", "REST")
df1 <- read.csv("df_areaprop.csv")

df1 <- df1[ , !(names(df1) %in% crops_to_remove)]
df1 <- df1[-c(65, 146, 154, 161), ]
c <- colnames(df1[,c(52:57)])[apply(df1[,c(52:57)],1,which.max)]

#c <- apply(df1[,c(52:93)],1,which.max)
length(unique(c))
summary(as.factor(c))
df1$c <- c

head(round(fitted(mod1), 2))



df1 <- read.csv("df_areaprop.csv")
df1 <- df1[-c(65, 146, 154, 161), ]

df1$cereal <- (df1$WHEA+ df1$RICE+ df1$MAIZ+ df1$BARL+ df1$SORG+ df1$OCER)/6
df1$root <- (df1$POTA+ df1$SWPO+ df1$YAMS+ df1$CASS+ df1$ORTS)/5
df1$pulse <- (df1$BEAN+ df1$CHIC+ df1$COWP+ df1$PIGE+ df1$LENT+ df1$OPUL)/6
df1$oilcrop <- (df1$SOYB+ df1$GROU+ df1$CNUT+ df1$OILP+ df1$SUNF+ df1$RAPE+ df1$SESA+ df1$OOIL)/8
df1$millet <- (df1$PMIL+ df1$SMIL)/2
df1$coffee <- (df1$ACOF+ df1$RCOF)/2
df1$fruit <- (df1$TROF+ df1$TEMF)/2
df1$banana <- (df1$BANA+ df1$PLNT)/2
df1$sugar <- (df1$SUGB+ df1$SUGC)/2
df1$fiber <- (df1$COTT+ df1$OFIB)/2
df1$cocoa <- (df1$COCO)
df1$tea <- (df1$TEAS)
df1$tobacco <- (df1$TOBA)
df1$vege <- (df1$VEGE)
df1$rest <- (df1$REST)

df1[,c("cereal", "root", "pulse", "oilcrop", "millet", "coffee", 
       "fruit", "banana", "sugar", "fiber", "cocoa", 
       "tea", "tobacco", "vege", "rest")] %>%
  mutate(row = row_number()) %>%
  gather(col, val, -row) %>%
  group_by(row) %>%
  arrange(val) %>%
  top_n(1) %>%
  do(head(., 1)) -> df2

summary(as.factor(df2$col))


