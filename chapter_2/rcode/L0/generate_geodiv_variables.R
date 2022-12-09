#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code calculates geodiversity metrics. It calculates SD of CHELSA biodiversity variables at multiple radii (3, 9, 15, 21, 27, 33 km). It calculcates the SD of elevation over the same radii. 

#Authors: Beth E. Gerstner

#Date: 1/21/22


library(geodiv)

library(raster)
library(rnaturalearth)
library(dplyr)
library(rgeos)
library(maps)
library(sf)

#Load 1km srtm data
srtm <- raster("/Volumes/BETH'S DRIV/zarnetske_lab/elevation/srtm_1km.tif")

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map to region of interest for future cropping. 

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(SA_study_region, xmin = -120, xmax = -30, ymin = -30, ymax = 40)

plot(study_region_crop)


# Crop SRTM raster to that of the study region
srtm_sr <- crop(srtm, study_region_crop)
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/CHELSA_4_only")
writeRaster(srtm_sr, filename="srtm_crop", format="GTiff")

## SRTM

# #set to a 3km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
# window <- matrix(1, nrow = 3, ncol = 3)
# sd <- focal_metrics(srtm_sr, window, metrics = list('sd'),
#                     progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x")
# writeRaster(sd$sd, filename="sd_3km", format="GTiff")

#set to a 9km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 9, ncol = 9)
sd.9 <- focal_metrics(srtm_sr, window, metrics = list('sd'),
                    progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x")
writeRaster(sd.9$sd, filename="sd_9km", format="GTiff")

# #set to a 15 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
# window <- matrix(1, nrow = 15, ncol = 15)
# sd.15 <- focal_metrics(srtm_sr, window, metrics = list('sd'),
#                     progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x")
# writeRaster(sd.15$sd, filename="sd_15km", format="GTiff")


#set to a 21 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 21, ncol = 21)
sd.21 <- focal_metrics(srtm_sr, window, metrics = list('sd'),
                       progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x")
writeRaster(sd.21$sd, filename="sd_21km", format="GTiff")


#set to a 27 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 27, ncol = 27)
sd.27 <- focal_metrics(srtm_sr, window, metrics = list('sd'),
                       progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x")
writeRaster(sd.27$sd, filename="sd_27km", format="GTiff")

#set to a 33 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 33 , ncol = 33)
sd.33 <- focal_metrics(srtm_sr, window, metrics = list('sd'),
                    progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x")
writeRaster(sd.33$sd, filename="sd_33km", format="GTiff")

## Cloud Cover

#read in mean annual cloud cover raste
cloud <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Cloud_forest_prediction_Jetz_2016/MODCF_meanannual.tiff")

cloud <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Cloud_forest_prediction_Jetz_2016/MODCF_CloudForestPrediction.tif")

# Crop cloud raster to that of the study region
cloud_sr <- crop(cloud, study_region_crop)
setwd("C:/Users/bgers/Desktop")
writeRaster(cloud_sr, filename="cloud_crop", format="GTiff")

#set to a 3km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 3, ncol = 3)
cloud.3 <- focal_metrics(cloud_sr, window, metrics = list('sd'),
                    progress = TRUE)
 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x")
writeRaster(cloud.3$sd, filename="cloud_3km", format="GTiff")

#set to a 9km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 9, ncol = 9)
cloud.9 <- focal_metrics(cloud_sr, window, metrics = list('sd'),
                      progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x")
writeRaster(cloud.9$sd, filename="cloud_9km", format="GTiff")

# #set to a 15 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 15, ncol = 15)
cloud.15 <- focal_metrics(cloud_sr, window, metrics = list('sd'),
                     progress = TRUE)
 
 setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x")
 writeRaster(cloud.15$sd, filename="cloud_15km", format="GTiff")


#set to a 21 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 21, ncol = 21)
cloud.21 <- focal_metrics(cloud_sr, window, metrics = list('sd'),
                       progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x")
writeRaster(cloud.21$sd, filename="cloud_21km", format="GTiff")


#set to a 27 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 27, ncol = 27)
cloud.27 <- focal_metrics(cloud_sr, window, metrics = list('sd'),
                       progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x")
writeRaster(cloud.27$sd, filename="cloud_27km", format="GTiff")

#set to a 33 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 33 , ncol = 33)
cloud.33 <- focal_metrics(cloud_sr, window, metrics = list('sd'),
                       progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x")
writeRaster(cloud.33$sd, filename="cloud_33km", format="GTiff")

## BioClim  variables

#Process environmental variables to only be for the study region
# Set working directory to folder where environmental data is stored 
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/CHELSA_full")

# Making stack of all 19 bioclimatic variables
env <- list.files(pattern='tif', full.names=TRUE)
env <- stack(env)

## Crop extent to a smaller region so it is easier to work with
# Crop environmental variables by the larger extent

env.clip <- crop(env, study_region_crop)

#write cropped Chelsa data to file
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/CHELSA_full/CHELSA_crop")
for (i in 1:19) {
  writeRaster(env.clip[[i]], paste0(strsplit(names(env.clip[[i]]),"CHELSA_1")[[1]][1],'.tif'))
}

#SD of CHELSA at 3 resolutions 3, 9, 15, 21, 27, 33
# variables
#Bio5: Max Temperature of Warmest Month
#Bio6: Min Temperature of Coldest Month
#Bio13: Precipitation of Wettest Month 
#Bio14: Precipitation of Driest Month 

#Pull bio5 out of stack
bio5 <- env.clip[[15]]

# #SD of bio5 3km
# window <- matrix(1, nrow = 3 , ncol = 3)
# bio5_sd_3km <- focal_metrics(bio5, window, metrics = list('sd'),
#                     progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x")
# writeRaster(bio5_sd_3km$sd, filename="bio5_sd_3km", format="GTiff")

#SD of bio5 9km
window <- matrix(1, nrow = 9 , ncol = 9)
bio5_sd_9km <- focal_metrics(bio5, window, metrics = list('sd'),
                             progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x")
writeRaster(bio5_sd_9km$sd, filename="bio5_sd_9km", format="GTiff")

# #SD of bio5 15km
# window <- matrix(1, nrow = 15 , ncol = 15)
# bio5_sd_15km <- focal_metrics(bio5, window, metrics = list('sd'),
#                              progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x")
# writeRaster(bio5_sd_15km$sd, filename="bio5_sd_15km", format="GTiff")

#SD of bio5 21km
window <- matrix(1, nrow = 21 , ncol = 21)
bio5_sd_21km <- focal_metrics(bio5, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x")
writeRaster(bio5_sd_21km$sd, filename="bio5_sd_21km", format="GTiff")

#SD of bio5 27km
window <- matrix(1, nrow = 27 , ncol = 27)
bio5_sd_27km <- focal_metrics(bio5, window, metrics = list('sd'),
                             progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x")
writeRaster(bio5_sd_27km$sd, filename="bio5_sd_27km", format="GTiff")

#SD of bio5 33km
window <- matrix(1, nrow = 33 , ncol = 33)
bio5_sd_33km <- focal_metrics(bio5, window, metrics = list('sd'),
                             progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x")
writeRaster(bio5_sd_33km$sd, filename="bio5_sd_33km", format="GTiff")
#______

#Pull bio6 out of stack
bio6 <- env.clip[[16]]

# #SD of bio6 3km
# window <- matrix(1, nrow = 3 , ncol = 3)
# bio6_sd_3km <- focal_metrics(bio6, window, metrics = list('sd'),
#                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x")
writeRaster(bio6_sd_3km$sd, filename="bio6_sd_3km", format="GTiff")

#SD of bio6 9km
window <- matrix(1, nrow = 9 , ncol = 9)
bio6_sd_9km <- focal_metrics(bio6, window, metrics = list('sd'),
                             progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x")
writeRaster(bio6_sd_9km$sd, filename="bio6_sd_9km", format="GTiff")

# #SD of bio6 15km
# window <- matrix(1, nrow = 15 , ncol = 15)
# bio6_sd_15km <- focal_metrics(bio6, window, metrics = list('sd'),
#                              progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x")
# writeRaster(bio6_sd_15km$sd, filename="bio6_sd_15km", format="GTiff")

#SD of bio6 21km
window <- matrix(1, nrow = 21 , ncol = 21)
bio6_sd_21km <- focal_metrics(bio6, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x")
writeRaster(bio6_sd_21km$sd, filename="bio6_sd_21km", format="GTiff")

#SD of bio6 27km
window <- matrix(1, nrow = 27 , ncol = 27)
bio6_sd_27km <- focal_metrics(bio6, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x")
writeRaster(bio6_sd_27km$sd, filename="bio6_sd_27km", format="GTiff")

#SD of bio6 33km
window <- matrix(1, nrow = 33 , ncol = 33)
bio6_sd_33km <- focal_metrics(bio6, window, metrics = list('sd'),
                             progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x")
writeRaster(bio6_sd_33km$sd, filename="bio6_sd_33km", format="GTiff")

#______

#Pull bio13 out of stack
bio13 <- env.clip[[5]]


# #SD of bio13 3km
# window <- matrix(1, nrow = 3 , ncol = 3)
# bio13_sd_3km <- focal_metrics(bio13, window, metrics = list('sd'),
#                              progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x")
# writeRaster(bio13_sd_3km$sd, filename="bio13_sd_3km", format="GTiff")

#SD of bio13 9km
window <- matrix(1, nrow = 9 , ncol = 9)
bio13_sd_9km <- focal_metrics(bio13, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x")
writeRaster(bio13_sd_9km$sd, filename="bio13_sd_9km", format="GTiff")


# #SD of bio13 15km
# window <- matrix(1, nrow = 15 , ncol = 15)
# bio13_sd_15km <- focal_metrics(bio13, window, metrics = list('sd'),
#                              progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x")
# writeRaster(bio13_sd_15km$sd, filename="bio13_sd_15km", format="GTiff")

#SD of bio13 21km
window <- matrix(1, nrow = 21 , ncol = 21)
bio13_sd_21km <- focal_metrics(bio13, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x")
writeRaster(bio13_sd_21km$sd, filename="bio13_sd_21km", format="GTiff")

#SD of bio13 27km
window <- matrix(1, nrow = 27 , ncol = 27)
bio13_sd_27km <- focal_metrics(bio13, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x")
writeRaster(bio13_sd_27km$sd, filename="bio13_sd_27km", format="GTiff")

#SD of bio13 33km
window <- matrix(1, nrow = 33 , ncol = 33)
bio13_sd_33km <- focal_metrics(bio13, window, metrics = list('sd'),
                             progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x")
writeRaster(bio13_sd_33km$sd, filename="bio13_sd_33km", format="GTiff")

#______

#Pull bio14 out of stack
bio14 <- env.clip[[6]]

# #SD of bio13 3km
# window <- matrix(1, nrow = 3 , ncol = 3)
# bio14_sd_3km <- focal_metrics(bio14, window, metrics = list('sd'),
#                               progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x")
# writeRaster(bio14_sd_3km$sd, filename="bio14_sd_3km", format="GTiff")

#SD of bio13 9km
window <- matrix(1, nrow = 9 , ncol = 9)
bio14_sd_9km <- focal_metrics(bio14, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x")
writeRaster(bio14_sd_9km$sd, filename="bio14_sd_9km", format="GTiff")

# #SD of bio14 15km
# window <- matrix(1, nrow = 15 , ncol = 15)
# bio14_sd_15km <- focal_metrics(bio14, window, metrics = list('sd'),
#                               progress = TRUE)
# 
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x")
# writeRaster(bio14_sd_15km$sd, filename="bio14_sd_15km", format="GTiff")

#SD of bio14 21km
window <- matrix(1, nrow = 21 , ncol = 21)
bio14_sd_21km <- focal_metrics(bio14, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x")
writeRaster(bio14_sd_21km$sd, filename="bio14_sd_21km", format="GTiff")

#SD of bio14 27km
window <- matrix(1, nrow = 27 , ncol = 27)
bio14_sd_27km <- focal_metrics(bio14, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x")
writeRaster(bio14_sd_27km$sd, filename="bio14_sd_27km", format="GTiff")

#SD of bio14 33km
window <- matrix(1, nrow = 33 , ncol = 33)
bio14_sd_33km <- focal_metrics(bio14, window, metrics = list('sd'),
                              progress = TRUE)

setwd("/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x")
writeRaster(bio14_sd_33km$sd, filename="bio14_sd_33km", format="GTiff")
