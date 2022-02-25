#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code calculates geodiversity metrics.

#Authors: Beth E. Gerstner

#Date: 1/21/22

install.packages("geodiv")
library(geodiv)
library(raster)
library(rnaturalearthdata)
library(dplyr)
library(rgeos)
library(maps)
library(sf)

#Load 1km srtm data
srtm <- raster("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/elevation/srtm_1km.tif")

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map to region of interest for future cropping. 

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(SA_study_region, xmin = -83, xmax = -65, ymin = -7, ymax = 13)

# Crop SRTM raster to that of the study region
srtm_sr <- crop(srtm, study_region_crop)

#set to a 3km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 3, ncol = 3)
sd <- focal_metrics(srtm_sr, window, metrics = list('sd'),
                    progress = TRUE)

#set to a 11 km window, which is appropriate given the home range of the species being tested (this may change in future iterations)
window <- matrix(1, nrow = 11, ncol = 11)
sd <- focal_metrics(srtm_sr, window, metrics = list('sd'),
                    progress = TRUE)

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/geodiv_param")
writeRaster(sd$sd, filename="sd_11km", format="GTiff")

#Process environmental variables to only be for the study region
# Set working directory to folder where environmental data is stored 
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/CHELSA")

# Making stack of all 19 bioclimatic variables
env <- list.files(pattern='tif', full.names=TRUE)
env <- stack(env)

## Crop extent to a smaller region so it is easier to work with
# Crop environmental variables by the larger extent

env.clip <- crop(env, study_region_crop)

#write cropped Chelsa data to file
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/CHELSA/cropped_chelsa")
for (i in 1:19) {
  writeRaster(env.clip[[i]], paste0(strsplit(names(env.clip[[i]]),"CHELSA_1")[[1]][1],'.tif'))
}

