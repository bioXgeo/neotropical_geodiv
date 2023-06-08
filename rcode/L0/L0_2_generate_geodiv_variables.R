#Title: Generating geodiversity variables

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code calculates geodiversity metrics. It calculates sq of CHELSA biodiversity variables at multiple radii (3, 9, 15, 21, 27, 33 km). It calculcates the sq of elevation over the same radii. 

#Inputs: Uses SRTM elevation data, annual cloud cover (Wilson & Jetz, 2014), and 4 preselected CHELSA Bioclim variables (https://chelsa-climate.org/) to generate geodiversity measures at various radii. 

#Outputs: raster files for each variable with values calculated over 6 different radii.

#Authors: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills

#Date: 2/2/23

#Load libraries
library(geodiv)
library(raster)
library(rnaturalearth)
library(dplyr)
library(rgeos)
library(maps)
library(sf)
library(rgdal)


# Process and prepare SRTM elevation data
srtm <- raster("INSERT FILE PATH")

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map to region of interest for future cropping. 
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(SA_study_region, xmin = -120, xmax = -30, ymin = -30, ymax = 40)

plot(study_region_crop)

# Crop SRTM raster to that of the study region
srtm_sr <- crop(srtm, study_region_crop)

setwd("INSERT FILE PATH")
writeRaster(srtm_sr, filename="srtm_crop", format="GTiff")


## SRTM
# Calculate sq of SRTM at all radii mentioned above.

# Set working directory to save
setwd("INSERT FILE PATH")

# Set radii to loop over
radii <- c(3,9,15,21,27,33)

# Run loop for srtm
for (i in radii) {
  window <- matrix(1, nrow = i, ncol = i)
  sq.srtm <- focal_metrics(srtm_sr, window, metrics = list('sq'),
                           progress = TRUE)
  f.name <- paste0('srtm_sq_', i, '.tif')
  writeRaster(sq.srtm$sq, filename=f.name, format="GTiff", overwrite=T)
}

## Cloud Cover
# Read in mean annual cloud cover raster from Jetz 2016 (generated from MODIS) and process raster
cloud <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Cloud_forest_prediction_Jetz_2016/MODCF_meanannual.tiff")


# Crop cloud raster to that of the study region
cloud_sr <- crop(cloud, study_region_crop)
#setwd("C:/Users/bgers/Desktop")
#writeRaster(cloud_sr, filename="cloud_crop", format="GTiff")

# Set to a 3km window same as above
cloud_sr <- raster("INSERT FILE PATH")

# Set radii to loop over
radii <- c(3,9,15,21,27,33)

# Run loop for cloud cover
for (i in radii) {
  window <- matrix(1, nrow = i, ncol = i)
  cloud_sq <- focal_metrics(cloud_sr, window, metrics = list('sq'),
                            progress = TRUE)
  f.name <- paste0('cloud_sq_', i, '.tif')
  writeRaster(cloud_sq$sq, filename=f.name, format="GTiff", overwrite=T)
}


## BioClim  variables
#Process environmental variables to only be for the study region
#Set working directory to folder where environmental data is stored 
setwd("INSERT FILE PATH")


# Making stack of all 19 bioclimatic variables
env <- list.files(pattern='tif', full.names=TRUE)
env <- stack(env)

# Crop extent to a smaller region so it is easier to work with
env.clip <- crop(env, study_region_crop)

# Write cropped CHELSA data to file
setwd("INSERT FILE PATH")
for (i in 1:19) {
  writeRaster(env.clip[[i]], paste0(strsplit(names(env.clip[[i]]),"CHELSA_1")[[1]][1],'.tif'))
}

#sq of CHELSA at 3 resolutions 3, 9, 15, 21, 27, 33
# variables
#Bio5: Max Temperature of Warmest Month
#Bio6: Min Temperature of Coldest Month
#Bio13: Precipitation of Wettest Month 
#Bio14: Precipitation of Driest Month 

# Generating the CHELSA geodiversity rasters
# Bio5
bio5_sr <- raster("INSERT FILE PATH")
radii <- c(3,9,15,21,27,33)
for (i in radii) {
  window <- matrix(1, nrow = i, ncol = i)
  bio5_sq <- focal_metrics(bio5_sr, window, metrics = list('sq'),
                           progress = TRUE)
  f.name <- paste0('bio5_sq_', i, '.tif')
  writeRaster(bio5_sq$sq, filename=f.name, format="GTiff", overwrite=T)
}


# Bio6
bio6_sr <- raster("INSERT FILE PATH")
radii <- c(3,9,15,21,27,33)
for (i in radii) {
  window <- matrix(1, nrow = i, ncol = i)
  bio6_sq <- focal_metrics(bio6_sr, window, metrics = list('sq'),
                           progress = TRUE)
  f.name <- paste0('bio6_sq_', i, '.tif')
  writeRaster(bio6_sq$sq, filename=f.name, format="GTiff", overwrite=T)
}

# Bio13
bio13_sr <- raster("INSERT FILE PATH")
radii <- c(3,9,15,21,27,33)
for (i in radii) {
  window <- matrix(1, nrow = i, ncol = i)
  bio13_sq <- focal_metrics(bio13_sr, window, metrics = list('sq'),
                            progress = TRUE)
  f.name <- paste0('bio13_sq_', i, '.tif')
  writeRaster(bio13_sq$sq, filename=f.name, format="GTiff", overwrite=T)
}

# Bio14
bio14_sr <- raster("INSERT FILE PATH")
radii <- c(3,9,15,21,27,33)
for (i in radii) {
  window <- matrix(1, nrow = i, ncol = i)
  bio14_sq <- focal_metrics(bio14_sr, window, metrics = list('sq'),
                            progress = TRUE)
  f.name <- paste0('bio14_sq_', i, '.tif')
  writeRaster(bio14_sq$sq, filename=f.name, format="GTiff", overwrite=T)
}


