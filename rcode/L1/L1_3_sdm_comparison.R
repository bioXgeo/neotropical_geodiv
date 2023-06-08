#Title: SDM Prediction processing

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code prepares all SDM predictions for future comparisons. Models (no geodiversity, geodiversity, and expert models) all need to have the same extent and projection. Each prediction was cropped to the extent of Colombia and a binary version was made. We also clip each optimal model for each species by a species-specific clipping extent generated in QGIS by references described and known barriers to species dispersal (IUCN).

#Data input: SDM predictions that were outputs of modeling code; species specific clipping regions generated in QGIS.

#Data output: Binary and continuous SDM predictions that have dispersal barriers removed for each species.

#Author: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills

library(raster)
library(ENMTools)
library(rgdal)
library(sf)

#crop the SDMs by the expert model so they are the same extent

# set working directory to folder with geodiversity and expert model rasters
setwd("INSERT FILE PATH")

# get list of geodiversity rasters and expert model raster
geo_rasters <- list.files("geodiv_optimal_models", pattern = "_mean.tif", full.names = TRUE)
expert_raster <- list.files("expert_maps", pattern = "_expert.tif", full.names = TRUE)
no_geo_rasters <- list.files("no_geodiv_models", pattern = "_mean.tif", full.names = TRUE)

# Extract species names from file paths
species_names <- gsub("^(.*?)_(.*?)_.*$", "\\1_\\2", basename(geo_rasters))

# load shapefile of Colombia
colombia_shp <- readOGR("INSERT FILE PATH")

# Get only unique species names
species_list <- unique(species_names)

# loop through each species and check that expert and geodiversity models are the same extent. If they are not, this loop crops to make them the same extent.

for (species_name in species_list) {
  # get list of geodiversity rasters and expert model raster
  geo_rasters <- list.files("geodiv_optimal_models", pattern = "_mean.tif", full.names = TRUE)
  expert_raster <- list.files("expert_maps", pattern = "_expert.tif", full.names = TRUE)
  no_geo_rasters <- list.files("no_geodiv_models", pattern = "_mean.tif", full.names = TRUE)
  
  # get file paths for all geodiversity rasters and expert model raster for this species
  geo_files <- grep(species_name, geo_rasters, value = TRUE) #change to species_name
  expert_file <- grep(species_name, expert_raster, value = TRUE)
  
  # load expert model raster and get extent
  expert_raster <- raster(expert_file)
  expert_ncells <- ncell(expert_raster)
  
  # loop through each geodiversity raster for this species
  for (geo_file in geo_files) {
    
    geo_raster <- raster(geo_files)
    geo_ncells <- ncell(geo_raster)
    
    if (expert_ncells > geo_ncells) {
      # crop expert raster to geo raster extent
      expert_crop <- crop(expert_raster, geo_raster)
      
      # mask expert crop with Colombia shapefile
      expert_crop_masked <- mask(expert_crop, colombia_shp)
      
      geo_dir <- dirname(expert_file)
      output_file <- file.path(geo_dir, paste0(species_name, "_expert_cropped.tif"))
      writeRaster(expert_crop_masked, output_file, format = "GTiff", overwrite = TRUE)
      
    } else {
      # crop geo raster to expert raster extent
      geo_crop <- crop(geo_raster, expert_raster)
     geo_crop[is.na(geo_crop)] <- 0
     geo_crop[geo_crop > 0] <- 1
      
      # mask geo crop with Colombia shapefile
      geo_crop_masked <- mask(geo_crop, colombia_shp)
      
      geo_dir <- dirname(geo_file)
      output_file <- file.path(geo_dir, paste0(species_name, "_geo_cropped_bin.tif"))
      writeRaster(geo_crop_masked, output_file, format = "GTiff", overwrite = TRUE)
    }
  }
}

# Loop through each species and check that expert and no- geodiversity models are the same extent. If they are not, this loop crops to make them the same extent.

  for (species_name in species_list) {
    # load expert model raster and get extent
    # get list of geodiversity rasters and expert model raster
    expert_raster <- list.files("expert_maps", pattern = "_expert.tif", full.names = TRUE)
    no_geo_rasters <- list.files("no_geodiv_models", pattern = "_mean.tif", full.names = TRUE)
    
    # get file paths for all geodiversity rasters and expert model raster for this species
    no_geo_files <- grep(species_name, no_geo_rasters, value = TRUE) #change to species_name
    expert_file <- grep(species_name, expert_raster, value = TRUE)
    
    # loop through each geodiversity raster for this species
    for (no_geo_file in no_geo_files) {
      
      # get file paths for all geodiversity rasters and expert model raster for this species
      no_geo_files <- grep(species_name, no_geo_rasters, value = TRUE) #change to species_name
      expert_file <- grep(species_name, expert_raster, value = TRUE)
      expert_raster <- raster(expert_file)
      
      # load geodiversity raster and crop to expert model extent
      no_geo_raster <- raster(no_geo_file)  #change to geo_file
      no_geo_raster[is.na(no_geo_raster)] <- 0
     no_geo_raster[no_geo_raster > 0] <- 1
      no_geo_raster <- crop(no_geo_raster, expert_raster)
      
      
      # mask geo crop with Colombia shapefile
      no_geo_crop_masked <- mask(no_geo_raster, colombia_shp)
      geo_dir <- dirname(no_geo_file) #change to geofile
      
      # Construct output file path with "_cropped" appended
      output_file <- file.path(geo_dir, paste0(species_name,"_no_geo_cropped_bin.tif")) #change to species_name
      
      # Save cropped geodiversity raster
      writeRaster(no_geo_crop_masked, output_file, format = "GTiff", overwrite = TRUE)
      
    }
  
}
## Species-Specific clipping extents  
#We also need to crop optimal models (those made at spatial grains leading to highest performance for each species) by species-specific clipping extents generated externally (QGIS). These clipping extents remove areas from the prediction beyond where each species can disperse so we can make direct comparisons between the expert maps and our models. 

## Binary version
#Loop to crop all the optimal distribution maps and no geodiversity maps by cropping extent 

#setwd to where the clipping shapefiles are saved
setwd("INSERT FILE PATH")

#list shapefiles
shapefiles <-  list.files("clipping_extent", pattern = "_clipping.shp", full.names = TRUE)

# Mask binary optimal models by species specific clipping extents
for (species_name in species_list) {
  # get list of geodiversity rasters and expert model raster
  shapefiles <- list.files("clipping_extent", pattern = "_clipping.shp", full.names = TRUE)
  rasters <- list.files("geodiv_optimal_models", pattern = "_bin.tif", full.names = TRUE)
  # Load the shapefile and raster for the current species
  shapefile <- paste0("clipping_extent/", species_name, "_clipping.shp")
  raster <- paste0("geodiv_optimal_models/", species_name, "_geo_cropped_bin.tif")
  
  # Read shapefile and raster
  shp <- st_read(shapefile)
  rst <- raster(raster)
  
  # Mask raster with shapefile
  masked_raster <- mask(rst, shp)
  
  # Save masked raster
  output_dir <- ("INSERT FILE PATH")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_geodiv_masked.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}


# Continuous version
# Loop to crop all the optimal distribution maps and no geodiversity maps by cropping extent

for (species_name in species_list) {
  # get list of geodiversity rasters and expert model raster
  shapefiles <- list.files("clipping_extent", pattern = "_clipping.shp", full.names = TRUE)
  rasters <- list.files("geodiv_optimal_models", pattern = "_cropped.tif", full.names = TRUE)
  # Load the shapefile and raster for the current species
  shapefile <- paste0("clipping_extent/", species_name, "_clipping.shp")
  raster <- paste0("geodiv_optimal_models/", species_name, "_geo_cropped.tif")
  
  # Read shapefile and raster
  shp <- st_read(shapefile)
  rst <- raster(raster)
  
  # Mask raster with shapefile
  masked_raster <- mask(rst, shp)
  
  # Save masked raster
  output_dir <- ("INSERT FILE PATH")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_geodiv_masked_cont.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}

#No geodiversity
#Binary version
for (species_name in species_list) {
  # get list of geodiversity rasters and expert model raster
  shapefiles <- list.files("clipping_extent", pattern = "_clipping.shp", full.names = TRUE)
  rasters <- list.files("no_geodiv_models", pattern = "_bin.tif", full.names = TRUE)
  # Load the shapefile and raster for the current species
  shapefile <- paste0("clipping_extent/", species_name, "_clipping.shp")
  raster <- paste0("no_geodiv_models/", species_name, "_no_geo_cropped_bin.tif")
  
  # Read shapefile and raster
  shp <- st_read(shapefile)
  rst <- raster(raster)
  
  # Mask raster with shapefile
  masked_raster <- mask(rst, shp)
  
  # Save masked raster
  output_dir <- ("INSERT FILE PATH")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_no_geodiv_masked.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}


# Continuous version
for (species_name in species_list) {
  # get list of geodiversity rasters and expert model raster
  shapefiles <- list.files("clipping_extent", pattern = "_clipping.shp", full.names = TRUE)
  rasters <- list.files("no_geodiv_models", pattern = "_cropped.tif", full.names = TRUE)
  # Load the shapefile and raster for the current species
  shapefile <- paste0("clipping_extent/", species_name, "_clipping.shp")
  raster <- paste0("no_geodiv_models/", species_name, "_no_geo_cropped.tif")
  
  # Read shapefile and raster
  shp <- st_read(shapefile)
  rst <- raster(raster)
  
  # Mask raster with shapefile
  masked_raster <- mask(rst, shp)
  
  # Save masked raster
  output_dir <- ("INSERT FILE PATH")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_no_geodiv_masked_cont.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}


