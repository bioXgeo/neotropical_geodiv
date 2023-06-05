#SDM standardization and clipping code
#description: This code is designed to loop through a directory containing multiple subdirectories, each representing a different species. Within each species subdirectory, the code reads in raster models for the expert, nogeo, and geo models. It then loops through each of the geo models and calculates the gain/loss in comparison to the expert and nogeo models. The code saves each gain/loss raster as a separate TIFF file with a filename that includes the species name and the index of the geo model being compared. Finally, the code closes all open raster files.

library(raster)
library(ENMTools)

#crop the SDMs by the expert model so they are the same extent

# set working directory to folder with geodiversity and expert model rasters
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models")

# get list of geodiversity rasters and expert model raster
geo_rasters <- list.files("geodiv_optimal_models", pattern = "_mean.tif", full.names = TRUE)
expert_raster <- list.files("expert_maps", pattern = "_expert.tif", full.names = TRUE)
no_geo_rasters <- list.files("no_geodiv_models", pattern = "_mean.tif", full.names = TRUE)

# Extract species names from file paths
species_names <- gsub("^(.*?)_(.*?)_.*$", "\\1_\\2", basename(geo_rasters))

# Get only unique species names
species_list <- unique(species_names)

# loop through each species
library(raster)
library(rgdal)

# load shapefile of Colombia
colombia_shp <- readOGR("D:/Anderson_Lab_Archive/LatinAmerica/CO.shp")

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

  # loop through each species
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
  
#Loop to crop all the optimal distribution maps and no geodiversity maps by cropping extent
#setwd to where the clipping shapefiles are saved

setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models")

shapefiles <-  list.files("clipping_extent", pattern = "_clipping.shp", full.names = TRUE)

library(sf)
library(raster)
#binary geodiv

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
  output_dir <- ("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/geodiv_optimal_models/")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_geodiv_masked.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}


#continuous

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
  output_dir <- ("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/geodiv_optimal_models/")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_geodiv_masked_cont.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}

#No geodiversity


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
  output_dir <- ("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/no_geodiv_models/")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_no_geodiv_masked.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}


#continuous

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
  output_dir <- ("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/no_geodiv_models/")
  masked_raster_filepath <- file.path(output_dir, paste0(species_name, "_no_geodiv_masked_cont.tif"))
  writeRaster(masked_raster, masked_raster_filepath, format = "GTiff", overwrite = TRUE)
}


