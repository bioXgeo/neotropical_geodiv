#Title: Rescaling expert maps

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: Some expert maps only had values of 1 and all unsuitable pixels had values of NA. To allow comparisons, we needed to rescale these maps to show 1: suitable and 0: unsuitable.

#Inputs: Expert maps for three species, Nasuella olivacea, Bassaricyon neblina, Tremarctos ornatus obtained from the BioModelos website (http://biomodelos.humboldt.org.co/)

#Outputs: Rescaled rasters for each species.

#Authors: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills

#Date: 2/2/23

# Read in the expert map raster for Nasuella olivacea
expert_map_no <- raster("INSERT FILE PATH")

# Create a new raster with the same extent and resolution as the expert map
new_expert_map_no <- raster(extent(expert_map_no), nrow = nrow(expert_map_no), ncol = ncol(expert_map_no), crs = crs(expert_map_no))

# Set all cells to 0 by default
new_expert_map_no[] <- 0

# Change the value of cells in the new raster that are 1 in the original expert map
new_expert_map_no[expert_map_no == 1] <- 1

# Change working directly to where you want to save
setwd("INSERT FILE PATH")
writeRaster(new_expert_map_no, "Nasuella_olivacea_expert.tif", format="GTiff", overwrite=T)

#B. neblina
# Read in the expert map raster
expert_map_bn <- raster("INSERT FILE PATH")

# Create a new raster with the same extent and resolution as the expert map
new_expert_map_bn <- raster(extent(expert_map_bn), nrow = nrow(expert_map_bn), ncol = ncol(expert_map_bn), crs = crs(expert_map_bn))

# Set all cells to 0 by default
new_expert_map_bn[] <- 0

# Change the value of cells in the new raster that are 1 in the original expert map
new_expert_map_bn[expert_map_bn == 1] <- 1

# set working directory to where you want to save
setwd("INSERT FILE PATH")
writeRaster(new_expert_map_bn, "Bassaricyon_neblina_expert.tif", format="GTiff", overwrite=T)

# t. ornatus
# Read in the expert map raster
expert_map_to <- raster("INSERT FILE PATH")

# Create a new raster with the same extent and resolution as the expert map
new_expert_map_to <- raster(extent(expert_map_to), nrow = nrow(expert_map_to), ncol = ncol(expert_map_to), crs = crs(expert_map_to))

# Set all cells to 0 by default
new_expert_map_to[] <- 0

# Change the value of cells in the new raster that are 1 in the original expert map
new_expert_map_to[expert_map_to == 1] <- 1

#set working directory to where you want to save
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/expert_maps")
writeRaster(new_expert_map_to, "Tremarctos_ornatus_expert.tif", format="GTiff", overwrite=T)
