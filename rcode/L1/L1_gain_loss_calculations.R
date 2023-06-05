#Gain Loss Comparisons
#Pull in optimal model for every species

#read in model results
perf_stats <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/stats_mean.csv")

highest_cbi_per_species <- perf_stats %>%
  group_by(species) %>%
  slice_max(order_by = cbi.train, n = 1)


print(highest_cbi_per_species)



# Define function to calculate gain and loss of geodiversity and non-geodiversity models
calc_gain_loss <- function(species_name, geodiversity_dir, no_geodiversity_dir, expert_maps_dir) {
  

  geodiversity_map <- raster(paste0(geodiversity_dir, "/", species_name, "_geo_cropped_bin.tif"))
  no_geodiversity_map <- raster(paste0(no_geodiversity_dir, "/", species_name, "_no_geo_cropped_bin.tif"))
  expert_map <- raster(paste0(expert_maps_dir, "/", species_name, "_expert.tif"))
  
  # Resample geodiversity and non-geodiversity maps to match expert map
  resample_maps(species_name, geodiversity_map, no_geodiversity_map, expert_map)
  
  # Load resampled geodiversity and non-geodiversity maps
  geodiversity_map_resampled <- raster(paste0(geodiversity_dir, "/", species_name, "geo_resampled_masked.tif"))
  no_geodiversity_map_resampled <- raster(paste0(no_geodiversity_dir, "/", species_name, "no_geo_resampled_masked.tif"))
  
  # Calculate gain and loss for geodiversity and non-geodiversity models
  gain_geodiversity <- cellStats(!is.na(geodiversity_map_resampled) & geodiversity_map_resampled == 1 & expert_map == 0, sum)
  
  loss_geodiversity <- cellStats(!is.na(geodiversity_map_resampled) & geodiversity_map_resampled == 0 & expert_map == 1, sum)
  gain_no_geodiversity <- cellStats(!is.na(no_geodiversity_map_resampled) & no_geodiversity_map_resampled == 1 & expert_map == 0, sum)
  loss_no_geodiversity <- cellStats(!is.na(no_geodiversity_map_resampled) & no_geodiversity_map_resampled == 0 & expert_map == 1, sum)
  
  total_geodiversity <- ncell(geodiversity_map_resampled)
  
  
  percentage_gain_geodiversity <- (gain_geodiversity / total_geodiversity) * 100
  percentage_loss_geodiversity <- (loss_geodiversity / total_geodiversity) * 100
  percentage_gain_no_geodiversity <- (gain_no_geodiversity / total_geodiversity) * 100
  percentage_loss_no_geodiversity <- (loss_no_geodiversity / total_geodiversity) * 100
  
  # Calculate Schoener's D using calc.niche.overlap function
  schoener_stack_geo <- stack(geodiversity_map_resampled, expert_map)
  schoener_geodiversity <- calc.niche.overlap(schoener_stack_geo,"D")
  schoener_stack_no_geo <- stack(no_geodiversity_map_resampled, expert_map)
  schoener_no_geodiversity <- calc.niche.overlap(schoener_stack_no_geo, "D")
  
  # Return results as a data frame
  data.frame(species = species_name,
             gain_geodiversity = percentage_gain_geodiversity,
             loss_geodiversity = percentage_loss_geodiversity,
             gain_no_geodiversity = percentage_gain_no_geodiversity,
             loss_no_geodiversity = percentage_loss_no_geodiversity,
             schoener_geodiversity = schoener_geodiversity[2,1],
             schoener_no_geodiversity = schoener_no_geodiversity[2,1])
}

# Define function to resample maps to match expert map
resample_maps <- function(species_name, geodiversity_map, no_geodiversity_map, expert_map) {
  
  # Resample geodiversity map to match expert map
  geodiversity_map_resampled <- resample(geodiversity_map, expert_map, method = "ngb")
  geodiversity_map_resampled <- mask(geodiversity_map_resampled, expert_map)
  
  # Resample non-geodiversity map to match expert map
  no_geodiversity_map_resampled <- resample(no_geodiversity_map, expert_map, method = "ngb")
  no_geodiversity_map_resampled <- mask(no_geodiversity_map_resampled, expert_map)
  
  # Save resampled maps to file
  writeRaster(geodiversity_map_resampled, paste0(geodiversity_dir, "/", species_name, "geo_resampled_masked.tif"), overwrite = TRUE)
  writeRaster(no_geodiversity_map_resampled, paste0(no_geodiversity_dir, "/", species_name, "no_geo_resampled_masked.tif"), overwrite = TRUE)
}

# Define directory paths
geodiversity_dir <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/geodiv_optimal_models"
no_geodiversity_dir <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/no_geodiv_models"
expert_maps_dir <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/expert_maps"

# Get list of files in each directory
geodiversity_files <- list.files(geodiversity_dir, full.names = TRUE)
no_geodiversity_files <- list.files(no_geodiversity_dir, full.names = TRUE)
expert_maps_files <- list.files(expert_maps_dir, full.names = TRUE)

# Extract species names from file names
geodiversity_species <-  gsub("^(.*?)_(.*?)_.*$", "\\1_\\2", basename(geodiversity_files))

no_geodiversity_species <- gsub("^(.*?)_(.*?)_.*$", "\\1_\\2", basename(no_geodiversity_files))
expert_maps_species <- gsub("^(.*?)_(.*?)_.*$", "\\1_\\2", basename(expert_maps_files))

# Get unique species names
all_species <- unique(c(geodiversity_species, no_geodiversity_species, expert_maps_species))

# Initialize empty data frame to store results
results <- data.frame()

# Loop over each species and calculate gain/loss and Schoener's D for each model
for (species_name in all_species) {
  # Construct file paths for current species
  geodiversity_file <- paste0(geodiversity_dir, "/", species_name[1], "_geo_cropped_bin.tif")
  no_geodiversity_file <- paste0(no_geodiversity_dir, "/", species_name[1], "_no_geo_cropped_bin.tif")
  expert_map_file <- paste0(expert_maps_dir, "/", species_name[1], "_expert.tif")
  
  # Check if all files exist for current species
  if (file.exists(geodiversity_file) & file.exists(no_geodiversity_file) & file.exists(expert_map_file)) {
    # Calculate gain/loss and Schoener's D for the current species
    species_results <- calc_gain_loss(species_name, geodiversity_dir, no_geodiversity_dir, expert_maps_dir)
    
    # Append results to overall data frame
    results <- rbind(results, species_results)
  }
}

# Save results to file
write.csv(results, "species_results.csv", row.names = FALSE)
