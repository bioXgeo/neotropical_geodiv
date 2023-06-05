folder_path <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/occurrence_records"
env_crop <-"C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/env_radii"
output <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/kernel_density"

library(rgeos)
library(dismo)


# Define a function to create an MCP
simpleMCP <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  p <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1)))
}

# Define a function to generate kernel density plots for multiple species and multiple pairs of environmental variables
# Define a function to generate kernel density plots for multiple species and multiple pairs of environmental variables
multiSpeciesKDE <- function(folder_path, env_crop) {
  
  # Get list of subfolders in the folder
  subfolders <- list.dirs(folder_path, recursive = FALSE)
  
  # Loop through each subfolder
  for (subfolder in subfolders) {
    
    # Get list of files in the subfolder
    files <- list.files(subfolder, pattern = "\\_thin1.csv$", full.names = TRUE)
    
    # Loop through each file
    for (file in files) {
      
      # Read in occurrence records
      pts.sp <- read.csv(file)
      coords <- cbind(pts.sp$decimalLongitude, pts.sp$decimalLatitude)
      pts.sp <- SpatialPointsDataFrame(coords, pts.sp)
      
      # Loop through each pair of environmental variables ending in the same pattern
      # Define a vector of environmental variable names
      env_names <- list.files(env_crop, pattern = "\\.tif$", full.names = TRUE)
      env_names <- basename(env_names)
      env_names <- sub("\\.tif$", "", env_names, fixed=TRUE)
      
      # Find unique endings in the environmental variable names
      unique_endings <- unique(gsub(".*_([^_]+)\\.tif$", "\\1", env_names))
      
      # Loop through each unique ending and pair the matching environmental variables
      for (ending in unique_endings) {
        # Find the indices of the environmental variables that end with the current ending
        env_indices <- grep(paste0("_", ending), env_names)
        
        # If there are at least two environmental variables with the current ending, pair them
        if (length(env_indices) >= 2) {
          # Pair the environmental variables
          env1 <- env_indices[1]
          env2 <- env_indices[2]
          
          # Perform analysis with the paired environmental variables
          env1 <- lapply(file.path(env_crop, env_names[env1]), raster::raster)
          env2 <- lapply(file.path(env_crop, env_names[env2]), raster::raster)
          
          # Extract the two environmental variables
          env <- raster::stack(env2[[1]], env1[[1]])
  
              # Generate MCP  
              mcp <- simpleMCP(pts.sp)
              
              # Buffer MCP by 0.7 deg and mask one environmental variable raster by it
              mcp_buf <- gBuffer(mcp, width=.6)
              mcp_buf_mask <- mask(env, mcp_buf)
              
              # Create background points for the future density plot
              mcp.backg.density <- as.data.frame(randomPoints(mcp_buf_mask, n=10000, excludep=FALSE))
              coords.backg <- cbind(mcp.backg.density[,1], mcp.backg.density[,2])
              mcp.backg.density <- SpatialPointsDataFrame(coords.backg, mcp.backg.density)
              
              
              # Extract environmental values for background points
              mcp_backg_dens <- extract(env, mcp.backg.density)
              
              # Extract environmental values for occurrence records
              occ_env <- as.data.frame(extract(env, pts.sp))
              
              # Get values for the two environmental variables from background environments
              y <- mcp_backg_dens[,1:2]
              
              # calculate the range of the lowest 70% values
             #range_99 <- quantile(y[,1], 0.999) - min(y[,1])
              
              # set the xlim to the lowest 70% range
             xlim <- c(0, 1000)
              
              # calculate the range of the lowest 70% values
            # range_99 <- quantile(y[,2], 0.999) - min(y[,2])
              
              # set the xlim to the lowest 70% range
             ylim <- c(0, 7)
              
              library(MASS)
              # Builds Two-Dimensional Kernel Density Estimation
              kde2 <- kde2d(y[,1], y[,2], n=1000, lims = cbind(xlim, ylim), h = c(400, 4))
              
              
              # Save density plot as a high-quality PNG file in the subfolder
              png(file.path(output, paste0(substr(basename(file), 1, nchar(basename(file))-4), "_", ending, ".png")))
              
              # Show density plot
              image(kde2, col= gray.colors(n=20, start=1, end=.0000001), xlab='SQ of SRTM elevation (m): ', ylab='SQ of min temp of coldest month ', xlim = xlim, ylim = ylim, cex=1)
              box()
              
              # This formula allows us to calculate the value of N at which 99% of the all density values are above N. Plugged in numbers until reaching 99%.
              sum(kde2$z[kde2$z >= .000000616]) / sum(kde2$z)
              levs3 <- c(0, 0.75*(max(kde2$z)))
              # Draws the contour lines on the density plot
              contour(kde2,levels=levs3, drawlabels=T, col="black", labcex=1, offset=4, labels="75%", add=T ) #99%
  
              #place occurrence records on plot
              points(occ_env[, 1], occ_env[, 2], col = 'blue', pch = 9, cex=1)
              
              # Close the PNG file
              dev.off()
            }
          }
        }
      }
}

multiSpeciesKDE(folder_path, env_crop)

                               #create plots for each species
# set the path to the directory containing the PNG files



library(ggplot2)
library(gridExtra)
library(png)
# Set the working directory to where the PNG files are saved
setwd("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/kernel_density")

# Get a list of all PNG files in the directory
png_files <- list.files(pattern = "/\.png$")

# Sort the files by the species name and the number at the end of the file name
png_files_1 <- png_files[order(sub("_.*", "", png_files), as.numeric(sub(".*_([0-9]+)\\.png$", "\\1", png_files)))]

png(width = 1200, height = 800, res = 400)


# Define the dimensions of the multi-panel plot
par(mfrow = c(2, 3), mar = c(0, 0, 0, 0))

# Loop over each unique species name in the list of PNG files
for (species in unique(sapply(png_files_1, function(x) paste(strsplit(x, "_")[[1]][1:2], collapse = "_")))) {
  # Subset the PNG files that start with the current species name
  species_files <- png_files_1[grep(paste0("^", species, "_"), png_files_1)]
  
  # Sort the species files by the number at the end of the file name
  species_files <- species_files[order(as.numeric(gsub("^.*_([0-9]+)\\.png$", "\\1", species_files)))]
  
  # Add each plot to a specific panel of the multi-panel plot
  for (file in species_files) {
    plot_img <- readPNG(file)
    plot(cex=.1,x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "",
         mar=c(0,0,0,0), ann=FALSE, axes = FALSE, xaxt = "n", yaxt = "n", bty = "n")
    rasterImage(plot_img, 0, 0, 1, 1)
    png(paste0(species, "multiplot.png"))
  }
}

dev.off()

