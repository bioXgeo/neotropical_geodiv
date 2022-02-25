#Project: Using geodiversity to improve SDMs for data poor species

#Description: This is test code to run on HPC. Uses ENMeval for multiple species and saves results as an excel file.

#Goal: to have this run for each species and then output a table of ENM eval Results for top performing models

#Authors: Beth E. Gerstner

#Date: 2/25/22

#Load in libraries 
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(wallace)

#read in species list
all.species <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/test/test_species_list.csv")

# Query GBIF database for occurrence records
queryDb_Po <- occs_queryDb(
  spNames = "Plecturocebus ornatus", #species name will change
  occDb = "gbif", 
  occNum = 500,
  RmUncertain = FALSE)
occs_Po <- queryDb_Po$Plecturocebus_ornatus$cleaned

## Specify the directory with the environmental variables
dir_envs_Po <- "/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/cropped_chelsa_sd"
envs_path <- file.path(dir_envs_Po, c('bio1_1981.2010_V.2.1.tif', 'bio2_1981.2010_V.2.1.tif', 'bio3_1981.2010_V.2.1.tif', 'bio4_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio6_1981.2010_V.2.1.tif', 'bio7_1981.2010_V.2.1.tif', 'bio8_1981.2010_V.2.1.tif', 'bio9_1981.2010_V.2.1.tif', 'bio10_1981.2010_V.2.1.tif', 'bio11_1981.2010_V.2.1.tif', 'bio12_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio15_1981.2010_V.2.1.tif', 'bio16_1981.2010_V.2.1.tif', 'bio17_1981.2010_V.2.1.tif', 'bio18_1981.2010_V.2.1.tif', 'bio19_1981.2010_V.2.1.tif', 'sd_11km.tif'))

# Create environmental object. Change the name of objects to be generic and not species based.
envs_Po <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('bio1_1981.2010_V.2.1.tif', 'bio2_1981.2010_V.2.1.tif', 'bio3_1981.2010_V.2.1.tif', 'bio4_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio6_1981.2010_V.2.1.tif', 'bio7_1981.2010_V.2.1.tif', 'bio8_1981.2010_V.2.1.tif', 'bio9_1981.2010_V.2.1.tif', 'bio10_1981.2010_V.2.1.tif', 'bio11_1981.2010_V.2.1.tif', 'bio12_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio15_1981.2010_V.2.1.tif', 'bio16_1981.2010_V.2.1.tif', 'bio17_1981.2010_V.2.1.tif', 'bio18_1981.2010_V.2.1.tif', 'bio19_1981.2010_V.2.1.tif', 'sd_11km.tif'),
  doBrick = FALSE)

#subset full GBIF dataset
occs_xy_Po <- occs_Po[c('longitude', 'latitude')]
occs_vals_Po <- as.data.frame(raster::extract(envs_Po, occs_xy_Po, cellnumbers = TRUE))

# Remove duplicated same cell values
occs_Po <- occs_Po[!duplicated(occs_vals_Po[, 1]), ]
occs_vals_Po <- occs_vals_Po[!duplicated(occs_vals_Po[, 1]), -1]

# remove occurrence records with NA environmental values
occs_Po <- occs_Po[!(rowSums(is.na(occs_vals_Po)) > 1), ]

# also remove variable value rows with NA environmental values
occs_vals_Po <- na.omit(occs_vals_Po)

# add columns for env variable values for each occurrence record
occs_Po <- cbind(occs_Po, occs_vals_Po)

#Thinning the occurrences to 10 km
# Thin occurrences 
occs_Po <- poccs_thinOccs(
  occs = occs_Po, 
  thinDist = 10)

# Generate background extent 
bgExt_Po <- penvs_bgExtent(
  occs = occs_Po,
  bgSel = "minimum convex polygon",
  bgBuf = 1)

# Mask environmental data to provided extent
bgMask_Po <- penvs_bgMask(
  occs = occs_Po,
  envs = envs_Po,
  bgExt = bgExt_Po)

# Sample background points from the provided area
bgSample_Po <- penvs_bgSample(
  occs = occs_Po,
  bgMask =  bgMask_Po,
  bgPtsNum = 10000)

# Extract values of environmental layers for each background point
bgEnvsVals_Po <- as.data.frame(raster::extract(bgMask_Po,  bgSample_Po))
##Add extracted values to background points table
bgEnvsVals_Po <- cbind(scientific_name = paste0("bg_", "Plecturocebus ornatus"), bgSample_Po,
                       occID = NA, year = NA, institution_code = NA, country = NA,
                       state_province = NA, locality = NA, elevation = NA,
                       record_type = NA, bgEnvsVals_Po)

# R code to get partitioned data
groups_Po <- part_partitionOccs(
  occs = occs_Po ,
  bg =  bgSample_Po, 
  method = "jack",
  kfolds = nrow(occs)) #should be same as number of occurrence records after thinning, which will change for each species

#Need to use Maxent.jar because of the ability to see perm importance
e.mx <- ENMevaluate(occs = occs_xy_Po, envs = bgMask_Po, bg = bgSample_Po, 
                      algorithm = 'maxent.jar', partitions = 'jackknife',
                      tune.args = list(fc = c("L","LQ","H"), rm = 1:4)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 


# Outputs
#Code that takes top  performing models based on AUC/Omission and AIC and puts stats in table
#Code that takes permutation importance for each species/model set and puts that in table
#Code that tallys the times each variable (based on permutation importance) is used for each model set
#Code that tallys the times each variable is in top 3 for each species/model set




