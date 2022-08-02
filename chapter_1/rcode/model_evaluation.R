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

# Point Values run (CHELSA + elev)

## Query selected database for occurrence records
queryDb_Ap <- occs_queryDb(
  spNames = "Alouatta palliata", 
  occDb = "gbif", 
  occNum = 10000,
  RmUncertain = FALSE)
occs_Ap <- queryDb_Ap$Alouatta_palliata$cleaned

### Obtain environmental data

#Using user-specified variables.

# Create environmental object 
## Specify the directory with the environmental variables
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/CHELSA_4_only"
envs_path <- file.path(dir_envs_Ap, c('srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# Remove duplicated same cell values
occs_Ap <- occs_Ap[!duplicated(occs_vals_Ap[, 1]), ]
occs_vals_Ap <- occs_vals_Ap[!duplicated(occs_vals_Ap[, 1]), -1]
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) > 1), ] 
# also remove variable value rows with NA environmental values
occs_vals_Ap_2 <- na.omit(occs_vals_Ap)
# add columns for env variable values for each occurrence record
occs_Ap <- cbind(occs_Ap_2, occs_vals_Ap_2) 


### Process Occurrence Data

#Remove occurrences outside of user drawn polygon
occs_Ap <- poccs_selectOccs(
  occs = occs_Ap,
  polySelXY = matrix(c(-86.444047, -80.905157, -73.915607, -74.39916, -78.839064, -83.85044, -88.334303, -87.76283, -86.444047, 14.76522, 14.254698, 11.254812, 4.117319, -6.150488, -4.487817, 8.962027, 13.657628, 14.76522), ncol = 2, byrow = FALSE),
  polySelID = 5373)


### Process environmental data

Sampling of 10000 background points and corresponding environmental data
using a “point buffers” method with a 1 degree buffer.

```{r}
# Generate background extent 
bgExt_Ap <- penvs_bgExtent(
  occs = occs_Ap,
  bgSel = "point buffers",
  bgBuf = 1)
# Mask environmental data to provided extent
bgMask_Ap <- penvs_bgMask(
  occs = occs_Ap,
  envs = envs_Ap,
  bgExt = bgExt_Ap)
# Sample background points from the provided area
bgSample_Ap <- penvs_bgSample(
  occs = occs_Ap,
  bgMask =  bgMask_Ap,
  bgPtsNum = 10000)
# Extract values of environmental layers for each background point
bgEnvsVals_Ap <- as.data.frame(raster::extract(bgMask_Ap,  bgSample_Ap))
##Add extracted values to background points table
bgEnvsVals_Ap <- cbind(scientific_name = paste0("bg_", "Alouatta palliata"), bgSample_Ap,
                       occID = NA, year = NA, institution_code = NA, country = NA,
                       state_province = NA, locality = NA, elevation = NA,
                       record_type = NA, bgEnvsVals_Ap)


### Partition occurrence data

#Partition occurrences and background points for model training and validation using random k-fold, a non-spatial partition method.

# R code to get partitioned data
groups_Ap <- part_partitionOccs(
  occs = occs_Ap ,
  bg =  bgSample_Ap, 
  method = "rand",
  kfolds = 4) 

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.3 <- ENMevaluate(occs = occs_xy_Po, envs = bgMask_Po, bg = bgSample_Ap, 
                      algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                      tune.args = list(fc = c("L","LQ","H"), rm = 1:4)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/chelse_4_only/alouatta_palliata")
write.csv(e.mx, file="a_palliata_ENMeval_results.csv")


# Extract models with lowest omission rate
#ORs <- e.mx[which(e.mx$Mean.OR10 == min(e.mx$Mean.OR10)),]
write.csv(ORs,file="ENMeval_OR_10.csv")

# Of those with low omission, find those with the highest AUC
#maxAUC.x10 <- ORs[which(ORs$Mean.AUC == max(ORs$Mean.AUC)),]
#write.csv(maxAUC.x10,file="ENMeval_OR_10_max_AUC.csv")

# minimize AICc
# evaluated the AICc within 2 delta units
minAIC <- e.mx[which(e.mx$delta.AICc <= 2),] #Make sure the column is delta.AICc
write.csv(minAIC,file="ENMeval_min_AIC.csv")

# variable importance table 
SETTING.imp <-e.mx@variable.importance$fc.H_rm.1
setwd("//Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/chelse_4_only/alouatta_palliata")
write.csv(H1.imp, "Po_permutation_imp.csv")

# ---------------
#3km run




#----------------
#15km run




#----------------
#31km run












# Outputs for optimal models (next step)
#Code that takes permutation importance for each species/model set and puts that in table
#Code that tallys the times each variable (based on permutation importance) is used for each model set
#Code that tallys the times each variable is in top 3 for each species/model set




