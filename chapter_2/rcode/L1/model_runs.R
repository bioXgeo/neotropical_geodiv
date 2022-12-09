#Project: Using geodiversity to improve SDMs for data poor species

#Description: Code for test run of data rich species across multiple radii (3km, 15km, 31km). 

#Goal: to have this run for each species and then output a table of ENM eval Results for top performing models

#Authors: Beth E. Gerstner

#Date: 3/31/22

#Load in libraries 
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(wallace)

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)

# Point Values run (CHELSA + remote sensing)

## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned

### Obtain environmental data
#1km resolution status quo run

#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/CHELSA_4_only"
envs_path <- file.path(dir_envs_Ap, c('srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', "cloud_crop.tif"))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif',"cloud_crop.tif"),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)

#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]


##RUN 1
#Need to use Maxent.jar because of the ability to see perm importance
e.mx.1 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                      algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                      tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_palliata/evaluation")
write.csv(e.mx.1@results, file="a_palliata_ENMeval_1x_results.1.run1.csv")


e.mx.1.results <- e.mx.1@results
# minimize AICc
# evaluated the AICc within 2 delta units
#minAIC <- e.mx.results[which(e.mx.results$delta.AICc <= 2),] #Make sure the column is delta.AICc
#write.csv(minAIC,file="a_palliata_min_AIC_e.mx.csv")


#Generate table of performance stats
e.mx.stats.1 <- e.mx.1.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.1, "a_palliata_stats_e.mx.1_run1.csv")


# variable importance table 
e.mx.1.var.imp <-e.mx.1@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.1.var.imp, "a_palliata_permutation_imp_e.mx.1.run1.csv")

#write prediction to file
writeRaster(e.mx.1@predictions$fc.LQHP_rm.1, filename="e.mx.1.pred.run1.tif", format="GTiff", overwrite=T)


# ### plot points to investigate prediction
# 
# xy <- occs_Ap[,c(3,4)]
# 
# spdf <- SpatialPointsDataFrame(coords = xy, data = occs_Ap,
#                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))




# ## RUN 2 - do this prior to ESA and update all code
# 
# #Need to use Maxent.jar because of the ability to see perm importance
# e.mx.2 <- ENMevaluate(occs = occs_Ap_ll, envs = bgMask_Ap, bg = bgSample_Ap, 
#                       algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
#                       tune.args = list(fc = c("L","LQ","H","P"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 
# 
# #save all results
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_palliata/evaluation")
# write.csv(e.mx.2@results, file="a_palliata_ENMeval_1x_results.2.csv")
# 
# 
# e.mx.2.results <- e.mx.2@results
# # minimize AICc
# # evaluated the AICc within 2 delta units
# #minAIC <- e.mx.results[which(e.mx.results$delta.AICc <= 2),] #Make sure the column is delta.AICc
# #write.csv(minAIC,file="a_palliata_min_AIC_e.mx.csv")
# 
# 
# #Generate table of performance stats
# e.mx.stats.2 <- e.mx.2.results[c("AUC","BOYCE")]
# 
# 
# # variable importance table 
# e.mx.2.var.imp <-e.mx@variable.importance$fc.H_rm.1
# write.csv(e.mx.2.var.imp, "a_palliata_permutation_imp_e.mx.2.csv")
# 
# 
# ## RUN 3
# 
# #Need to use Maxent.jar because of the ability to see perm importance
# e.mx.3 <- ENMevaluate(occs = occs_Ap_ll, envs = bgMask_Ap, bg = bgSample_Ap, 
#                       algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
#                       tune.args = list(fc = c("L","LQ","H","P"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 
# 
# #save all results
# setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/1x/Alouatta_palliata/evaluation")
# write.csv(e.mx.3@results, file="a_palliata_ENMeval_1x_results.3.csv")
# 
# 
# e.mx.3.results <- e.mx.3@results
# # minimize AICc
# # evaluated the AICc within 2 delta units
# #minAIC <- e.mx.results[which(e.mx.results$delta.AICc <= 2),] #Make sure the column is delta.AICc
# #write.csv(minAIC,file="a_palliata_min_AIC_e.mx.csv")


# #Generate table of performance stats
# e.mx.stats.3 <- e.mx.3.results[c("AUC","BOYCE")]
# 
# 
# # variable importance table 
# e.mx.3.var.imp <-e.mx.3@variable.importance$fc.H_rm.1
# write.csv(e.mx.3.var.imp, "a_palliata_permutation_imp_e.mx.3.csv")


#STATS FOR 1x
# #Average of performance stats
# a.palliata.1x.stat <-rowbind(e.mx.stats.1,e.mx.stats.2,e.mx.stats.3)
# a.palliata.ps.avg <- colMeans(a.palliata.1x.stat)
# write.csv(a.palliata.ps.avg, "a_palliata_performance_stat_avg_1x")
# 
# #Average of permutation importance across runs
# a.palliata.perm.imp.1x <-cbind(e.mx.1.var.imp,e.mx.2.var.imp,e.mx.3.var.imp)
# a.palliata.ps.avg$avg <- rowMeans(a.palliata.perm.imp.1x)
# write.csv(a.palliata.perm.imp.avg, "a_palliata_perm_imp_avg_1x")


# ---------------
#3km run
## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned
#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/3x"
envs_path <- file.path(dir_envs_Ap, c('sd_3km.tif', 'srtm_crop.tif', 'bio6_sd_3km.tif', 'bio5_sd_3km.tif', 'bio14_sd_3km.tif', 'bio13_sd_3km.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif','cloud_crop.tif','cloud_3km.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('sd_3km.tif', 'srtm_crop.tif', 'bio6_sd_3km.tif', 'bio5_sd_3km.tif', 'bio14_sd_3km.tif', 'bio13_sd_3km.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif','cloud_crop.tif','cloud_3km.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)


#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.3 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                    algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                    tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/3x/Alouatta_palliata/evaluation")
write.csv(e.mx.3@results, file="a_palliata_ENMeval_3x_results.run1.csv")


### CHANGE TO 3x

e.mx.3.results <- e.mx.3@results
# minimize AICc
# evaluated the AICc within 2 delta units
#minAIC <- e.mx.results[which(e.mx.results$delta.AICc <= 2),] #Make sure the column is delta.AICc
#write.csv(minAIC,file="a_palliata_min_AIC_e.mx.csv")


#Generate table of performance stats
e.mx.stats.3 <- e.mx.3.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.3, "a_palliata_stats_e.mx.3_run1.csv")


# variable importance table 
e.mx.3.var.imp <-e.mx.3@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.3.var.imp, "a_palliata_permutation_imp_e.mx.3.run1.csv")

#write prediction to file
writeRaster(e.mx.3@predictions$fc.LQHP_rm.1, filename="e.mx.3.pred.run1.tif", format="GTiff", overwrite=T)

#_______________
#9km run

## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned
#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/9x"
envs_path <- file.path(dir_envs_Ap, c('sd_9km.tif', 'bio6_sd_9km.tif', 'bio5_sd_9km.tif', 'bio14_sd_9km.tif', 'bio13_sd_9km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_9km.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('sd_9km.tif', 'bio6_sd_9km.tif', 'bio5_sd_9km.tif', 'bio14_sd_9km.tif', 'bio13_sd_9km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_9km.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)

### Partition occurrence data

#Partition occurrences and background points for model training and validation using random k-fold, a non-spatial partition method.

# R code to get partitioned data
#groups_Ap <- part_partitionOccs(
#occs = occs_Ap ,
# bg =  bgSample_Ap, 
# method = "rand",
# kfolds = 4) 

#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.9 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                      algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                      tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/9x/Alouatta_palliata/evaluation")
write.csv(e.mx.9@results, file="a_palliata_ENMeval_9x_results_run1.csv")


e.mx.9.results <- e.mx.9@results


#Generate table of performance stats
e.mx.stats.9 <- e.mx.9.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.9, "a_palliata_stats_e.mx.9_run1.csv")


# variable importance table 
e.mx.9.var.imp <-e.mx.9@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.9.var.imp, "a_palliata_permutation_imp_e.mx.9_run1.csv")

#write prediction to file
writeRaster(e.mx.9@predictions$fc.LQHP_rm.1, filename="e.mx.9.pred.run1.tif", format="GTiff", overwrite=T)


#----------------
#15km run
## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned
#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/15x"
envs_path <- file.path(dir_envs_Ap, c('sd_15km.tif', 'bio6_sd_15km.tif', 'bio5_sd_15km.tif', 'bio14_sd_15km.tif', 'bio13_sd_15km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_15km.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('sd_15km.tif', 'bio6_sd_15km.tif', 'bio5_sd_15km.tif', 'bio14_sd_15km.tif', 'bio13_sd_15km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_15km.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)

### Partition occurrence data

#Partition occurrences and background points for model training and validation using random k-fold, a non-spatial partition method.

# R code to get partitioned data
#groups_Ap <- part_partitionOccs(
#occs = occs_Ap ,
# bg =  bgSample_Ap, 
# method = "rand",
# kfolds = 4) 

#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]


#Need to use Maxent.jar because of the ability to see perm importance
e.mx.15 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                      algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                      tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/15x/Alouatta_palliata/evaluation")
write.csv(e.mx.15@results, file="a_palliata_ENMeval_15x_results_run1.csv")


e.mx.15.results <- e.mx.15@results


#Generate table of performance stats
e.mx.stats.15 <- e.mx.15.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.15, "a_palliata_stats_e.mx.15_run1.csv")


# variable importance table 
e.mx.15.var.imp <-e.mx.15@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.15.var.imp, "a_palliata_permutation_imp_e.mx.15_run1.csv")

#write prediction to file
writeRaster(e.mx.15@predictions$fc.LQHP_rm.1, filename="e.mx.15.pred.run1.tif", format="GTiff", overwrite=T)

#_________
#21km run

## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned
#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/21x"
envs_path <- file.path(dir_envs_Ap, c('sd_21km.tif', 'bio6_sd_21km.tif', 'bio5_sd_21km.tif', 'bio14_sd_21km.tif', 'bio13_sd_21km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_21km.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('sd_21km.tif', 'bio6_sd_21km.tif', 'bio5_sd_21km.tif', 'bio14_sd_21km.tif', 'bio13_sd_21km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_21km.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)

### Partition occurrence data

#Partition occurrences and background points for model training and validation using random k-fold, a non-spatial partition method.

# R code to get partitioned data
#groups_Ap <- part_partitionOccs(
#occs = occs_Ap ,
# bg =  bgSample_Ap, 
# method = "rand",
# kfolds = 4) 

#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.21 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                       algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                       tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/21x/Alouatta_palliata/evaluation")
write.csv(e.mx.21@results, file="a_palliata_ENMeval_21x_results_run1.csv")


e.mx.21.results <- e.mx.21@results


#Generate table of performance stats
e.mx.stats.21 <- e.mx.21.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.21, "a_palliata_stats_e.mx.21_run1.csv")


# variable importance table 
e.mx.21.var.imp <-e.mx.21@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.21.var.imp, "a_palliata_permutation_imp_e.mx.21_run1.csv")

#write prediction to file
writeRaster(e.mx.21@predictions$fc.LQHP_rm.1, filename="e.mx.21.pred.run1.tif", format="GTiff", overwrite=T)


#----------------
#27km run

## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned
#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/27x"
envs_path <- file.path(dir_envs_Ap, c('sd_27km.tif', 'bio6_sd_27km.tif', 'bio5_sd_27km.tif', 'bio14_sd_27km.tif', 'bio13_sd_27km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_27km.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('sd_27km.tif', 'bio6_sd_27km.tif', 'bio5_sd_27km.tif', 'bio14_sd_27km.tif', 'bio13_sd_27km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_27km.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)

### Partition occurrence data

#Partition occurrences and background points for model training and validation using random k-fold, a non-spatial partition method.

# R code to get partitioned data
#groups_Ap <- part_partitionOccs(
#occs = occs_Ap ,
# bg =  bgSample_Ap, 
# method = "rand",
# kfolds = 4) 

#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.27 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                       algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                       tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/27x/Alouatta_palliata/evaluation")
write.csv(e.mx.27@results, file="a_palliata_ENMeval_27x_results_run1.csv")


e.mx.27.results <- e.mx.27@results


#Generate table of performance stats
e.mx.stats.27 <- e.mx.27.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.27, "a_palliata_stats_e.mx.27_run1.csv")


# variable importance table 
e.mx.27.var.imp <-e.mx.27@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.27.var.imp, "a_palliata_permutation_imp_e.mx.27_run1.csv")

#write prediction to file
writeRaster(e.mx.27@predictions$fc.LQHP_rm.1, filename="e.mx.27.pred.run1.tif", format="GTiff", overwrite=T)

#____________
#33km run

## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
# get a list of species occurrence data
userOccs_Ap <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Alouatta_palliata_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Ap <- userOccs_Ap$Alouatta_palliata$cleaned
#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Ap <- "/Volumes/BETH'S DRIV/zarnetske_lab/chelsa_sd/33x"
envs_path <- file.path(dir_envs_Ap, c('sd_33km.tif', 'bio6_sd_33km.tif', 'bio5_sd_33km.tif', 'bio14_sd_33km.tif', 'bio13_sd_33km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_33km.tif'))
# Create environmental object 
envs_Ap <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('sd_33km.tif', 'bio6_sd_33km.tif', 'bio5_sd_33km.tif', 'bio14_sd_33km.tif', 'bio13_sd_33km.tif','srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'cloud_crop.tif','cloud_33km.tif'),
  doBrick = FALSE)
occs_xy_Ap <- occs_Ap[c('longitude', 'latitude')]
occs_vals_Ap <- as.data.frame(raster::extract(envs_Ap, occs_xy_Ap, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Ap_2 <- occs_Ap[!(rowSums(is.na(occs_vals_Ap)) >= 1), ] 
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

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

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

#generate full prediction extent for input into 'envs'
crop_study_ap <- crop(envs_Ap, bgExt_Ap)

### Partition occurrence data

#Partition occurrences and background points for model training and validation using random k-fold, a non-spatial partition method.

# R code to get partitioned data
#groups_Ap <- part_partitionOccs(
#occs = occs_Ap ,
# bg =  bgSample_Ap, 
# method = "rand",
# kfolds = 4) 

#subset occs_ap to longitude and latitude
occs_Ap_ll <- occs_Ap[,c("longitude","latitude")]

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.33 <- ENMevaluate(occs = occs_Ap_ll, envs = crop_study_ap, bg = bgSample_Ap, 
                       algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                       tune.args = list(fc = c("LQHP"), rm = 1)) #will have to store maxent jar file on HPC? Maxent uses this file to run. 

#save all results
setwd("/Volumes/BETH'S DRIV/zarnetske_lab/model_runs/33x/Alouatta_palliata/evaluation")
write.csv(e.mx.33@results, file="a_palliata_ENMeval_33x_results_run1.csv")


e.mx.33.results <- e.mx.33@results


#Generate table of performance stats
e.mx.stats.33 <- e.mx.33.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.33, "a_palliata_stats_e.mx.33_run1.csv")


# variable importance table 
e.mx.33.var.imp <-e.mx.33@variable.importance$fc.LQHP_rm.1
write.csv(e.mx.33.var.imp, "a_palliata_permutation_imp_e.mx.33_run1.csv")

#write prediction to file
writeRaster(e.mx.33@predictions$fc.LQHP_rm.1, filename="e.mx.33.pred.run1.tif", format="GTiff", overwrite=T)




# Outputs for optimal models (next step)
#Code that takes permutation importance for each species/model set and puts that in table
#Code that tallys the times each variable (based on permutation importance) is used for each model set
#Code that tallys the times each variable is in top 3 for each species/model set




