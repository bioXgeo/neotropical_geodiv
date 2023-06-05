#Olinguito test

library(wallace)
library(raster)
library(rgeos)
library(rgdal)
#run base model and do selection to compare with final model we made using geodiv. Can compare with original expert model.

## Query selected database for occurrence records. These have been pre-thinned by 10km.
occs_path <- "C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/occurrence_records/Lagothrix_lagotricha_thinned_full"
occs_path <- file.path(occs_path, "Lagothrix_lagotricha_thinned_wallace.csv")
# get a list of species occurrence data
userOccs_Bn <- occs_userOccs(
  txtPath = occs_path, 
  txtName = "Lagothrix_lagotricha_thinned_thin1.csv", 
  txtSep = ",", 
  txtDec = ".")
occs_Bn <- userOccs_Bn$Lagothrix_lagotricha$cleaned

### Obtain environmental data
#1km resolution status quo run

#Using user-specified variables.
# Create environmental object 
## Specify the directory with the environmental variables. 
dir_envs_Bn <- "D:/zarnetske_lab/CHELSA_GEO_only"
envs_path <- file.path(dir_envs_Bn, c('srtm_crop.tif', 'srtm_sq_33.tif', 'bio6_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'bio13_sq_33.tif', 'cloud_sq_33.tif'))
# Create environmental object 
envs_Bn <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('srtm_crop.tif', 'srtm_sq_33.tif', 'bio6_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'bio13_sq_33.tif', 'cloud_sq_33.tif'),
  doBrick = FALSE)
occs_xy_Bn <- occs_Bn[c('longitude', 'latitude')]
occs_vals_Bn <- as.data.frame(raster::extract(envs_Bn, occs_xy_Bn, cellnumbers = TRUE))
# remove occurrence records with NA environmental values
occs_Bn_2 <- occs_Bn[!(rowSums(is.na(occs_vals_Bn)) >= 1), ] 
# also remove variable value rows with NA environmental values
occs_vals_Bn_2 <- na.omit(occs_vals_Bn)
# add columns for env variable values for each occurrence record
occs_Bn <- cbind(occs_Bn_2, occs_vals_Bn_2) 



### Process environmental data

#Sampling of 10000 background points and corresponding environmental data
#using a “point buffers” method with a 1 degree buffer.

# Generate background extent 
bgExt_Bn <- penvs_bgExtent(
  occs = occs_Bn,
  bgSel = "point buffers",
  bgBuf = 1)
# Mask environmental data to provided extent
bgMask_Bn <- penvs_bgMask(
  occs = occs_Bn,
  envs = envs_Bn,
  bgExt = bgExt_Bn)
# Sample background points from the provided area
bgSample_Bn <- penvs_bgSample(
  occs = occs_Bn,
  bgMask =  bgMask_Bn,
  bgPtsNum = 10000)
# Extract values of environmental layers for each background point
bgEnvsVals_Bn <- as.data.frame(raster::extract(bgMask_Bn,  bgSample_Bn))
##Add extracted values to background points table
bgEnvsVals_Bn <- cbind(scientific_name = paste0("bg_", "Lagothrix lagotricha"), bgSample_Bn,
                       occID = NA, year = NA, institution_code = NA, country = NA,
                       state_province = NA, locality = NA, elevation = NA,
                       record_type = NA, bgEnvsVals_Bn)

#generate full prediction extent for input into 'envs'
crop_study_Bn <- crop(envs_Bn, bgExt_Bn)

#subset occs_Bn to longitude and latitude
occs_Bn_ll <- occs_Bn[,c("longitude","latitude")]


##RUN 1
#Need to use Maxent.jar because of the ability to see perm importance
e.mx.1 <- ENMevaluate(occs = occs_Bn_ll, envs = crop_study_Bn, bg = bgSample_Bn, 
                      algorithm = 'maxent.jar', partitions = 'randomkfold', #change this to cross validation
                      tune.args = list(fc = c("LQ"), rm = c(1))) #will have to store maxent jar file on HPC? Maxent uses this file to run. 


#save all results
setwd("D:/zarnetske_lab/run2_results/Lagothrix_lagotricha")
write.csv(e.mx.1@results, file="l_lagotricha_ENMeval_1x_results.run2.csv")


e.mx.1.results <- e.mx.1@results
# minimize AICc
# evaluated the AICc within 2 delta units
minAIC.1 <- e.mx.1.results[which(e.mx.1.results$delta.AICc ==0),] #Make sure the column is delta.AICc
#write.csv(minAIC,file="b_neblina_min_AIC_e.mx.csv")


#Generate table of performance stats
e.mx.stats.1 <- e.mx.1.results[c("auc.train","cbi.train")]
write.csv(e.mx.stats.1, "l_lagotricha_stats_e.mx.1_run1.csv")


# variable importance table 
e.mx.1.var.imp <-e.mx.1@variable.importance$fc.LQ_rm.1
write.csv(e.mx.1.var.imp, "l_lagotricha_permutation_imp_e.mx.1.run1.csv")

#write prediction to file
writeRaster(e.mx.1@predictions$fc.LQ_rm.1, filename="e.mx.1.3km.tif", format="GTiff", overwrite=T)

plot(e.mx.1@predictions$fc.LQ_rm.1)



#Function to threshold the model
sdm_threshold <- function(sdm, occs, type = "mtp", binary = FALSE){
  occPredVals <- raster::extract(sdm, occs)
  if(type == "mtp"){
    thresh <- min(na.omit(occPredVals))
  } else if(type == "p10"){
    if(length(occPredVals) < 10){
      p10 <- floor(length(occPredVals) * 0.9)
    } else {
      p10 <- ceiling(length(occPredVals) * 0.9)
    }
    thresh <- rev(sort(occPredVals))[p10]
  }
  sdm_thresh <- sdm
  sdm_thresh[sdm_thresh < thresh] <- NA
  if(binary){
    sdm_thresh[sdm_thresh >= thresh] <- 1
  }
  return(sdm_thresh)
}

# 1st run for species
species_prediction_1_p_10 <- sdm_threshold(e.mx.1@predictions$fc.LQ_rm.1, occs_xy_Bn[,1:2], type="mtp")

writeRaster(species_prediction_1_p_10,filename="Lagothrix_lagotricha_top_6_3km.tif",format="GTiff", overwrite=T)



# ### plot points to investigate prediction
# 
xy <- occs_xy_Bn[,c(1,2)]
# 
spdf <- SpatialPointsDataFrame(coords = xy, data = occs_Bn,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
points(spdf)

# We can also plot the binned background points with the occurrence points on top to 
# visualize where the training data is located.
#points(eval.bg(e.mx.1), pch = 3, col = eval.bg.grp(e.mx.1), cex = 0.5)
points(eval.occs(e.mx.1), pch = 21, bg = eval.occs.grp(e.mx.1))

crop_extent <- readOGR("D:/zarnetske_lab/run2_results/Lagothrix_lagotricha/l_lagotricha_clip.shp")
rivers <- readOGR("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/expert_maps/river_lines_colombia/River_lines.shp")
# Mask the raster using the crop shape
library(raster)

# Create a new raster with the same extent and resolution as the original, but with values set to 0

# Mask the original raster using the mask raster
masked_raster_cont <- mask(species_prediction_1_p_10, crop_extent)
masked_raster_cont[masked_raster_cont>0] <-1
masked_raster_bin <-masked_raster_cont

lagotricha_33km <- mask(species_prediction_1_p_10, lagotricha_clip, inverse = TRUE)


#expert
expert_extent <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/expert_maps/Lagothrix_lagotricha_expert.tif")

#need the no geodiv model as well and can compare all 3 in a side by side map, add rivers


plot(expert_extent)
new_extent_masked_cont <- crop(masked_raster_cont, expert_extent)
new_extent_masked_bin <- crop(masked_raster_bin, expert_extent)

#no geodiversity raster
no_geo <- raster("D:/zarnetske_lab/run2_results/Lagothrix_lagotricha/Lagothrix_lagotricha_sdm_1_mean.tif")

no_geo_crop <- mask(no_geo, crop_extent)
no_geo_crop[no_geo_crop>0] <- 1

no_geo_crop <- crop(no_geo_crop, masked_raster_bin)

differences <-new_extent_masked_cont - no_geo

# Write the masked raster to a new file
writeRaster(masked_raster, "path/to/masked_raster.tif", overwrite=TRUE)
# Write the masked raster to a new file
writeRaster(masked_raster, "path/to/masked_raster.tif", overwrite=TRUE)

# Write the masked raster to a new file
writeRaster(masked_raster, "path/to/masked_raster.tif", overwrite=TRUE)

mask_final_model <- species_prediction_1_p_10[!species_prediction_1_p_10==crop_extent]
cropped_final_model <- crop(species_prediction_1_p_10, crop_extent)

test <- crop(cropped_final_model, crop_extent)
final_model_bn_e <-crop(model_1x_bn, expert_model)
final_base_model_bn_e <- crop(cropped_base_model, expert_model)
final_base_model_bn_e




writeRaster(test,filename="final_model_bn.tif",format="GTiff", overwrite=T)


#resample to match extents/res
library(terra)
r1 = rast("D:/zarnetske_lab/run1_results/final_models/bn_final_pred.tif")
r2 = rast("D:/zarnetske_lab/run1_results/final_models/e.mx.1.base.optimal.tif")
r3 = rast("D:/Projection_LQ1/Final_Clipped_Model/LQ1_clipped_new1.tif")
r1 <- resample(r1, r2)
r3 <- resample(r3, r2)


#PULL IN EXPERT PUBLISHED MODEL
  expert_model <- raster("D:/Projection_LQ1/Final_Clipped_Model/LQ1_clipped_new1.tif")
  expert_model <- r3 >=.329
  r3[r3 < .329] <- NA
  expert_model <- r3
  
  #pull in final optimal model
  
  r1[r1 < 0.399] <- NA
  final_optimal <- r1


  #pull in base model
  base_optimal <-raster("D:/zarnetske_lab/run1_results/final_models/e.mx.1.base.optimal.tif")
  r2[r2 < 0.355] <- NA
  base_optimal <- r2
  
  library(dplyr)
  expert_model <-expert_model %>% raster()
  final_optimal <- final_optimal %>% raster()
  base_optimal <- base_optimal %>% raster()
  
  base_optimal <- mask(base_optimal,crop_extent)
  final_optimal <- mask(final_optimal,crop_extent)
  
  library(dismo)
  #overlap
  overlap_final_model <-nicheOverlap(final_optimal,expert_model) #94% overlap between final optimal and non geodiv base model
  
  overlap_base_model<-nicheOverlap(base_optimal,expert_model) #93% overlap between final optimal and non geodiv base model
  
  overlap_final_base_model<-nicheOverlap(base_optimal,final_optimal) #91% overlap between final optimal and non geodiv base model
  
  
  #Maps of olinguito distribution
  # Plot raster and shape of Colombia
  my.palette <- brewer.pal(n = 20, name = "Purples")
 plot(colombia_shp, border = "gray")
  plot(differences, col = my.palette, main = "B", add = TRUE)
  
  # Add north arrow
  prettyMap(scale = 1, force_aspect = 1)
  addnortharrow(pos = "bottomright", padin = c(.7, .7), scale = .5, lwd = 1,
                border = "black", cols = c("white", "black"), text.col = "black")

  addscalebar(
    plotunit = NULL,
    plotepsg = NULL,
    widthhint = 0.25,
    unitcategory = "metric",
    htin = 0.1,
    padin = c(0.15, 0.15),
    style = "bar",
    bar.cols = c("black", "white"),
    lwd = 1,
    linecol = "black",
    tick.cex = 0.7,
    labelpadin = 0.08,
    label.cex = 0.8,
    label.col = "black",
    pos = "bottomright"
  )
  points(spdf, pch=20, cex=.4, col="red")
  
#final optimal
  
  library(prettymapr)
  library(RColorBrewer)
  my.palette <- brewer.pal(n = 20, name = "Purples")
  plot(final_optimal, col = my.palette, main="C")
  plot(study_region[1], col='light gray', border='gray',add=T)
  plot(final_optimal, col = my.palette, main="C", add=T)
  addnortharrow(
    pos = "bottomright",
    padin = c(.7, .7),
    scale = .5,
    lwd = 1,
    border = "black",
    cols = c("white", "black"),
    text.col = "black"
  )
  
  
  addscalebar(
    plotunit = NULL,
    plotepsg = NULL,
    widthhint = 0.25,
    unitcategory = "metric",
    htin = 0.1,
    padin = c(0.15, 0.15),
    style = "bar",
    bar.cols = c("black", "white"),
    lwd = 1,
    linecol = "black",
    tick.cex = 0.7,
    labelpadin = 0.08,
    label.cex = 0.8,
    label.col = "black",
    pos = "bottomright"
  )
  points(spdf, pch=20, cex=.4, col="red")
  
  #Expert optimal
  
  expert_model <- mask(expert_model, crop_extent)
  
  library(prettymapr)
  library(RColorBrewer)
  my.palette <- brewer.pal(n = 20, name = "Purples")
  plot(expert_extent, col = my.palette, main="A")
  plot(study_region[1], col='light gray', border='gray',add=T)
  plot(expert_model, col = my.palette, main="A", add=T)
  addnortharrow(
    pos = "bottomright",
    padin = c(.7, .7),
    scale = .5,
    lwd = 1,
    border = "black",
    cols = c("white", "black"),
    text.col = "black"
  )
  
  
  addscalebar(
    plotunit = NULL,
    plotepsg = NULL,
    widthhint = 0.25,
    unitcategory = "metric",
    htin = 0.1,
    padin = c(0.15, 0.15),
    style = "bar",
    bar.cols = c("black", "white"),
    lwd = 1,
    linecol = "black",
    tick.cex = 0.7,
    labelpadin = 0.08,
    label.cex = 0.8,
    label.col = "black",
    pos = "bottomright"
  )
  points(spdf, pch=20, cex=.4, col="red")
  
  

