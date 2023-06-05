#SDM thresholding
library(raster)

test <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/Plecturocebus_ornatus_sdm_9_mean.tif")

occs <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/occurrence_records/Plecturocebus_ornatus_thinned_full/Plecturocebus_ornatus_thinned_thin1.csv")
#Function to threshold the model
sdm_threshold <- function(sdm, occs, type = "mtp", binary = FALSE){
  occPredVals <- raster::extract(test, occs)
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
species_prediction_1_p_10 <- sdm_threshold(test, occs[,2:3], type="p10")
coords <- cbind(occs$decimalLongitude, occs$decimalLatitude)
pts.sp <- SpatialPointsDataFrame(coords, occs)
points(pts.sp)

# 2nd run for species
species_prediction_2_p_10 <- sdm_threshold(e.mx@predictions$fc.LQ_rm.1, occs[,2:3], type="p10")

# 3rd run for species
species_prediction_3_p_10 <- sdm_threshold(e.mx@predictions$fc.LQ_rm.1, occs[,2:3], type="p10")

#ensemble model of each run 
species_prediction_ensemble_p_10 <- (species_prediction_1_p_10+species_prediction_2_p_10+species_prediction_3_p_10)/3

#ruleset:
# greater than 25 records type="p10"
# less than 25 records type="mtp"

