#Variable correlations

library(raster)
library(rnaturalearth)
library(dplyr)
library(rgeos)
library(maps)
library(sf)
library(corrplot)


# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map to region of interest for future cropping. 

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(SA_study_region, xmin = -83, xmax = -65, ymin = -7, ymax = 13)


setwd("D:/Zarnetske_Lab/CHELSA_GEO_only/33km_test")

# Load the raster package for reading and manipulating raster data
library(raster)

# Create an empty stack to hold the raster objects
raster_stack <- stack()

# Loop through each tif file in the folder, convert it to a raster object, and add it to the stack
for (filename in list.files(pattern = "\\.tif$")) {
  raster_object <- raster(filename)
  raster_stack <- stack(raster_stack, raster_object)
}

#check for variable correlations
cors=cor(values(raster_stack), use=c("complete.obs")) # evaluate correlations

corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations

usdm::vif(raster_stack)

#elevation correlated, but since we are not aiming to project beyond occurrence records and we are looking to compare geodiversity of the same variable, we keep this predictor. Elith, Phillips, et al. (2010) point out that Maxent’s built-in variable selection (via L1-regularization) is reliable, relatively insensitive to correlation among variables, and model performance may actually be degraded by imposing additional model selection procedures prior to running Maxent! They do suggest that sticking to proximal variables is preferable when projecting models to novel contexts.

  
  #Merow et al. (2013) identify two general approaches to selecting variables. The machine learning approach is based on the understanding that the Maxent algorithm will, by design, select the most useful variables and features, so we can include all reasonable variables.

#However, this probably only applies when the objective is to provide accurate predictions of occurrences in the same context in which the model is built. Efforts to understand the environmental constraints on that distribution, or projecting it to a new context, will be potentially confounded when the model includes correlated variables.

#env=env[[c("bio2_1981.2010_V.2.1",
"bio4_1981.2010_V.2.1","bio5_1981.2010_V.2.1","bio6_1981.2010_V.2.1","bio13_1981.2010_V.2.1","bio14_1981.2010_V.2.1")]] # keep just reasonably uncorrelated ones
#env.clip=env.clip[[c("bio2_1981.2010_V.2.1","bio4_1981.2010_V.2.1","bio5_1981.2010_V.2.1","bio6_1981.2010_V.2.1","bio13_1981.2010_V.2.1","bio14_1981.2010_V.2.1")]] # keep just reasonably uncorrelated ones
#cors=cor(values(env.clip),use='complete.obs') # evaluate correlations
#corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6)# plot correlations

