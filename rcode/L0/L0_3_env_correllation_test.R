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


# Set working directory to folder where environmental data is stored 
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/CHELSA")

# Making stack of all 19 bioclimatic variables
env <- list.files(pattern='tif', full.names=T)
env <- stack(env)

## Crop extent to a smaller region so it is easier to work with
# Crop environmental variables by the larger extent

env.clip <- crop(env, study_region_crop)

#check for variable correlations
cors=cor(values(env.clip),use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations

env=env[[c("bio2_1981.2010_V.2.1",
"bio4_1981.2010_V.2.1","bio5_1981.2010_V.2.1","bio6_1981.2010_V.2.1","bio13_1981.2010_V.2.1","bio14_1981.2010_V.2.1")]] # keep just reasonably uncorrelated ones
env.clip=env.clip[[c("bio2_1981.2010_V.2.1","bio4_1981.2010_V.2.1","bio5_1981.2010_V.2.1","bio6_1981.2010_V.2.1","bio13_1981.2010_V.2.1","bio14_1981.2010_V.2.1")]] # keep just reasonably uncorrelated ones
cors=cor(values(env.clip),use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6)# plot correlations

