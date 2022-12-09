

```{r}
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(wallace)
```

The *Wallace* session code .Rmd file is composed of a chain of module
functions that are internal to *Wallace*. Each of these functions
corresponds to a single module that the user ran during the session. To
see the internal code for these module functions, click on the links in
the .Rmd file. Users are encouraged to write custom code in the .Rmd
directly to modify their analysis, and even modify the module function
code to further customize. To see the source code for any module
function, just type its name into the R console and press Return.

```{r}
# example:
# just type the function name and press Return to see its source code
# paste this code into a new script to edit it
occs_queryDb
```

Your analyses are below.

------------------------------------------------------------------------
  
  ## Analysis for *Plecturocebus ornatus* (Po)
  
  ### Obtain Occurrence Data
  
  You searched the gbif database for *Plecturocebus ornatus*, limited to
500 records. You decided to remove occurrences without uncertainty
information? FALSE

```{r}
# Query selected database for occurrence records
queryDb_Po <- occs_queryDb(
  spNames = "Plecturocebus ornatus", 
  occDb = "gbif", 
  occNum = 500,
  RmUncertain = FALSE)
occs_Po <- queryDb_Po$Plecturocebus_ornatus$cleaned
```

### Obtain environmental data

Using user-specified variables.

```{r}
## Specify the directory with the environmental variables
dir_envs_Po <- "/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/cropped_chelsa_sd"
envs_path <- file.path(dir_envs_Po, c('bio1_1981.2010_V.2.1.tif', 'bio2_1981.2010_V.2.1.tif', 'bio3_1981.2010_V.2.1.tif', 'bio4_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio6_1981.2010_V.2.1.tif', 'bio7_1981.2010_V.2.1.tif', 'bio8_1981.2010_V.2.1.tif', 'bio9_1981.2010_V.2.1.tif', 'bio10_1981.2010_V.2.1.tif', 'bio11_1981.2010_V.2.1.tif', 'bio12_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio15_1981.2010_V.2.1.tif', 'bio16_1981.2010_V.2.1.tif', 'bio17_1981.2010_V.2.1.tif', 'bio18_1981.2010_V.2.1.tif', 'bio19_1981.2010_V.2.1.tif', 'sd_11km.tif'))
# Create environmental object 
envs_Po <- envs_userEnvs(
  rasPath = envs_path,
  rasName = c('bio1_1981.2010_V.2.1.tif', 'bio2_1981.2010_V.2.1.tif', 'bio3_1981.2010_V.2.1.tif', 'bio4_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio6_1981.2010_V.2.1.tif', 'bio7_1981.2010_V.2.1.tif', 'bio8_1981.2010_V.2.1.tif', 'bio9_1981.2010_V.2.1.tif', 'bio10_1981.2010_V.2.1.tif', 'bio11_1981.2010_V.2.1.tif', 'bio12_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio15_1981.2010_V.2.1.tif', 'bio16_1981.2010_V.2.1.tif', 'bio17_1981.2010_V.2.1.tif', 'bio18_1981.2010_V.2.1.tif', 'bio19_1981.2010_V.2.1.tif', 'sd_11km.tif'),
  doBrick = FALSE)
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
```

### Process Occurrence Data

Remove the occurrence localities by ID.

```{r}
# remove the rows that match the occIDs selected
occs_Po <- poccs_removeByID(
  occs = occs_Po , 
  removeID = c(159, 137))
```

### Process Occurrence Data

Thinning the occurrences to 10 km

```{r}
# Thin occurrences 
occs_Po <- poccs_thinOccs(
  occs = occs_Po, 
  thinDist = 10)
```

### Process environmental data

Sampling of 10000 background points and corresponding environmental data
using a “minimum convex polygon” method with a 1 degree buffer.

```{r}
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
```

### Partition occurrence data

Partition occurrences and background points for model training and
validation using jackknife, a non-spatial partition method.

```{r}
# R code to get partitioned data
groups_Po <- part_partitionOccs(
  occs = occs_Po ,
  bg =  bgSample_Po, 
  method = "jack",
  kfolds = 32) 
```

### Build and Evaluate Niche Model

Generating a species distribution model using the maxnet algorithm as
implemented in ENMeval V2.0 (with clamping = TRUE). For tuning using L,
LQ, H feature classes and regularization multipliers in the 1, 3.5 range
increasing by 0.5. Not using any categorical predictor variables.

```{r}
# Run maxent model for the selected species
#model_Po <- model_maxent(
#occs = occs_Po,
#bg = bgEnvsVals_Po,
# user.grp = groups_Po, 
#bgMsk = bgMask_Po,
#rms = c(1, 3.5), 
# rmsStep =  0.5,
# fcs = c('L', 'LQ', 'H'),
# clampSel = TRUE,
# algMaxent = "maxnet",
# parallel = TRUE,
# numCores = 3)

#Need to use Maxent.jar because of the ability to see perm importance
e.mx.3 <- ENMevaluate(occs = occs_xy_Po, envs = bgMask_Po, bg = bgSample_Po, 
                      algorithm = 'maxent.jar', partitions = 'jackknife',
                      tune.args = list(fc = c("L","LQ","H"), rm = 1:4))

H1.imp <-e.mx.3@variable.importance$fc.H_rm.1

#co-optimal model
LQ2.imp <-e.mx.3@variable.importance$fc.LQ_rm.2
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/model_runs/data_poor/chelsa_geodiv/11x11km_window_sd/plecturocebus_orantus/MCP_1dg_2locrm")
write.csv(H1.imp, "Po_permutation_imp.csv")
write.csv(e.mx.3@results, "Po_results.csv")
```

```{r}
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Chapter_1/model_runs/data_poor/chelsa_geodiv/11x11km_window_sd/plecturocebus_orantus/MCP_1dg_2locrm")
write.csv(H1.imp, "Po_permutation_imp.csv")
write.csv(e.mx.3@results, "Po_results.csv")
```
### Visualize

Generate a map of the maxnet generated model with with a “mtp” threshold
rule of 0.0701663778522781.

```{r}
# Select current model and obtain raster prediction
m_Po <- model_Po@models[["fc.LQ_rm.2"]] 
predSel_Po <- predictMaxnet(m_Po, bgMask_Po,
                            type = "cloglog", 
                            clamp = TRUE)
# extract the suitability values for all occurrences
occs_xy_Po <- occs_Po[c('longitude', 'latitude')]
# determine the threshold based on the current prediction
occPredVals_Po <- raster::extract(predSel_Po, occs_xy_Po)
# Define probability of quantile based on selected threshold
thresProb_Po <- switch("mtp", 
                       "mtp" = 0, "p10" = 0.1, "qtp" = 0)
# Define threshold value
thres_Po <- quantile(occPredVals_Po, probs = thresProb_Po)
# Applied selected threshold
predSel_Po <- predSel_Po > thres_Po

# Get values of prediction
mapPredVals_Po <- getRasterVals(predSel_Po, "cloglog")
# Define colors and legend  
rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
legendPal <- colorNumeric(rev(rasCols), mapPredVals_Po, na.color = 'transparent')
rasPal <- c('gray', 'blue')
# Generate map
m <- leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) 
m  %>%
  addLegend("bottomright", colors = c('gray', 'blue'),
            title = "Thresholded Suitability<br>(Training)",
            labels = c("predicted absence", "predicted presence"),
            opacity = 1, layerId = "train") %>% 
  #add occurrence data
  addCircleMarkers(data = occs_Po, lat = ~latitude, lng = ~longitude,
                   radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                   fillOpacity = 0.2, weight = 2, popup = ~pop) %>% 
  ##Add model prediction
  addRasterImage(predSel_Po, colors = rasPal, opacity = 0.7,
                 group = 'vis', layerId = 'mapPred', method = "ngb") %>%
  ##add background polygons
  addPolygons(data = bgExt_Po, fill = FALSE,
              weight = 4, color = "blue", group = 'proj')
```

## Project model

Projecting the model to a new user drawn area using a “mtp” threshold of
0.0701663778522781.

```{r}
# First must generate the projection area according to the drawn polygon in the GUI
proj_draw_Po <-proj_draw(
  polyPjXY = matrix(c(-76.037062, -71.641118, -71.48726, -76.267849, -76.037062, 5.574224, 5.61796, 0.001984, 0.210724, 5.574224),ncol=2,byrow=FALSE),
  polyPjID = 3366,
  drawPjBuf = 0.2)
# Create object of projection variables
projAreaEnvs_Po <- envs_Po
# Generate a projection of the model to the desired area
proj_area_Po <- proj_area(
  evalOut = model_Po,
  curModel = "fc.H_rm.1",
  envs = projAreaEnvs_Po , 
  outputType = "cloglog",
  alg = "maxnet",
  clamp = TRUE,
  pjExt = proj_draw_Po) 

#store the cropped projection variables
projExt_Po <- proj_area_Po$projExt

# extract the suitability values for all occurrences
occs_xy_Po <- occs_Po[c('longitude', 'latitude')]
# determine the threshold based on the current prediction
occPredVals_Po <- raster::extract(predSel_Po, occs_xy_Po)
# Define probability of quantile based on selected threshold
proj_thresProb_Po <- switch("mtp", 
                            "mtp" = 0, "p10" = 0.1, "qtp" = 0)
# Define threshold value
proj_thres_Po <- quantile(occPredVals_Po, 
                          probs = proj_thresProb_Po)
# Add threshold if specified 
proj_area_Po <- proj_area_Po$projArea > proj_thres_Po

##Make map
###Make map of projection
bb_Po <-  bgExt_Po@bbox
bbZoom <- polyZoom(bb_Po[1, 1], bb_Po[2, 1], bb_Po[1, 2], 
                   bb_Po[2, 2], fraction = 0.05)
mapProjVals_Po <- getRasterVals(proj_area_Po,"cloglog")

# if threshold specified
rasPal_Po <- c('gray', 'red')
m <- leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) 
m %>%
  fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4]) %>%
  addLegend("bottomright", colors = c('gray', 'red'),
            title = "Thresholded Suitability<br>(Projected)",
            labels = c("predicted absence", "predicted presence"),
            opacity = 1, layerId = 'proj')%>%
  
  # map model prediction raster and projection polygon
  clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
  addRasterImage(proj_area_Po, colors = rasPal_Po, opacity = 0.7,
                 layerId = 'projRas', group = 'proj', method = "ngb") %>%
  ##add projection polygon (user drawn area)
  addPolygons(data = proj_draw_Po, fill = FALSE,
              weight = 4, color = "blue", group = 'proj')
```
