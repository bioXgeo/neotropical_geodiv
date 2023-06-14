#Title: SDM Model Run L1_1_sdm_model.R

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code calculates geodiversity metrics. It calculates SD of CHELSA biodiversity variables at multiple radii (3, 9, 15, 21, 27, 33 km). It calculcates the SD of elevation over the same radii.

#Data input: 

#Data output: Binary and continuous SDM predictions for each species for different radii and for multiple runs, and an average of those.

#Authors: Beth E. Gerstner & Patrick Bills

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske

# versions
# 1/30/23: major refactor for running flexibility, so can be run concurrently in a super computer environment (e.g. slurm)
# 3/21/22: original script https://github.com/bgerstner90/geodiv/blob/master/chapter_1/rcode/test_model_evaluation.R
# 1/21/19: nearly complete WIP.  adapted to use correct data folder, but wallace can't read occs

## Requirement: Environment variables
# to make this code more flexible, it uses the following environment variables - set these in your .Renviron file 
# or in a shell script to call this program. 
# SDM_BASE_PATH path to where the data lives, with subfolders for species occs and rasters
# alternatively if SDM_BASE_PATH is not set, HPCC_BASE_PATH will be used
# SDM_ENVS_PATH sub path inside SDM_BASE_PATH where the env rasters are
# SDM_OCCS_PATH sub dir od base path
# sprintf patterns to name these files, since the code dependend on file names to be very specifically 
# SDM_OCCS_PATH_TEMPLATE='occurrence_records/%s_thinned_full'
# SDM_OCCS_FILE_TEMPLATE='%s_thinned_wallace.csv'

# ensure libs are available 
library(terra)
library(ENMeval) 
library(wallace)
library(rJava)
library(dismo)
library(dplyr)

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

# global configuration settings
RADII <- c(1,3,9,15,21,27,33)
TEST_SPECIES="Alouatta_palliata"
TEST_RADIUS=1

# get or set the global configuration for the base path so you don't have to send it to functions everytime
# side effect: sets env variable if a parameter is sent
sdmBasePath <- function(base_path = NULL){
  # 1. use argument if it's present, set os env
  if( ! (is.null(base_path) || base_path == "") ) {
    # trick to get this set in the OS env so we can use it as a 'global' var
    # update the global OS environment for the value sent to the function
    do.call(Sys.setenv, as.list(setNames(base_path, "BASE_PATH")))
    
    return(base_path )
  } else {
    # 2. no var sent, so read environment
    base_path <- Sys.getenv('SDM_BASE_PATH')
  }
  
  # 3 if this base path is not set, check if we are on the HPCC
  # this allows you to have a single .Renviron with a 'local' var and an HPCC path
  if(base_path == "") {
    if(Sys.getenv("HPCC_CLUSTER_FLAVOR") != "") {
      base_path = Sys.getenv('HPCC_BASE_PATH')
    } else {
      # if parameter null, no environment variable set, use current dir
      base_path = "."
    }
    
    # again, set this in the env as global variable since it's not set
    do.call(Sys.setenv, as.list(setNames(base_path, "BASE_PATH")))
  }
  return(base_path)
}


#' standardized way to construct path to data given radius
#' this will ALWAYS be <base_path>/SDM_ENVS_PATH/<radius>x>
#' 
envsDataPath <- function(radius = 1, suffix = "x", envsDir = NULL, basePath = sdmBasePath()){
  # current location 1/23 is  base path + /environmental_variables/33x/<tiffs>
  if(is.null(envsDir)) {
    # if no param sent, use value set in .Renviron or in OS environment
    envsDir = Sys.getenv('SDM_ENVS_PATH')
  } 
  
  
  dp <- file.path(basePath, envsDir, paste0(sprintf("%d", radius), suffix))
  return(dp)
}


#' read in ennvironment rasters for a given radius
#' envsDir path to rasters, or NULL if want to use standard path
#' to construct data path different from standard, 
#'  use envsDir =  envsDataPath(radius, suffix, envsDir, bathPath)

read_envs <- function(radius, envsDir = NULL){
  
  envsDir = envsDataPath(radius, envsDir = envsDir)
  
  
  if(!file.exists(envsDir)){
    warning(paste("path to envs not found, returning null:", envsDir))
    return(NULL)
  }
  
  
  # NOTE for reference, the current list is c('srtm_crop.tif', 'bio6_1981.2010_V.2.1.tif', 'bio5_1981.2010_V.2.1.tif', 'bio14_1981.2010_V.2.1.tif', 'bio13_1981.2010_V.2.1.tif',"cloud_crop.tif")
  # but here we get the list of file nanmes by reading the names from given folder, which makes it more flexible
  patternTifFile = ".+\\.tif[f]?"  # regular expression matches *.tif and *.tiff
  # get list of all the tiffs in the folder
  envs_raster_names <-  list.files(envsDir, patternTifFile )
  
  # make sure there are files there
  if(length(envs_raster_names) == 0){
    warning(paste("no tif files found in", envsDir, "returning NULL"))
    return(NULL)
  }
  
  envs <- wallace::envs_userEnvs(
    rasPath = file.path(envsDir, envs_raster_names),
    rasName = envs_raster_names,
    doBrick = FALSE)
  
  return(envs)
}


occsDataPath <- function(species, occsPathTemplate = NULL, basePath = sdmBasePath() ) {
  
  # current location of data (1/23) is 
  # basePath + occurrence_records/Alouatta_palliata_thinned_full
  # basePath + occs dir + species dir
  
  if( is.null(occsPathTemplate)) {
    occsPathTemplate = Sys.getenv('SDM_OCCS_PATH_TEMPLATE')
  }
  
  if(stringr::str_detect(occsPathTemplate,'%')) {
    tryCatch(
      {occsFullPath<- file.path(basePath,sprintf(occsPathTemplate,species ))
      },
      error = function(e) {
        warning("invalid template param in occsPathTemplate , see docs for sprintf")
        return(NULL)
      }
      
    )
  } else {
    occsFullPath<- file.path(sdmBasePath,occsPathTemplate)
  }
  
  if(file.exists(occsFullPath)) {
    return(occsFullPath)
  } else { 
    warning(paste("occurence data folder not found, returning NULL ",occsFullPath))
    return(NULL)            
  }
  
  
  
}


#' construct file name for occurrences file given template 
#' 
#' wallace needs both the full path and the file name to open 
occsDataFilename<-function(species,occsFileNameTemplate  = NULL){
  
  if(is.null(occsFileNameTemplate )) 
  {  occsFileNameTemplate  = Sys.getenv('SDM_OCCS_FILE_TEMPLATE')}
  
  if(stringr::str_detect(occsFileNameTemplate,'%')) {
    occsFilePath <- sprintf(occsFileNameTemplate, species)
  } else {
    # not an sprintf() template so ... ?   just return the path assuming it's the full. path to the file 
    occsFilePath <- occsFileNameTemplate
  }
  
  return(occsFilePath )
  
}



read_occs <-function(species, occsPathTemplate=NULL, occsFileNameTemplate=NULL, basePath = NULL){
  #questionL for txt name why use the file name (a..p..thined ) vs just the species per Wallace documentation https://rdrr.io/cran/wallace/man/occs_userOccs.html
  #occs_path <- "/Volumes/BETH'S DRIV/zarnetske_lab/candidate_species_2022/thinned_data/Alouatta_palliata_thinned_full"
  #occs_path <- file.path(occs_path, "Alouatta_palliata_thinned_thin1.csv")
  
  # TODO - leave this to the other functions? 
  if(is.null(basePath)) { basePath = sdmBasePath() }
  
  
  wallaceFriendlyOccsFile =  sprintf("%s_thinned_wallace.csv", species) # occsDataFilename(species,occsFileNameTemplate)
  wallaceFriendlyOccsPath = file.path(occsDataPath(species, occsPathTemplate, basePath = basePath),
                                      wallaceFriendlyOccsFile)
  if(!(file.exists(wallaceFriendlyOccsPath))){
    # can't find the wallace file, so look for the original file 
    # paste0(species,"_thinned_full/",species,"_thinned_thin1.csv")
    originOccsFileName = sprintf("%s_thinned_thin1.csv", species)
    
    origOccsPath = file.path(occsDataPath(species, occsPathTemplate, basePath = basePath),
                             originOccsFileName)
    # if we can't find the original occs, we have the file path/names wrong and need to stop
    stopifnot(file.exists(origOccsPath))
    
    # read, change and save data with new file name
    occs <- read.csv(origOccsPath)
    names(occs) <- c("scientific_name","longitude","latitude")
    write.csv(occs,wallaceFriendlyOccsPath)
    
    
  }
  # https://rdrr.io/cran/wallace/man/occs_userOccs.html
  userOccs_sp <- wallace::occs_userOccs(
    
    txtPath = wallaceFriendlyOccsPath,
    txtName = wallaceFriendlyOccsFile,
    txtSep = ",",
    txtDec = ".")
  
  return(userOccs_sp[[species]]$cleaned)
}


#' read in pre-queries (and pre-thinned) occurrence records
process_occs <- function(occs, envs) {
  
  occs_xy <- occs[c('longitude', 'latitude')]
  occs_vals <- as.data.frame(raster::extract(envs, occs_xy, cellnumbers = TRUE))
  # remove occurrence records with NA environmental values
  occs_2<- occs[!(rowSums(is.na(occs_vals)) >= 1), ]
  # also remove variable value rows with NA environmental values
  occs_vals_2 <- na.omit(occs_vals)
  # add columns for env variable values for each occurrence record
  occs <- cbind(occs_2, occs_vals_2)
  
  return(occs)
  
}

#' remove points outside of species sampling polygon
#' sometimes for some data points are mislabeled, this removes them
#' for current project data is good, so this function is not used. 
filter_occs <-function(occs,areaPoly,polyID){
  #Remove occurrences outside of user drawn polygon
  occs_or_null <- wallace::poccs_selectOccs(
    occs = occs,
    polySelXY = areaPoly,
    polySelID = polyID
  )
  
  #NOTE if there are no values to remove, poccs_selectOccs() returns NULL!!!
  if(is.null(occs_or_null)){
    return(occs)
  } else {
    return(occs_or_null)
  }
  
}

#' background sampling calculations
#' Sampling of 10000 background points and corresponding environmental data using a “point buffers” method with a 1 degree buffer.
#'
#' value: list with several data outputs bgExt, bgMask, bgSample, bgEnvsVals)
#'
bg_sampling <- function(occs, envs, species){
  
  # Generate background extent
  bgExt <- wallace::penvs_bgExtent(
    occs = occs,
    bgSel = "point buffers",
    bgBuf = 1)
  # Mask environmental data to provided extent
  bgMask <- wallace::penvs_bgMask(
    occs = occs,
    envs = envs,
    bgExt = bgExt)
  # Sample background points from the provided area
  bgSample <- wallace::penvs_bgSample(
    occs = occs,
    bgMask =  bgMask,
    bgPtsNum = 10000)
  
  
  # Extract values of environmental layers for each background point
  bgEnvsVals <- as.data.frame(raster::extract(bgMask,  bgSample))
  
  # Add extracted values to background points table
  bgEnvsVals <- cbind(scientific_name = paste0("bg_", species), bgSample,
                      occID = NA, year = NA, institution_code = NA, country = NA,
                      state_province = NA, locality = NA, elevation = NA,
                      record_type = NA, bgEnvsVals)
  
  bgDataList <- list("bgExt" = bgExt,
                     "bgMask" = bgMask,
                     "bgSample" = bgSample,
                     "bgEnvsVals" = bgEnvsVals)
  
  return(bgDataList)
}

#' gather params, create bg sample and create ENM Evaluation
run_model <- function(occs, envs, species,nCores=NULL){
  
  # save number of occurrences for setting params
  n_occs <- nrow(occs)
  
  # previously, feature classes based on n occurrence records 
  # LQH >= 15; LQ between 10 -14; L < 10
  ## feature class as string of letters
  #featureClass <- "L"
  #if(n_occs > 10 ){ featureClass <- paste0(featureClass, "Q") }
  #if(n_occs >=15 ){ featureClass <- paste0(featureClass, "H") }
  
  
  # Now just using static feature class for all data, but saving the code above  
  # in case we change our minds again
  
  featureClass <- "LQ"
  
  # 
  # Conditional settings for cross validation
  # Jackknife for <= 25
  # Random K-fold > 25
  
  
  partitioning_cutoff <- 25 
  
  if (n_occs <= partitioning_cutoff) {  
    partitioning_alg <- 'jackknife'   } 
  else {                  
    partitioning_alg <- 'randomkfold' }
  
  
  bgData <- bg_sampling(occs, envs, species)
  #generate full prediction extent for input into 'envs'
  envs_cropped <- raster::crop(envs, bgData$bgExt)
  #subset occs_ap to longitude and latitude
  occs_ll <- occs[,c("longitude","latitude")]
  
  ##RUN 1
  #Need to use Maxent.jar because of the ability to see perm importance
  #will have to store maxent jar file on HPC? Maxent uses this file to run.
  
  print(paste("running ENMevaluate n=", n_occs, "feature class", featureClass, " partitions", partitioning_alg))
  
  # if(is.null(nCores) || nCores == "" || is.na(nCores)){
  e.mx <- ENMeval::ENMevaluate(occs = occs_ll, envs = envs_cropped, bg = bgData$bgSample,
                               algorithm = 'maxent.jar', partitions = partitioning_alg,
                               tune.args = list(fc = featureClass, rm = 1),
                               numCores = nCores)      
  # } else {
  #    e.mx <- ENMeval::ENMevaluate(occs = occs_ll, envs = envs_cropped, bg = bgData$bgSample,
  #                       algorithm = 'maxent.jar', partitions = partitioning_alg,
  #                       tune.args = list(fc = featureClass, rm = 1))
  # }
  return(e.mx)
}


#' add identifying columns to dataframe enable aggregation of results
#' 
addGroupCols<- function(df, species, radiusKm, runNumber){
  new_df <- data.frame("species"=species, "radiuskm"=radiusKm, "rep" = runNumber, df)
  return(new_df)
}

#' consistently name RDA files so that can read them in again
model.Filename <- function(species,radiusKm,runNumber){
  paste0(species, "_enmeval_model.",radiusKm,"_km_run",runNumber,".Rdata")
}



#' write components of ENMevaluation to disk
#' e.mx output from ENMevaluate
#' species Genus_species used for file naming
#' radiusKm integer used for file naming
#' runNumber used for file naming
#' outputPath full or relative path to cwd (not to base path)
save_model <- function(e.mx, species, radiusKm, runNumber, outputPath ){
  
  print(paste("saving model output to ", outputPath))
  
  
  if(! file.exists(outputPath)){ dir.create(outputPath, recursive=TRUE) }
  
  # save whol results. this may be redundant with the CSVs, but can be useful for plotting
  
  
  save(e.mx, file=file.path(outputPath, model.Filename(species,radiusKm,runNumber)))
  
  # plot
  plot.Filename <- paste0(species, "_enmeval_responsecurevs.",radiusKm,"_km_run",runNumber,".pdf")
  response_curve_species_radius(e.mx, file.path(outputPath, plot.Filename))    
  
  # results
  e.mx.results <- e.mx@results  
  # "a_palliata_ENMeval_1x_results.1.run1.csv"
  results.Filename = paste0(species, "_enmeval_1x_results.",radiusKm,"_run",runNumber,".csv")
  write.csv(addGroupCols(e.mx.results,species, radiusKm, runNumber), 
            file=file.path(outputPath, results.Filename),
            row.names =FALSE)
  
  # minimize AICc
  # evaluated the AICc within 2 delta units
  minAIC <- e.mx.results[which(e.mx.results$delta.AICc <= 2),] #Make sure the column is delta.AICc
  minAIC.Filename <- paste0(species, "_min_AIC_em.x.", radiusKm, "_run",runNumber,".csv")
  # NOTE this file name is changed from original model_evaluation script to accommodate radius and run number
  write.csv(addGroupCols(minAIC,species, radiusKm, runNumber),
            file=file.path(outputPath, minAIC.Filename),
            row.names =FALSE)
  
  
  #Generate table of performance stats
  e.mx.stats <- e.mx.results[c("auc.train","cbi.train")]
  stats.Filename <- paste0(species, "_stats_e.mx.",radiusKm, "_run", runNumber,".csv")
  # "a_palliata_stats_e.mx.1_run1.csv"
  write.csv(addGroupCols(e.mx.stats,species, radiusKm, runNumber), 
            file.path(outputPath, stats.Filename),
            row.names =FALSE)
  
  
  # previous code didn't work because there is not a list item with this name
  # e.mx.var.imp <-e.mx@variable.importance$fc.LQHP_rm.1 --> #"a_palliata_permutation_imp_e.mx.1.run1.csv"
  # variable importance table, names are fc.Q_rm.1, fc.H_rm.1, etc.  there is no fc.LQHP_rm.1
  # loop through the fc names and save each as a CSV
  for(fcName in names(e.mx@variable.importance)) {
    varimp.Filename <- paste0(species, "_imp_e.mx_", fcName, "_",radiusKm, "_run", runNumber,".csv")  
    write.csv(addGroupCols(e.mx@variable.importance[[fcName]],species, radiusKm, runNumber), 
              file = file.path(outputPath, varimp.Filename),
              row.names =FALSE)
  }
  
  
  # loop through the fc names of the layers and save each as a raster
  for(layerName in names(e.mx@predictions)) {
    prediction.Filename = paste0(species, "_pred_",layerName, "_", radiusKm, "_run", runNumber,".tif")  
    writeRaster(e.mx@predictions[[layerName]], filename=file.path(outputPath, prediction.Filename), format="GTiff", overwrite=T)
  }  
  
}


#' 
#' read in one type of output CSV for all species, radii and reps. 
#' assumes/requires that there are subdirs for each speciese.g. /mnt/.../myoutput/+
readModelOutputs <-function(outputType,  outputPath){
  
  df = NULL
  fileMatchPattern = paste0(".*",outputType,".*\\.csv")
  for(speciesDir in list.files(outputPath)){
    for ( f in list.files(path = file.path(outputPath,speciesDir), pattern = fileMatchPattern )){
      new_df <- read.csv(file.path(outputPath,speciesDir,f),stringsAsFactors = FALSE)
      if(is.null(df)){ df <-  new_df } else { df <- rbind(df, new_df) }
    }
  }
  
  return(df)
  
}

aggregateModelOutputs <- function(outputPath,outputTypes = c("imp","AIC","stats","results"), writeCSVs=FALSE){
  
  # the different outputs have different numeric columns.  These column names are used by the summarise() method to 
  # to calculate the mean
  
  numeric_vars <- list()
  numeric_vars[['results']]<- c('auc.train', 'cbi.train', 'auc.diff.avg', 'auc.diff.sd', 'auc.val.avg', 'auc.val.sd', 'cbi.val.avg', 'cbi.val.sd', 'or.10p.avg', 'or.10p.sd', 'or.mtp.avg', 'or.mtp.sd', 'AICc', 'delta.AICc', 'w.AIC', 'ncoef')
  numeric_vars[['stats']] <- c('auc.train', 'cbi.train')
  numeric_vars[['AIC']]<- c('auc.train', 'cbi.train', 'auc.diff.avg', 'auc.diff.sd', 'auc.val.avg', 'auc.val.sd', 'cbi.val.avg', 'cbi.val.sd', 'or.10p.avg', 'or.10p.sd', 'or.mtp.avg', 'or.mtp.sd', 'AICc', 'delta.AICc', 'w.AIC', 'ncoef')
  numeric_vars[['imp']]<- c( 'percent.contribution', 'permutation.importance')
  
  group_vars<- list()
  group_vars[['results']] <- c('df','species', 'radiuskm')
  group_vars[['stats']] <- c('df','species', 'radiuskm')
  group_vars[['AIC']] <- c('df','species', 'radiuskm')
  group_vars[['imp']] <- c('df','species', 'radiuskm', 'variable')
  
  allOutputs = list()
  
  for( outputType in c("imp","AIC","stats","results")){
    
    df <- readModelOutputs(outputType,  outputPath)
    # there is probably a better way to do this, but I can't get how to use column names as strg vars in group by
    df <- switch(outputType, 
                 results = dplyr::group_by(df, species, radiuskm),
                 AIC = dplyr::group_by(df, species, radiuskm),
                 stats = dplyr::group_by(df, species, radiuskm),
                 imp= dplyr::group_by(df, species, radiuskm, variable)
    )
    
    # calculate the mean of the variables   
    allOutputs[[outputType]] <- dplyr::summarise(df,across(all_of( numeric_vars[[outputType]]), mean))
    
    if(writeCSVs == TRUE){
      outputFileName <- file.path(outputPath, paste0(outputType, "_mean.csv"))
      write.csv(allOutputs[[outputType]],file=outputFileName, row.names=FALSE)
    }
  }
  
  # now merge those output types with just one row per (species, radius) combo, merge them 
  # start with the first one
  mergedOutputs <- allOutputs[["AIC"]]
  
  # merge each of the others into it
  for(outputType in c("stats","results")){
    # baseR version of this - is the output any different or more correct?  
    # mergedOutputs <- merge(mergedOutputs,allOutputs[[outputType]])
    mergedOutputs <-   dplyr::left_join(mergedOutputs,allOutputs[[outputType]], by=c('species', 'radiuskm'))
  }
  return(mergedOutputs)
}


response_curve_species_radius<-function(e.mx, pdfFile=NULL){
  
  if(! is.null(pdfFile)) { pdf(pdfFile) }
  dismo::response(e.mx@models[[1]])
  if(!is.null(pdfFile)) { dev.off() }
  
}


sdm_read_and_run <- function(species, radiusKm = 1, runNumber = 1, basePath = NULL, envsDir = NULL, occsPathTemplate = NULL, occsFileNameTemplate = NULL, outputPath = NULL){
  message(paste("running model for ", species, "radius=", radiusKm, " run number=", runNumber ))
  
  basePath = sdmBasePath(basePath)
  # read environmental rasters
  envs <- read_envs(radius = radiusKm, envsDir = envsDir)
  
  # read and process occurrences
  occs <- read_occs(species, 
                    occsPathTemplate, 
                    occsFileNameTemplate,
                    basePath)
  occs <- process_occs(occs, envs)
  
  # run model
  e.mx <- run_model(occs, envs, species)
  
  # save if a folder was provided
  if(! is.null(outputPath)) {
    save_model(e.mx, species, radiusKm, runNumber, outputPath )
  }
  
  # return the model for the next step if any
  return(e.mx)
  
}


#' threshold rasters in sdm model
#' 
#' applies threshold and removes values for raster elements inside model. 
#' code by Beth Gerstner
sdmThreshold <- function(sdm, occs, type = "mtp", binary = FALSE){
  
  # sdm  = e.mx output from wallace?
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
  } else {
    stop(paste("sdmThreshold type must be mtp or p10 : ", type))
  }
  
  sdm_thresh <- sdm
  sdm_thresh[sdm_thresh < thresh] <- NA
  if(binary){
    sdm_thresh[sdm_thresh >= thresh] <- 1
  }
  
  return(sdm_thresh)
}


#' #' function wrapper for thresholding, to pull sdm and occs out of  e.mx for use in lapply across runs
#' imageThresh <- function(runNum, outputPath, species, radiusKm ){
#'       load(file=file.path(outputPath, species,model.Filename(species,radiusKm,runNum)))
#'       numOccs = length(e.mx@occs)
#'       if(numOccs >= 25) { 
#'         threshtype="p10" } 
#'       else { 
#'         thresh_type = "mtp" } 
#'       sdm_thresh = sdmThreshold(sdm=e.mx@predictions, occs=e.mx@occs[c("longitude","latitude")], type = thresh_type)
#'       return(sdm_thresh)
#' }

# sub-function that loads just one run's sdm, to be used with lapply below
loadSDM <- function(runNum, outputPath, species, radiusKm){
  rDataFile <- file.path(outputPath, species, model.Filename(species,radiusKm,runNum))
  load(file=rDataFile)
  # assumes the Wallace model object is called e.mx
  return(e.mx@predictions)
}

#' calculate the average SMD values for a Species X radius across runs and return
#' builds filename to open using model.Filename fn
meanSDM <- function(outputPath, species, radiusKm, numRuns = 3){
  
  # read SDMS for all runs into a list
  SDMlist <- lapply(1:numRuns, loadSDM, outputPath=outputPath, species=species, radiusKm=radiusKm) 
  
  # use the Reduce function on the list to calc the element-wise mean 
  # assumes all the SMDs are identical dimensions
  meanSDM <- Reduce("+",SDMlist)/numRuns 
  
  return(meanSDM)
}

# convenience function that just reads the occurrences CSV and counts rows.  
# sometimes we just need the count of occurrences. 
# this is hear to demonstrate how to do this, and also so you don't have to write it again
# 
numOccs<-function(species, occsPathTemplate = NULL, occsFileNameTemplate = NULL){
  
  # counting Occurrences from standard occurrences file path and names
  # using NULL for templates, then naming functions will get these values from the Environment
  # we use these to keep it as flex as possible
  occsdata <- read.csv(
    file.path(
      occsDataPath(species,occsPathTemplate),
      occsDataFilename(species,occsFileNameTemplate)
    )
  )
  
  return( nrow(occsdata) )
  
}


#' process SDM layer in e.mx and average across replicates, save result as new tif
imagePostProcessing<- function(outputPath, species, radiusKm, numRuns = 3){
  
  # read saved wallace e.mx models to pull SDMs
  # we could just read the tif files too
  speciesRadiusMeanSDM <- meanSDM(outputPath, species, radiusKm, numRuns) 
  thresh_type = ifelse(numOccs(species) >= 25, "p10", "mtp" )
  occs=read_occs(species)
  sdm_thresh = sdmThreshold(sdm=speciesRadiusMeanSDM, occs = occs[c('longitude','latitude')], type = thresh_type)
  return(sdm_thresh)
  
}

saveMeanSDM<- function(meanSDM, species, radiusKm, outputPath){
  sdmFilename = paste0(species, "_sdm_", radiusKm, "_mean.tif")  
  
  meanSdmFilename <- file.path(outputPath, species, sdmFilename)
  result <- writeRaster(meanSDM, filename=meanSdmFilename, format="GTiff", overwrite=T)
  if(file.exists(meanSdmFilename)){
    return(meanSdmFilename)
  } else {
    return(NA)
  }
  
}



imagePostProcessingAllRadii <- function(outputPath, species, numRuns = 3, radii = RADII){
  
  filesGenerated = c()
  # get list of .Rdata files Tremarctos_ornatus_enmeval_model.27_km_run2.Rdata
  # but for now just hard code the radii
  for ( radiusKm in radii ) {
    print(paste('processing', species, ", radius=", radiusKm))
    meanSDM <- imagePostProcessing(outputPath, species, radiusKm, numRuns)
    meanSDMFile <- saveMeanSDM(meanSDM, species, radiusKm, outputPath)
    print(meanSDMFile)
    filesGenerated = c(filesGenerated, meanSDMFile)
  }
  
  return(filesGenerated)
  
}

#' process all SDMs 
#' 
#' for all the species (directories in output path), invoke function to process all radii
#' this is separated so the functions can be tested without running all species, which takes a long time
imagePostProcessingAllSpecies <- function(outputPath, numRuns = 3){
  
  stopifnot(file.exists(outputPath))
  
  # get the species names from the subdirs of the output path
  # each directory in the output from the SDM code above is a species name
  speciesDirs <- list.dirs(outputPath, full.names =FALSE, recursive=FALSE)
  
  for(speciesDir in speciesDirs){
    imagePostProcessingAllRadii(outputPath, speciesDir, numRuns)
  }
  
}

sdm_average <- function(outputPath,numRuns= 3 ){
  mergedOutputs <- aggregateModelOutputs(outputPath,writeCSVs=TRUE)
  mergeOutputsFileName <- "merged_sdm_outputs.csv" 
  write.csv(mergedOutputs,file=file.path(outputPath,mergeOutputsFileName), row.names=FALSE)
  # the function below has the occurrence data read built-in, based on standardized file naming and env variables. 
  # it may be better to re-factor the image processing functioning to accept occs as a parameter so you can 
  # test diffenet data types and methods
  listOfImageFiles <- imagePostProcessingAllSpecies(outputPath, numRuns)
  return(listOfImageFiles)
}


####### Command Line Interface 
# this section of the script is only run when it's invoked from a script using 
# Rscript L1_1_sdm_model species radiusKm runNumber outputPath 

if (sys.nframe() == 0){
  usage <- "Rscript --vanilla sdm_run.R <Genus_species> <radiuskm[default 1]> <number of runs[default 1]> output_path full path to output)"
  help <- "set OS env variables SDM_BASE_PATH to main folder with data and SDM_ENVS_PATH to sub folder where rasters are"               
  
  args = commandArgs(trailingOnly= TRUE)
  
  print(args)
  
  # TODO use argparse for named arguments rather than positional
  # for more flex and to reduce errors
  
  # test if there are enough arguments: if not, return an error
  if (length(args) < 4) {
    stop(usage, call.=FALSE)
  } else { 
    species  = args[1] # valid species of form Genus_species to match file names
    radiusKm   = as.integer(args[2]) # integer matching envs dirs 
    runNumber = args[3] # integer
    outputPath = args[4] # full path of where to save files
  }
  
  
  # TODO validate all of these args.  for now just assume they are correct
  
  
  # TODO don't just rely on env variables for other paths/templates.  allow additional args
  
  # number of cores to limit to
  if(length(args > 5)) { nCores <- args[5] } else {nCores <- NULL}
  
  
  
  envs <- read_envs(radius = radiusKm)
  
  # read and process occurrences
  occs <- read_occs(species)
  occs <- process_occs(occs, envs)
  
  
  
  # run model
  e.mx <- run_model(occs=occs, envs=envs, species=species,nCores=nCores)
  
  # TODO check for validity in e.mx
  
  # save to the outputPath, which also creates the folder if necessary
  if(! is.null(outputPath)) {
    save_model(e.mx, species, radiusKm, runNumber, outputPath )
  }
  
  
}
