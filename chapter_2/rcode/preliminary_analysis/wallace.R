#Project: Using geodiversity to improve SDMs for data poor species

#Description: This code is a workflow to build SDMs using 1) bioclim variables 2) bioclims & geodiversity measures, testing model performancing, processing outputs using MODIS forest cover, and calculating EOO to understand how inclusion of geodiversity affects model outputs and resulting estimations of IUCN metrics. 

#Authors: Beth E. Gerstner

#Date: 1/18/22

install.packages("devtools")
devtools::install_github("cmerow/maskRangeR/maskRangeR", dependencies=TRUE)
devtools::install_github("https://github.com/andrepazv/changeRangeR", dependencies= TRUE)
1# load packages
library(maskRangeR)
library(changeRangeR)


devtools::install_github("nathanvan/parallelsugar")
devtools::install_github("https://github.com/andrepazv/changeRangeR/tree/paths_fix2", dependencies= TRUE)
# Load package
library(changeRangeR)

install.packages("ENMeval")

devtools::install_github("https://github.com/wallaceEcoMod/wallace/tree/biomodelos", dependencies = TRUE, force=T)
# Open Wallace
library(wallace)
run_wallace()

library(shiny)
runExample("01_hello", host = "0.0.0.0", port = 9999)

remotes::install_github("https://github.com/wallaceEcoMod/wallace/tree/biomodelos")

