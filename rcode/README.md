# Project Title
Can scale-dependent geodiversity improve species distribution models in a Neotropical biodiversity hotspot?

# Project Summary
Knowledge about species’ distributions is essential for conservation, especially in the light of global change. Species distribution models (SDMs) are often used to identify species’ ranges as part of designating their conservation status. Despite the utility of SDMs for conservation, many species are data deficient, especially in montane biodiversity hotspots. Further, SDMs often rely solely on environmental variable means as predictors at single resolutions, yet species are also distributed in relation to environmental heterogeneity, and these relationships are scale-dependent. To capture spatial heterogeneity in the environment, scale-dependent measures of geodiversity can be applied to environmental variables and incorporated into SDMs. These variables offer an opportunity to improve biodiversity and conservation assessments, but their utility for SDMs has not been tested. We test a range of geodiversity variables computed at varying scales of remotely sensed climate and elevation. We compare the performance of Maxent SDMs generated using CHELSA bioclim variables only, to those including geodiversity measures for data poor mammals in montane Colombia. Results suggest that certain scales of geodiversity, such as standard deviation of elevation (SDelev), can improve the ability of SDMs to explain distributions of data poor species. Permutation importance, a measure of variable contribution in Maxent, also increased with spatial resolution of SDelev, signaling that certain measures of geodiversity are more relevant at some scales than others. This study’s open workflow for identifying scales of geodiversity predictors and their SDM performance, will help practitioners make more robust models for data poor species and improve conservation assessments.

## Funding
NASA FINESST Grant #80NSSC19K1332

## Collaborators
- Beth E. Gerstner: PhD Candidate, Michigan State University (MSU)
- Patrick Bills: Data Scientist, MSU
- Phoebe L. Zarnetske: PI, [MSU Spatial & Community Ecology Lab (SpaCE Lab)](http://www.communityecologylab.com)

## Directories

All directories are named for the data level, in accordance with guidelines from the [Environmental Data Initiative](http://www.environmentaldatainitiative.org) where Level 0 (L0) raw data are read in and cleaned, then output as Level-1 (L1) data, which are subsequently evaluated and summarized as Level-2 (L2) data.

## L0

The L0 subfolder contains scripts for Level-0 (raw data) analysis, mainly pulling and compiling data. This contains the following scripts: 

- L0_1_biomodelos_occ_download.R: download BioModelos based occurrence records based on previously determined species list
- L0_2_occ_thinning_loop.R: loop uses SpThin to spatially thin downloaded occurrence records in preparation for model running (L1)
- L0_3_generate_geodiv_variables.R: use geodiv package in R to generate geodiversity variables for use as inputs into SDMs (L1)
- workflow_diagram: diagram demonstrating all steps

## L1
The L1 subfolder contains scripts for Level-1 analysis, mainly calculating database statistics. Specifically:

- L1_1_model_runs.R: generate SDMs for species using previously made geodiversity varaibles (L0)
- L1_2_model_projecting.R: project SDMs for further visualization
- L1_3_model_run_stats.R: evaluate results of SDM runs for each species


## L2
The L2 subfolder contains scripts for Level-2 analysis, mainly visualization of the database. Specifically:

TBD

*This readme last modified by BEG & PLZ Dec 9 2022*
