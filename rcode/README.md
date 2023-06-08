# Project Title
Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

# Project Summary
 Knowledge about species’ distributions is essential for conservation, especially in light of global change. Species distribution models (SDMs) often rely on mean environmental conditions, yet species distributions are also a function of environmental heterogeneity and filtering acting at multiple spatial scales. Geodiversity, which quantifies variation in Earth's features and processes, has the potential to improve SDMs and conservation assessments, but its effectiveness remains untested. We test a range of geodiversity variables computed at varying scales using climate and elevation data. We compare the predictive performance of MaxEnt SDMs generated using CHELSA bioclimatic variables to those also including geodiversity measures for 31 Colombian mammals. Results suggest that scales at which geodiversity is calculated impact the ability of SDMs to explain species distributions. Some variables consistently exhibited an increasing or decreasing trend in permutation importance with spatial grain, showing slight scale-dependence and indicating that some measures of geodiversity are more relevant at particular scales. Integrating geodiversity variables into SDMs, and doing so at the appropriate spatial scale, enhances our knowledge of species-environment relationships and may improve our ability to predict suitable habitats, thereby contributing to the conservation and management of biodiversity.

## Funding
NASA FINESST Grant #80NSSC19K1332

## Collaborators
- Beth E. Gerstner: PhD Candidate, Michigan State University (MSU)
- Mary E. Blair: Director of Biodiversity Informatics Research, American Museum of Natural History
- Patrick Bills: Data Scientist, MSU
- Cristian A. Cruz-Rodriguez: Alexander Von Humboldt Institute, Colombia
- Phoebe L. Zarnetske: PI, [MSU Spatial & Community Ecology Lab (SpaCE Lab)](http://www.communityecologylab.com)

## Directories

All directories are named for the data level, in accordance with guidelines from the [Environmental Data Initiative](http://www.environmentaldatainitiative.org) where Level 0 (L0) raw data are read in and cleaned, then output as Level-1 (L1) data, which are subsequently evaluated and summarized as Level-2 (L2) data.

## L0

The L0 subfolder contains scripts for Level-0 (raw data) analysis, mainly pulling and compiling data. This contains the following scripts: 

- L0_1_occ_thinning_loop.R: loop uses SpThin to spatially thin downloaded occurrence records in preparation for model running (L1)
- L0_2_generate_geodiv_variables.R: use geodiv package in R to generate geodiversity variables for use as inputs into SDMs (L1)
- L0_3_rescale_expert_maps: rescales certain expert maps with improper scales (Na:1 instead of 0:1).


## L1
The L1 subfolder contains scripts for Level-1 analysis, mainly calculating database statistics. Specifically:

- L1_1_model_runs.R: generate SDMs for species using previously made geodiversity variables (L0).
- L1_2_model_performance_evaluation: evaluate results of SDM runs for each species and generate key performance plots (Figures 2 & 3).
- L1_3_sdm_comparison: standardizes extents of all maps (expert, no-geodiversity models, geodiversity models), generates binary and continuous versions, and removes areas beyond dispersal barriers using species specific clipping regions.
- L1_4_gain_loss_calculations: calculates gain and loss in model predictions when comparing expert, no-geodiversity models, geodiversity models, and also determines omission rate of each. Basis for Table S1.
- L1_5_species_trait_groups: groups species based on traits (mass and diet) and generates box plots of performance for each spatial grain tested. Basis for Figure 4.


## L2
The L2 subfolder contains scripts for Level-2 analysis, mainly visualization of the database. Specifically:

- L2_1_biogeographic_region_plot: Map of biogeographic regions in Colombia used as a basis for this study. Basis for Figure 1.
- L2_2_species_comparison_example: Multipanel plots for two species which easily demonstrate differences in predictions between expert, no geodiversity, and geodiversity models.


*This readme last modified by BEG on June 9 2023*
