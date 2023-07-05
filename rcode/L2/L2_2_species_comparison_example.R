
#Title: Species prediction examples and comparisons

#Project: Assessing the impact of scale-dependent geodiversity on species distribution models in a biodiversity hotspot

#Description: This code makes a multipanel figure comparing expert, no geodiversity, and geodiversity models for two species (Lagothrix lagotricha; Aotus griseimembra). Occurrence records for each species are overlayed on the maps.

#Data input: SDMs and expert maps for two species mentioned above.

#Data output: Multipanel plots for each species (inputs for Figure 5 within manuscript)

#Author: Beth E. Gerstner

#Collaborators: Mary E. Blair, Cristian A. Cruz-Rodriguez, Phoebe L. Zarnetske, Patrick Bills

#load libraries
library(raster)
library(dplyr)
library(raster)
library(maps)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(sf)
library(ggsn)
library(rgeos)
library(BAMMtools)
library(viridis)
library(cowplot)
library(gridExtra)

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -78, xmax = -67, ymin = -5, ymax = 13)

#read in occurrence records for species
lagothrix_lagotricha <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/occurrence_records/Lagothrix_lagotricha_thinned_full/Lagothrix_lagotricha_thinned_wallace.csv")


# Retain only unique records. 
full_lagotricha_occ  <-unique(lagothrix_lagotricha[c("scientific_name","latitude","longitude")])

# Read in optimal model for species (33 km run)
lagotricha_33km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/geodiv_optimal_models/Lagothrix_lagotricha_geodiv_masked_cont.tif")

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_33 <- rasterToPoints(lagotricha_33km, spatial = TRUE)

# Convert to a 'conventional' dataframe
lagotricha_33_df <- data.frame(lagotricha_33)

# Test map
test <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_33_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_geodiv_masked_cont)) + scale_fill_viridis()  # labs(fill = "Suitability") #  theme(legend.background = element_rect(fill = "transparent"),legend.position = c(0.85, 0.2))

#Add scale
final_lagotricha_33 <-test + theme(legend.position=c("none"), panel.background = element_rect(fill = "aliceblue"))+ ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=1)

lagotricha_1km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/no_geodiv_models/Lagothrix_lagotricha_no_geodiv_masked_cont.tif")


# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_1 <- rasterToPoints(lagotricha_1km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_1_df <- data.frame(lagotricha_1)


test_2 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_1_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_no_geodiv_masked_cont)) +   scale_fill_viridis()


#Add scale
final_lagotricha_1km <-test_2 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue")) + ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=1) #labs(fill = "Suitability") 

#expert map
lagotricha_expert<- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/expert_maps/Lagothrix_lagotricha_expert.tif")

#remove 0
values(lagotricha_expert)[values(lagotricha_expert) == 0] = NA

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_expert <- rasterToPoints(lagotricha_expert, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_expert_df <- data.frame(lagotricha_expert)

test_3 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_expert_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_expert)) + scale_fill_gradient(low="white", high="black")

#Add scale
final_lagotricha_expert <-test_3 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -73, x.max = -68.8, y.min =-4.6, y.max = -3.8,dist = 150, st.dist=.3, st.size=2.1, height=.4, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomright", scale=.1, symbol=1) + ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red",size=1, alpha=.5)


#Change to spatial dataframe
SA_study_region_df <- fortify(study_region)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw()


# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -79, 
    xmax = -67, 
    ymin = -5, 
    ymax = 14),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


# Add inset map to bird occurrence map
final_lagotricha_occs_inset_expert <-ggdraw(final_lagotricha_expert) +
  draw_plot(
    {
      inset_map_box +
        theme(legend.position = "none", axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_blank(),
              panel.grid.major = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.16, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.65,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.30, 
    height = 0.13)

# Create multipanel plot
lagotricha <-grid.arrange(final_lagotricha_occs_inset_expert,final_lagotricha_1km, final_lagotricha_33, ncol = 3)

# Add labels to the grid
grid.text("expert", x = 0.2, y = 0.81, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("no geodiv", x = 0.53, y = 0.81, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("33km geodiv", x = 0.86, y = 0.81, gp = gpar(fontsize = 12, fontface = "bold"))


# Do same analysis with Aotus griseimembra

# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -78, xmax = -72, ymin = 2, ymax = 13)
plot(study_region_crop)

# Load occurrence records for species
Aotus_griseimembra <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/occurrence_records/Aotus_griseimembra_thinned_full/Aotus_griseimembra_thinned_wallace.csv")


# Retain only unique records. 
full_griseimembra_occ  <-unique(Aotus_griseimembra[c("scientific_name","latitude","longitude")])

#Load optimal model for species
griseimembra_27km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/geodiv_optimal_models/Aotus_griseimembra_geodiv_masked_cont.tif")

# Need the raster to be a dataframe for mapping. Convert to points to do this.
griseimembra_27 <- rasterToPoints(griseimembra_33km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
griseimembra_27_df <- data.frame(griseimembra_33)

# test plot
test <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=griseimembra_27_df, aes(x=x, y=y, fill=Aotus_griseimembra_geodiv_masked_cont)) + scale_fill_viridis()  # labs(fill = "Suitability") #  theme(legend.background = element_rect(fill = "transparent"),legend.position = c(0.85, 0.2))

#Add scale
final_griseimembra_27 <-test + theme(legend.position=c("none"), panel.background = element_rect(fill = "aliceblue"))+ ylab("Latitude") + xlab("Longitude") + geom_point(data = full_griseimembra_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=1)

# Load no geodiversity model
griseimembra_1km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/post_processed_models/no_geodiv_models/Aotus_griseimembra_no_geodiv_masked_cont.tif")


# Need the raster to be a dataframe for mapping. Convert to points to do this.
griseimembra_1 <- rasterToPoints(griseimembra_1km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
griseimembra_1_df <- data.frame(griseimembra_1)


# Test plot
test_2 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=griseimembra_1_df, aes(x=x, y=y, fill=Aotus_griseimembra_no_geodiv_masked_cont)) +   scale_fill_viridis()


#Add scale
final_griseimembra_1km <-test_2 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue")) + ylab("Latitude") + xlab("Longitude") + geom_point(data = full_griseimembra_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=1) #labs(fill = "Suitability") 

#expert map
griseimembra_expert<- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/expert_maps/Aotus_griseimembra_expert.tif")

#remove 0
values(griseimembra_expert)[values(griseimembra_expert) == 0] = NA

# Need the raster to be a dataframe for mapping. Convert to points to do this.
griseimembra_expert <- rasterToPoints(griseimembra_expert, spatial = TRUE)

# Then convert to a 'conventional' dataframe
griseimembra_expert_df <- data.frame(griseimembra_expert)

#test plot
test_3 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=griseimembra_expert_df, aes(x=x, y=y, fill=Aotus_griseimembra_expert)) + scale_fill_gradient(low="white", high="black")

#Add scale
final_griseimembra_expert <-test_3 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -75, x.max = -73, y.min =2.3, y.max = 3,dist = 150, st.dist=.2, st.size=2.1, height=.2, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomright", scale=.1, symbol=1) + ylab("Latitude") + xlab("Longitude") + geom_point(data = full_griseimembra_occ, aes(x = longitude, y = latitude), color="red",size=1, alpha=.5)

# Add inset map 
final_griseimembra_occs_inset_expert <-ggdraw(final_griseimembra_expert) +
  draw_plot(
    {
      inset_map_box +
        theme(legend.position = "none", axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_blank(),
              panel.grid.major = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.18, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.66,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.30, 
    height = 0.15)

# Create multipanel plot
griseimembra <-grid.arrange(final_griseimembra_occs_inset_expert,final_griseimembra_1km, final_griseimembra_33, ncol = 3)

# Add labels to the grid
grid.text("expert", x = 0.2, y = 0.83, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("no geodiv", x = 0.53, y = 0.83, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("27km geodiv", x = 0.86, y = 0.83, gp = gpar(fontsize = 12, fontface = "bold"))

