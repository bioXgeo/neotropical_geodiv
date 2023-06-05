
# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -78, xmax = -67, ymin = -5, ymax = 13)


lagothrix_lagotricha <- read.csv("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/occurrence_records/Lagothrix_lagotricha_thinned_full/Lagothrix_lagotricha_thinned_wallace.csv")


# Retain only unique records. This helps prevent overloading R when mapping. There's no reason to have more than one occurrence for a species at the same location unless we are mapping richness.
full_lagotricha_occ  <-unique(lagothrix_lagotricha[c("scientific_name","latitude","longitude")])


lagotricha_33km_all <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/geodiv_models/Lagothrix_lagotricha_SDM_33_mean_redone_p10_thresh.tif")

lagotricha_33km <- mask(lagotricha_33km, colombia_shp)

lagotricha_clip <- readOGR("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/worked_example/lagothrix_clipping_3.shp")

lagotricha_33km <- mask(lagotricha_33km, lagotricha_clip, inverse = TRUE)

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_33 <- rasterToPoints(lagotricha_33km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_33_df <- data.frame(lagotricha_33)


test <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_33_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_SDM_33_mean_redone_p10_thresh)) + scale_fill_viridis()  # labs(fill = "Suitability") #  theme(legend.background = element_rect(fill = "transparent"),legend.position = c(0.85, 0.2))

#Add scale
final_lagotricha_33 <-test + theme(legend.position=c("none"), panel.background = element_rect(fill = "aliceblue"))+ ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=1)


#Create an inset map

#Load in South America
#Load in study area
#create ggplot map and can remove the zoom in part below

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


plot(SA_study_region[1])


#Change to spatial dataframe

SA_study_region_df <- fortify(SA_study_region)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw()


# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -78, 
    xmax = -69, 
    ymin = -5, 
    ymax = 10),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


# Add inset map to bird occurrence map
final_lagotricha_occs_inset <-ggdraw(final_lagotricha_33) +
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
    x = 0.19, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_lagotricha_occs_inset





lagotricha_1km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/no_geodiv_models/Lagothrix_lagotricha_SDM_1_mean_redone_p10_thresh.tif")

lagotricha_1km <- mask(lagotricha_1km, lagotricha_clip, inverse = TRUE)
lagotricha_1km <- mask(lagotricha_1km, colombia_shp)

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_1 <- rasterToPoints(lagotricha_1km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_1_df <- data.frame(lagotricha_1)


test_2 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_1_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_SDM_1_mean_redone_P10_thresh)) +   scale_fill_viridis()


#Add scale
final_lagotricha_1km <-test_2 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue")) + ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=1) #labs(fill = "Suitability") 



#Create an inset map

#Load in South America
#Load in study area
#create ggplot map and can remove the zoom in part below

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


plot(SA_study_region[1])


#Change to spatial dataframe

SA_study_region_df <- fortify(SA_study_region)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw()


# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -80, 
    xmax = -70, 
    ymin = 0, 
    ymax = 13),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


# Add inset map to bird occurrence map
final_lagotricha_occs_inset_1km <-ggdraw(final_lagotricha_1km) +
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
    x = 0.19, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.60,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.35, 
    height = 0.20)


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
final_lagotricha_expert <-test_3 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -72, x.max = -69.5, y.min =-5, y.max = -4,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomright", scale=.1, symbol=1) + ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red",size=1, alpha=.5)

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
    x = 0.19, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.70,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.30, 
    height = 0.15)


library(cowplot)
library(gridExtra)
# Create multipanel plot


grid.arrange(final_lagotricha_occs_inset_expert,final_lagotricha_1km, final_lagotricha_33, ncol = 3)

# Add labels to the grid
grid.text("expert", x = 0.2, y = 0.81, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("no geodiv", x = 0.53, y = 0.81, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("33km geodiv", x = 0.86, y = 0.81, gp = gpar(fontsize = 12, fontface = "bold"))


# Do same analysis with Lagothrix lagotricha

lagotricha_33km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/geodiv_models/Lagothrix_lagotricha_SDM_33_mean_redone_p10_thresh.tif")

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_33 <- rasterToPoints(lagotricha_33km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_33_df <- data.frame(lagotricha_33)


test <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_33_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_SDM_33_mean_redone_p10_thresh)) + scale_fill_gradient(low="darkblue", high="white")   # labs(fill = "Suitability") #  theme(legend.background = element_rect(fill = "transparent"),legend.position = c(0.85, 0.2))

#Add scale
final_lagotricha_33 <-test + theme(legend.position=c("none"), panel.background = element_rect(fill = "aliceblue")) + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=.5)


#Create an inset map

#Load in South America
#Load in study area
#create ggplot map and can remove the zoom in part below

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


plot(SA_study_region[1])


#Change to spatial dataframe

SA_study_region_df <- fortify(SA_study_region)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw()


# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -80, 
    xmax = -70, 
    ymin = 0, 
    ymax = 13),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


# Add inset map to bird occurrence map
final_lagotricha_occs_inset <-ggdraw(final_lagotricha_33) +
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
    x = 0.19, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_lagotricha_occs_inset


# 1km

lagotricha_1km <- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/no_geodiv_models/Lagothrix_lagotricha_SDM_1_mean_redone_p10_thresh.tif")

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_1 <- rasterToPoints(lagotricha_1km, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_1_df <- data.frame(lagotricha_1)


test_2 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_1_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_SDM_1_mean_redone_p10_thresh)) + scale_fill_gradient(low="darkblue", high="white")

#Add scale
final_lagotricha_1km <-test_2 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue")) +  ylab("Latitude") + xlab("Longitude") + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=.5) #labs(fill = "Suitability") 



#Create an inset map

#Load in South America
#Load in study area
#create ggplot map and can remove the zoom in part below

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


plot(SA_study_region[1])


#Change to spatial dataframe

SA_study_region_df <- fortify(SA_study_region)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw()


# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -80, 
    xmax = -70, 
    ymin = 0, 
    ymax = 13),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


# Add inset map to bird occurrence map
final_lagotricha_occs_inset_1km <-ggdraw(final_lagotricha_1km) +
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
    x = 0.19, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.60,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.35, 
    height = 0.20)


#expert map
lagotricha_expert<- raster("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/sdms/expert_maps/Lagothrix_lagotricha_expert.tif")

# Need the raster to be a dataframe for mapping. Convert to points to do this.
lagotricha_expert <- rasterToPoints(lagotricha_expert, spatial = TRUE)

# Then convert to a 'conventional' dataframe
lagotricha_expert_df <- data.frame(lagotricha_expert)


test_3 <- ggplot() + theme_bw() + geom_sf(data = study_region_crop, fill = "white") + geom_raster(data=lagotricha_expert_df, aes(x=x, y=y, fill=Lagothrix_lagotricha_expert)) + scale_fill_gradient(low="white", high="black")

#Add scale
final_lagotricha_expert <-test_3 + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue")) + ylab("Latitude") + xlab("Longitude") +  scalebar(x.min = -76, x.max = -74, y.min =-5, y.max = -4,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.1, symbol=1) + geom_point(data = full_lagotricha_occ, aes(x = longitude, y = latitude), color="red", size=1, alpha=.5)

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
    x = 0.19, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.7,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.28, 
    height = 0.15)



library(gridExtra)
grid.arrange(final_lagotricha_occs_inset_expert,final_lagotricha_1km, final_lagotricha_33, ncol = 3)

# Add labels to the grid
grid.text("Expert", x = 0.2, y = 0.88, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("No geodiv", x = 0.53, y = 0.88, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("Optimal geodiv (33km)", x = 0.86, y = 0.88, gp = gpar(fontsize = 12, fontface = "bold"))


cropped_lagotricha_1km <- crop(lagotricha_1km, lagotricha_expert)
cropped_lagotricha_33km <- crop(lagotricha_33km, lagotricha_expert)



cropped_lagotricha_1km <- resample(cropped_lagotricha_1km, lagotricha_expert, method = "bilinear")
cropped_lagotricha_33km <- resample(cropped_lagotricha_33km, lagotricha_expert, method = "bilinear")

# Check that all rasters have the same number of columns and rows
compareRaster(lagotricha_expert, r2_res)
compareRaster(lagotricha_expert, r3_res)

lagotricha_stack <- stack(lagotricha_expert, cropped_lagotricha_1km, cropped_lagotricha_33km)

#Schoener's D
test <- calc.niche.overlap(lagotricha_stack, "D", quiet = T)
