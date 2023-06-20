#Biogeographic region map
library(ggnewscale)
# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Colombia")


# Load the study region shapefiles
study_region_1 <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/biogeographic_regions/Choco-Darien1.shp")
study_region_2 <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/biogeographic_regions/Guajira1.shp")
study_region_3 <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/biogeographic_regions/Imeri1.shp")
study_region_4 <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/biogeographic_regions/Magdalena1.shp")
study_region_5 <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/biogeographic_regions/Paramo1.shp")
study_region_6 <- st_read("C:/Users/bgers/Desktop/MSU/Zarnetske_Lab/Data/Chapter_2/results/biogeographic_regions/Sabana1.shp")

legend_data <- data.frame(
  Study_Region = c("Chocó-Darién", "Caribbean*", "Amazonian*", "Magdalena", "Andean*", "Sabana"),
  Fill_Color = c("#CC00CC", "#FF9900", "#99CCFF", "#339900", "#FF3333", "#FFCC66")
)

# Plot the world map of Colombia with legend
biogeographic_regions <-ggplot() + new_scale_fill() +
  geom_sf(data = study_region, fill = "white", color = "black") +
  geom_sf(data = study_region_1, aes(fill = "Region 1"), color = "black") +
  geom_sf(data = study_region_2, aes(fill = "Region 2"), color = "black") +
  geom_sf(data = study_region_3, aes(fill = "Region 3"), color = "black") +
  geom_sf(data = study_region_4, aes(fill = "Region 4"), color = "black") +
  geom_sf(data = study_region_5, aes(fill = "Region 5"), color = "black") +
  geom_sf(data = study_region_6, aes(fill = "Region 6"), color = "black") +
  scale_fill_manual(
    values = legend_data$Fill_Color,
    labels = legend_data$Study_Region,
    name = "Biogeographic Region"
  ) +
  theme_bw() + scalebar(x.min = -79, x.max = -76.5, y.min =-4.7, y.max = -3,dist = 150, st.dist=.2, st.size=3, height=.1, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region, location="bottomleft", scale=.1, symbol=1) + xlab("Longitude") + ylab("Latitude") + theme(text = element_text(size = 12))

#Create inset

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


plot(SA_study_region[1])

# Create a bounding box geometry
bbox <- st_bbox(c(xmin=-83, xmax= -60, ymin= -20, ymax=13), crs = st_crs(SA_study_region))

# Crop the polygon to the bounding box
cropped_polygon <- st_crop(SA_study_region, bbox)


#Change to spatial dataframe

SA_study_region_df <- fortify(cropped_polygon)

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

biogeo_occs_inset <-ggdraw(biogeographic_regions) +
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
    x = -0.015, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.76,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.22)

