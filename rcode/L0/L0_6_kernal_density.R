#Kernal density plot of occurrence records across scale 
#Define a function to create an MCP
simpleMCP <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  p <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1)))
}

#load in species occurrence records
pts.sp <- read.csv("put record path")

#Generate MCP  
mcp <- simpleMCP(pts.sp)
plot(mcp, add=TRUE)

#Buffer MCP by 0.7 deg and mask one environmental variable raster by it
mcp_buf <- gBuffer(mcp, width=0.7)
plot(mcp_buf, add=TRUE)
mcp_buf_mask <- mask(env_crop[[1]], mcp_buf)

#Create background points for the future density plot
mcp.backg.density <- randomPoints(mcp_buf_mask[[1]], n=10000, excludep=FALSE)

# Extract environmental values for background points
mcp_backg_dens <- extract(env_crop, mcp.backg.density)

# Get values for Bio1 and Bio12 from background environments
y <- mcp_backg_dens[,c(1,4)]

# Builds Two-Dimensional Kernel Density Estimation
kde2 <- kde2d(y[,1], y[,2], n=1000, lims = c(.5, 30, 0, 10400))

# Show density plot
image(kde2, col= gray.colors(n=12, start=1, end=.4), xlab= ('Annual mean temperature'~(degree~C)), ylab='Annual precipitation (mm)', xlim = c(0,30), ylim = c(0,10400), cex=1)

#This formula allows us to calculate the value of N at which 99% of the all density  values are above N. Plugged in numbers until reaching 99%. 
sum(kde2$z[kde2$z >= .000000616]) / sum(kde2$z) = .99%

# Draws the contour lines on the density plot
contour(kde2,levels=.000000616, drawlabels=T, col="black", labcex=1, offset=4, labels="99%", add=T ) #99%

# Adding environmental values at occurrence records to the density plot
points(occ_orig_env[,1], occ_orig_env[,4], col='black', pch=20, cex=.8)


# Add legend for occurrence data
legend(2,9800, legend=c("Georeferences from Helgen et al. 2013", "", "Records used in this study:" ,"Georeferenced museum", "Citizen science"),
       col=c("black", "black", "black", "black", "black"), cex=.95, pch=c(20,NA,NA, 22,1))

