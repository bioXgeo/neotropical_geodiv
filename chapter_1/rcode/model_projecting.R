#projecting model to larger extent and viewing occurrence records

#par(mfrow=c(2,1), mar=c(2,1,2,0))
# The simplest model: linear features only and high regularization.
# The most complex model: linear, quadratic, and hinge features with low regularization
plot(eval.predictions(e.mx.21)[['fc.LQ_rm.2']], ylim = c(-30,20), xlim = c(-90,-30), 
     legend = FALSE, main = 'LQ_2 prediction')



# We can also plot the binned background points with the occurrence points on top to 
# visualize where the training data is located.
#points(eval.bg(e.mx.1), pch = 3, col = eval.bg.grp(e.mx.1), cex = 0.5)
points(eval.occs(e.mx.1), pch = 21, bg = eval.occs.grp(e.mx.1))

#project model to larger extent:
prediction <- predict(e.mx.21@models[[10]], envs_Bn)

final_model_bn <-prediction>=0.400
library(rgdal)

crop_extent <- readOGR("/Volumes/BETH'S DRIV/Olinguito_conservation_analysis/Clipping_extent/Clipping_extent.shp")

cropped_final_model <- mask(final_model_bn, crop_extent)
test<- crop(cropped_final_model, crop_extent)

writeRaster(test,filename="final_model_bn.tif",format="GTiff", overwrite=T)
