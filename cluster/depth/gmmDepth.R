lda_depth_data
library(mclust, quietly=TRUE)
gmm_depth = Mclust(data=lda_depth_data[,2:3],G=4, modelNames = "VEI")
summary(gmm_depth)
lda_depth_data$gmmCluster<-gmm_depth$classification
gmm_depth$classification
plot_ly(lda_depth_data, x = ~lda.LD1, y = ~lda.LD2, symbol = ~type, color = ~gmmCluster)
plot_ly(lda_depth_data, x = ~lda.LD1, y = ~lda.LD2, z =~lda.LD3, symbol = ~type, color = ~gmmCluster) %>% 
  layout(scene =list(aspectmode='cube'))
mclust.options("emModelNames")