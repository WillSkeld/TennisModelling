lda_depth_data
library(fclust, quietly=TRUE)
fkm_depth = FKM(X = lda_depth_data[2:3], m = 1.5, RS = 50, index = "SIL.F")
summary(fkm_depth)
lda_depth_data$fkmCluster<-fkm_depth$clus[,1]
gmm_depth$classification
plot_ly(lda_depth_data, x = ~lda.LD1, y = ~lda.LD2, symbol = ~type, color = ~fkmCluster)
