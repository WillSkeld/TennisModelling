lda_situ_data
library(fclust, quietly=TRUE)
fkm_situ = FKM(X = lda_situ_data[2:3], m = 2, RS = 50, index = "SIL.F")
summary(fkm_situ)
lda_situ_data$fkmCluster<-fkm_situ$clus[,1]
gmm_situ$classification
plot_ly(lda_situ_data, x = ~lda.LD1, y = ~lda.LD2, symbol = ~type, color = ~fkmCluster)
