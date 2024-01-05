lda_type_data
library(fclust, quietly=TRUE)
fkm_type = FKM(X = lda_type_data[2:3], m = 2, RS = 50, index = "SIL.F")
summary(fkm_type)
lda_type_data$fkmCluster<-fkm_type$clus[,1]
gmm_type$classification
plot_ly(lda_type_data, x = ~lda.LD1, y = ~lda.LD2, symbol = ~type, color = ~fkmCluster)
