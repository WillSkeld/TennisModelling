scaled_data_type <- scale(sapply(playerMatrixdfType[,c(3:44,47:77)], as.numeric))

playerSitkmeans<-kmeans(scaled_data_depth,5)
fviz_nbclust(scaled_data_type, kmeans, method = "wss",k.max=15)
