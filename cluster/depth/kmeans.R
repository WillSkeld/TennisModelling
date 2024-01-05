scaled_data_depth <- scale(sapply(playerMatrixdfDepth[,3:29], as.numeric))

playerSitkmeans<-kmeans(scaled_data_depth,5)
fviz_nbclust(scaled_data_depth, kmeans, method = "wss",k.max=15)
