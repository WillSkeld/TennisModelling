library(cluster)
library(factoextra)
scaled_data_situ <- scale(sapply(playerMatrixdf[,3:29], as.numeric))

playerSitkmeans<-kmeans(scaled_data_type,5)
fviz_nbclust(scaled_data_situ, kmeans, method = "wss",k.max=50)

c1<-t(matrix(playerSitkmeans$centers[1,],nrow = 9,ncol = 3))
c2<-t(matrix(playerSitkmeans$centers[2,],nrow = 9,ncol = 3))
c3<-t(matrix(playerSitkmeans$centers[3,],nrow = 9,ncol = 3))
c4<-t(matrix(playerSitkmeans$centers[4,],nrow = 9,ncol = 3))
c5<-t(matrix(playerSitkmeans$centers[5,],nrow = 9,ncol = 3))

playerMatrixdfCluster<-cbind(playerMatrixdf,playerSitkmeans$cluster)
playerMatrixdfCluster%>%filter(`playerSitkmeans$cluster`==1)%>%select(Player,Player.Gamestyle,`playerSitkmeans$cluster`)
playerMatrixdfCluster%>%filter(`playerSitkmeans$cluster`==2)%>%select(Player,Player.Gamestyle,`playerSitkmeans$cluster`)
playerMatrixdfCluster%>%filter(`playerSitkmeans$cluster`==3)%>%select(Player,Player.Gamestyle,`playerSitkmeans$cluster`)
playerMatrixdfCluster%>%filter(`playerSitkmeans$cluster`==4)%>%select(Player,Player.Gamestyle,`playerSitkmeans$cluster`)
playerMatrixdfCluster%>%filter(`playerSitkmeans$cluster`==5)%>%select(Player,Player.Gamestyle,`playerSitkmeans$cluster`)

kmeans(playerMatrixdf[,3:29],5)
for(i in 1:10){
  t<-kmeans(playerMatrixdf[,3:29],i)
  print(t$betweenss/t$totss)
}
