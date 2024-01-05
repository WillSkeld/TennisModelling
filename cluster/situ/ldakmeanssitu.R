lda_situ_data <- data.frame(type = p_situ$class, lda = p_situ$x)
library(ggplot2)
ggplot(lda_situ_data) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

playerSitkmeans<-kmeans(lda_situ_data[2:5],4)
factoextra::fviz_nbclust(lda_situ_data[2:5], kmeans, method = "wss",k.max=15)
