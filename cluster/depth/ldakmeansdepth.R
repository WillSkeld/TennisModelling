lda_depth_data <- data.frame(type = p_depth$class, lda = p_depth$x)
library(ggplot2)
ggplot(lda_depth_data) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

playerDepthkmeans<-kmeans(lda_depth_data[2:3],5)
lda_depth_data$cluster <- factor(playerDepthkmeans$cluster)
factoextra::fviz_nbclust(lda_depth_data[2:4], kmeans, method = "wss",k.max=15)
lda_depth
plot_ly(lda_depth_data, x = ~lda.LD1, y = ~lda.LD2, z = ~lda.LD3, color = ~cluster, symbol = ~type) %>% 
  layout(scene =list(aspectmode='cube'))
plot_ly(lda_depth_data, x = ~lda.LD1, y = ~lda.LD2, color = ~cluster, symbol = ~type)
