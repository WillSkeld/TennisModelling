library(plotly)
lda_type_data <- data.frame(type = p$class, lda = p$x)
library(ggplot2)
ggplot(lda_type_data) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

playerTypekmeans<-kmeans(lda_type_data[2:3],5)
lda_type_data$cluster <- factor(playerTypekmeans$cluster)
levels(lda_type_data$cluster) <- c("Big Server / Aggressive Baseliner", "Counter Puncher", 
                                   "Big Server", "Aggressive Baseliner", "All Court Player")
plot_ly(lda_type_data, x = ~lda.LD1, y = ~lda.LD2, symbol = ~cluster, color = ~type)
plot_ly(lda_type_data, x = ~lda.LD1, y = ~lda.LD2, z = ~lda.LD3, symbol = ~cluster, color = ~type) %>% 
  layout(scene =list(aspectmode='cube'))

df<- data.frame(ldatype$scaling)

#transition matrix to scale to lda to kmeans 