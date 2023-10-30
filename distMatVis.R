mats<-list(TPagg,TPacp,TPser,TPctp,TPsag)
M<-matrix(nrow = 5, ncol = 5)

for(i in 1:5){
  for(j in 1:5){
    M[i,j]<- norm(mats[[i]]-mats[[j]], 'F')
  }
}
colnames(M)<- c('AGG', 'ACP', 'SER', 'CTP', 'SAG')
rownames(M)<- c('AGG', 'ACP', 'SER', 'CTP', 'SAG')
melted_data <- melt(M)
ggplot(melted_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.1,) +
  labs(
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))
