library(plotly)
serve_shots <- inner_join(serve_stats_t, shot_type_stats_s, 'Player1') %>% 
  filter(Base_pct > 90 & Gs_pct > 75)
serve_shots %>% ggplot(aes(x = ace_pct, y = Gs_pct)) + geom_point()
serve_shots %>% ggplot(aes(x = ace_pct, y = Base_pct)) + geom_point()
p<-plot_ly(serve_shots, x = ~Base_pct, y = ~Gs_pct, z = ~ace_pct) %>% 
  layout(scene =list(aspectmode='cube'))
p
serve_shots_k <- serve_shots %>% select(c(ace_pct, Gs_pct, Base_pct))
kcl <- (kmeans(serve_shots_k, 4))
serve_shots_k$cluster = factor(kmeans(serve_shots_k,4)$cluster)
plot_ly(serve_shots_k, x = ~Base_pct, y = ~Gs_pct, z = ~ace_pct, color = ~cluster) %>% 
  layout(scene =list(aspectmode='cube'))
