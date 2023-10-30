#heatmaps
library(expm)
library(ggplot2)
library(reshape2)
TPpop
TPagg
TPser
TPacp
TPctp
TPsag

#-----------------------------------Pop----------------------------------------------------------------------
melted_data <- melt(TPpop) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))

#-----------------------------------Agg----------------------------------------------------------------------
melted_data <- melt(TPagg) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------Ser----------------------------------------------------------------------
melted_data <- melt(TPser) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------acp----------------------------------------------------------------------
melted_data <- melt(TPacp) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------ctp----------------------------------------------------------------------
melted_data <- melt(TPctp) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))
#-----------------------------------sag----------------------------------------------------------------------
melted_data <- melt(TPpop) %>% filter((Var1 =='Attacking In' |Var1 == 'Defensive In' | Var1 =='Neutral In') &(Var2!='Point End') )
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       mid = "purple",
                       low = "white",
                       midpoint = 0.355,) +
  labs(x = "Column", y = "Row", 
       title = "Pop Matrix - Situation") +
  theme(axis.text.x = element_text(angle = 90))

