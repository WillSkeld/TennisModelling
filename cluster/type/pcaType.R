playerMatrixdfType<- playerMatrixdfType %>% mutate(`Player.Gamestyle` = 
                                                       case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                                   `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                                   'Aggressive Baseliner',
                                                                 `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                                   `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                                   `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                                   'Big Server / Aggressive Baseliner',
                                                                 `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                                 `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                                                 `Player.Gamestyle` == 'Big server' ~ ' Big Server',
                                                       ))

pca_set <- playerMatrixdfType[,3:77]
pca_set<- scale(sapply(pca_set, as.numeric))
pca_set <- pca_set[,-c(43:44)]
pcatype<-prcomp(pca_set)
summary(pcatype)
