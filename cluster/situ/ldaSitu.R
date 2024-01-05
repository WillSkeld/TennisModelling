playerMatrixdfSitu<- playerMatrixdfSitu %>% mutate(`Player.Gamestyle` = 
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
dim(playerMatrixdfSitu)
lda_set <- playerMatrixdfSitu[,2:29]
lda_set[,2:28]<- scale(sapply(lda_set[,2:28], as.numeric))
MASS::lda(Player.Gamestyle ~ ., data = lda_set)
