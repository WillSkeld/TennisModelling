library(tidyverse)
library(dplyr)
sts <- sbs %>% filter(Type=="men's singles")
sts$`Shot Number`[as.numeric(sts$`Shot Number`)>2] <- 'Rally'

player<-"Lorenzo Sonego"
RF<-playerStateMatrixType("Roger Federer")
playerGamestyles<-sts %>% select(Player, `Player Gamestyle`) %>% distinct() %>% 
  drop_na()%>% as.list()
fullstatelistType<-statesIDs_type
playerMatrixdfType<-data.frame(Player=character(),`Player Gamestyle`=character(),
                           DSI_DSE=character(),DSI_GE=character(),DSI_LE=character(),DSI_OE=character(),DSI_VE=character(),
                           DSI_DSI=character(),DSI_GI=character(),DSI_LI=character(),DSI_OI=character(),DSI_VI=character(),
                           DSI_DSW=character(),DSI_GW=character(),DSI_LW=character(),DSI_OW=character(),DSI_VW=character(),
                           GI_DSE=character(),GI_GE=character(),GI_LE=character(),GI_OE=character(),GI_VE=character(),
                           GI_DSI=character(),GI_GI=character(),GI_LI=character(),GI_OI=character(),GI_VI=character(),
                           GI_DSW=character(),GI_GW=character(),GI_LW=character(),GI_OW=character(),GI_VW=character(),
                           LI_DSE=character(),LI_GE=character(),LI_LE=character(),LI_OE=character(),LI_VE=character(),
                           LI_DSI=character(),LI_GI=character(),LI_LI=character(),LI_OI=character(),LI_VI=character(),
                           LI_DSW=character(),LI_GW=character(),LI_LW=character(),LI_OW=character(),LI_VW=character(),
                           OI_DSE=character(),OI_GE=character(),OI_LE=character(),OI_OE=character(),OI_VE=character(),
                           OI_DSI=character(),OI_GI=character(),OI_LI=character(),OI_OI=character(),OI_VI=character(),
                           OI_DSW=character(),OI_GW=character(),OI_LW=character(),OI_OW=character(),OI_VW=character(),
                           VI_DSE=character(),VI_GE=character(),VI_LE=character(),VI_OE=character(),VI_VE=character(),
                           VI_DSI=character(),VI_GI=character(),VI_LI=character(),VI_OI=character(),VI_VI=character(),
                           VI_DSW=character(),VI_GW=character(),VI_LW=character(),VI_OW=character(),VI_VW=character())

playerMatrixList<-list()
fullstatelistType<-statesIDs_type
for(i in 1:86){
  playerMatrixList[[playerGamestyles$Player[i]]]<-playerStateMatrixType(playerGamestyles$Player[i])
  pVector<-t(playerMatrixList[[playerGamestyles$Player[i]]][6:10,1:15]) %>% as.vector()
  pVector
  playerMatrixdfType[i,]<-c(playerGamestyles$Player[i],playerGamestyles$`Player Gamestyle`[i], pVector)
}
playerMatrixdfType[23,]

playerStateMatrixType<-function(player){
  
  sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number', 
                           'Shot Type', 'Serve', 'Situation', 'Shot Stroke','Shot Type', 'Shot By', 'Player Gamestyle')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
    mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(!(`Shot Outcome` == 'Double Fault' & `Shot Number` == 'Return') &
                                                            !(is.na(`Shot Outcome`) & `Shot Number` == 'Serve') & 
                                                            !(`Shot Outcome` == 'Out') & 
                                                            !(is.na(`Shot Stroke`) & !(`Shot Number` == 'Serve')) &
                                                            !(`Shot Outcome` == 'Return WInner') & !(`Shot Outcome` == 'WInner') &
                                                            !(is.na(`Shot Number`)) & 
                                                            !(`Shot Outcome` == 'Winner' &`Shot Number` == 'Return') &
                                                            !(`Shot Number`== 'Serve' & !is.na(`Shot Stroke`))
    ) %>% filter(Player == player) #Filter data and reduce columns
  
  reducedSts<- sts1 %>% mutate(`Shot Outcome` = case_when(`Shot Outcome` == 'Return Error' | 
                                                            `Shot Outcome` == 'Unforced Error' |
                                                            `Shot Outcome` == 'Forced Error' ~ 'Error', 
                                                          `Shot Outcome` == 'Return Winner' |
                                                            `Shot Outcome` == 'Winner'  ~ 'Winner',
                                                          `Shot Outcome` == 'In'  ~ 'In'),
                               `Shot Type` = case_when(`Shot Type` == 'Drop Shot' |`Shot Type` == 'Drop' |`Shot Type` == 'Drop Volley' ~ 'Drop Shot', 
                                                       `Shot Type` == 'Volley' |`Shot Type` == 'Half Volley' | `Shot Type` == 'Drive Volley' |
                                                         `Shot Type` == 'Smash'  ~ 'Volley',
                                                       `Shot Type` == 'Lob' |`Shot Type` == 'Lob Volley'  ~ 'Lob',
                                                       `Shot Type` == 'Other' |`Shot Type` == 'Unclassified' | `Shot Type` == 'Pick-Up'  ~ 'Other',
                                                       `Shot Type` == 'Groundstroke' | `Shot Type` == 'Slice' | `Shot Type` == 'Block'  ~ 'Groundstroke'
                               )) %>% filter(!(is.na(Situation)) & !(is.na(`Shot Type`)))
  
  reducedOutcomes_type <- reducedSts %>% filter(`Shot Number`!= 'Serve') %>% select(c('Shot Outcome', 'Shot Type')) %>% distinct()
  
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  statesIDs_type <- fullstatelistType
  
  seqShotIDs_type <- reducedSts %>%  filter(`Shot Number`!= 'Serve') %>% 
    select(c('Shot Index','Shot Outcome', 'Shot Number', 'Shot Type', 'ovrID', 'Shot By')) %>%
    left_join(statesIDs_type, by=join_by(`Shot Outcome`, `Shot Type`)) %>%
    select(stateID, `Shot Index`, ovrID, `Shot By`) #group each shot to find sequential points using a point ID
  
  pairStates_type <- seqShotIDs_type %>% left_join(seqShotIDs_type, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
    filter(`Shot By.x` == 0) %>% select(c('stateID.x', 'stateID.y')) %>% 
    filter(((stateID.x == 2 | stateID.x == 1 | stateID.x == 3 | stateID.x == 5 | stateID.x == 4 |
               stateID.x == 15 | stateID.x == 14 | stateID.x == 13 | stateID.x == 12 | stateID.x == 11) 
            & is.na(stateID.y)) | ((stateID.x == 6 | stateID.x == 7 |  stateID.x == 9 | stateID.x == 8 | stateID.x == 10) & !is.na(stateID.y)))
  #join to find sequential shots within the same point
  
  statePairTallies_type <- pairStates_type %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
    group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 16))# state pair totals
  
  
  curStatePairTallies_type <- pairStates_type %>% 
    group_by(stateID.x) %>% tally() %>%
    arrange(.by_group = TRUE)#Individual state tallies
  for(i in 1:15){
  if(!(i %in% curStatePairTallies_type$stateID.x)){
    curStatePairTallies_type <- curStatePairTallies_type %>% add_row(stateID.x =i,n=0)%>%arrange(stateID.x)
  }
  }
  
  #-------------------------------------------SET UP----------------------------------------------------------
  
  tpsLite1_type <- function(){
    M <- matrix(nrow = nrow(statesIDs_type)+1, ncol = nrow(statesIDs_type)+1)
    for(i in 0:nrow(curStatePairTallies_type)+1){
      for(j in 0:nrow(statesIDs_type)+1){
        if(nrow(statePairTallies_type %>% filter(stateID.x == i & stateID.y == j)) <1){
          M[i,j] <- 0
        }
        else{
          M[i,j] <- as.numeric((statePairTallies_type %>% filter(stateID.x == i & stateID.y == j) %>%
                                  ungroup() %>% select(n)))/as.numeric((curStatePairTallies_type %>% 
                                                                          filter(stateID.x == i) %>% select(n)))
        }
      }
    }
    return(M)
  } #create tranisition matrix
  
  
  TP_type <- tpsLite1_type()
  TP_type[16,16] <-1
  TP_type[16,]<-c(rep(0,15),1)
  rownames(TP_type)<- c('DSE', 'GE',
                        'LE', 'OE', 'VE',
                        'DSI', 'GI',
                        'LI', 'OI', 'VI',
                        'DSW', 'GW',
                        'LW', 'OW', 'VW', 'PE')
  colnames(TP_type)<- c('DSE', 'GE',
                        'LE', 'OE', 'VE',
                        'DSI', 'GI',
                        'LI', 'OI', 'VI',
                        'DSW', 'GW',
                        'LW', 'OW', 'VW', 'PE')
  
  return(TP_type)
}

