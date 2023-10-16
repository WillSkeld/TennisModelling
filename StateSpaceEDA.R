library(tidyverse)
library(dplyr)

sts <- sbs
sts$`Shot Number`[as.numeric(sts$`Shot Number`)>2] <- 'Rally'
sts1 <- sts %>% select(c('Date','Player','Opponent','Point Index', 'Shot Index','Shot Outcome', 'Shot Number', 
  'Shot Type', 'Approach', 'Situation', 'Shot Stroke')) %>% group_by(Date,Player,Opponent,`Point Index`) %>%
  mutate(ovrID=cur_group_id()) %>% ungroup() %>% filter(!(`Shot Outcome` == 'Double Fault' & `Shot Number` == 'Return') &
                                                        !(is.na(`Shot Outcome`) & `Shot Number` == 'Serve') & 
                                                          !(`Shot Outcome` == 'Out') & 
                                                          !(is.na(`Shot Stroke`) & !(`Shot Number` == 'Serve')) &
                                                          !(`Shot Outcome` == 'Return WInner') & !(`Shot Outcome` == 'WInner') &
                                                          !(is.na(`Shot Number`)) & 
                                                          !(`Shot Outcome` == 'Winner' &`Shot Number` == 'Return') &
                                                          !(`Shot Number`== 'Serve' & !is.na(`Shot Stroke`))
  )


states <- sts %>% select(c('Shot Outcome', 'Shot Number', 
                                   'Shot Type', 'Approach', 'Situation', 'Shot Stroke')) %>% distinct()
#Have list of states
#Want frequencies of each state
#Want frequencies of pairs of states
#Shot location parameters

statesLite1 <- sts1 %>% select(c('Shot Outcome', 'Shot Number', 'Shot Stroke')) %>% distinct() %>%
  group_by(`Shot Outcome`,`Shot Number`, `Shot Stroke`) %>% arrange(.by_group = TRUE) %>% 
  mutate(stateID=cur_group_id())

temp1 <- sts1 %>% select(c('Shot Index','Shot Outcome', 'Shot Number', 'Shot Stroke', 'ovrID')) %>%
  left_join(statesLite1, by=join_by(`Shot Outcome`, `Shot Number`, `Shot Stroke`)) %>%
  select(stateID, `Shot Index`, ovrID)



pairStatesLite <- temp1 %>% left_join(temp1, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
  select(c('stateID.x', 'stateID.y')) 


statePairTallies <- pairStatesLite %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
  group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 19))

curStatePairTallies <- pairStatesLite %>% 
  group_by(stateID.x) %>% tally() %>%
    arrange(.by_group = TRUE)

tpsLite1 <- function(){
  M <- matrix(nrow = nrow(statesLite1)+1, ncol = nrow(statesLite1)+1)
  for(i in 0:nrow(curStatePairTallies)+1){
    for(j in 0:nrow(statesLite1)+1){
      if(nrow(statePairTallies %>% filter(stateID.x == i & stateID.y == j)) <1){
          M[i,j] <- 0
      }
      else{
        M[i,j] <- as.numeric((statePairTallies %>% filter(stateID.x == i & stateID.y == j) %>%
                ungroup() %>% select(n)))/as.numeric((curStatePairTallies %>% 
                                               filter(stateID.x == i) %>% select(n)))
      }
        }
  }
  return(M)
}


TP<- tpsLite1()

(statePairTallies %>% filter(stateID.x == 1 & stateID.y == 40) %>% ungroup() %>%
    select(n))
M <- matrix(1,1)       
as.numeric((statePairTallies %>% filter(stateID.x == 1 & stateID.y == 40) %>%
             ungroup() %>% select(n))/(curStatePairTallies %>% 
                                         filter(stateID.x == 1) %>% select(n)))
nrow((statePairTallies %>% filter(stateID.x == 1 & stateID.y == 9) %>%
           ungroup() %>% select(n))) > 1

