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
  ) #Filter data and reduce columns


states <- sts %>% select(c('Shot Outcome', 'Shot Number', 
                                   'Shot Type', 'Approach', 'Situation', 'Shot Stroke')) %>% distinct()


reducedOutcomes <- sts1 %>% mutate(`Shot Outcome` = case_when(`Shot Outcome` == 'Return Error' | 
                                                                `Shot Outcome` == 'Unforced Error' |
                                                                `Shot Outcome` == 'Forced Error' ~ 'Error', 
                                                              `Shot Outcome` == 'Return Winner' |
                                                                `Shot Outcome` == 'Winner'  ~ 'Winner',
                                                              `Shot Outcome` == 'In'  ~ 'In',)) %>%
  filter(`Shot Number`!= 'Serve') %>% select(c('Shot Outcome', 'Shot Stroke')) %>% distinct()


#HFHSOFUHUFGIUASUO
#Test other states, remove serves, simplify outcomes

statesLite1 <- sts1 %>% select(c('Shot Outcome', 'Shot Number', 'Shot Stroke')) %>% distinct() %>%
 group_by(`Shot Outcome`,`Shot Number`, `Shot Stroke`) %>% arrange(.by_group = TRUE) %>% 
  mutate(stateID=cur_group_id()) #gives each state a no. instead of using full state desc.

temp1 <- sts1 %>% select(c('Shot Index','Shot Outcome', 'Shot Number', 'Shot Stroke', 'ovrID')) %>%
  left_join(statesLite1, by=join_by(`Shot Outcome`, `Shot Number`, `Shot Stroke`)) %>%
  select(stateID, `Shot Index`, ovrID) #group each shot to find sequential points using a point ID

pairStatesLite <- temp1 %>% left_join(temp1, by=join_by(ovrID, closest('Shot Index'<'Shot Index'))) %>% 
  select(c('stateID.x', 'stateID.y')) #join to find sequential shots within the same point


statePairTallies <- pairStatesLite %>% group_by(stateID.x, stateID.y) %>% tally() %>% 
  group_by(stateID.x) %>% arrange(.by_group = TRUE) %>% replace_na(list(stateID.y = 19)) # state pair totals

curStatePairTallies <- pairStatesLite %>% 
  group_by(stateID.x) %>% tally() %>%
    arrange(.by_group = TRUE) #Individual state tallies

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
} #create tranisition matrix


TP<- tpsLite1()


