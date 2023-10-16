library(tidyverse)
#Group y Player, Gamestyle, Handedness, etc. 

#Point Winner or Loser?
#1stServe/2ndServe/Return/Rally
#ShotType
#Approach
#ShotSituation
#ShotLocation - grouped?

sbs[1,]

#Want a MatchID
#Chains for each point in each match
#Calculate tranisition pros

i<-669
allStates <- function(){
  tStateList <- list()
  for(i in 1:100){
    tStateList[[i]]<-c(sbs[i,'Point Index'], sbs[i,'Shot Index'], sbs[i,'Shot Outcome'],
                       sbs[i,'Shot Number'], sbs[i,'Shot Type'], sbs[i,'Approach'], sbs[i,'Situation'])
  }
  return(tStateList)
}

stateList <- allStates()
stateList

temp <- list()
temp[[1]] <- c(sbs[i,'Shot Outcome'], sbs[i,'Shot Number'], sbs[i,'Shot Type'],
               sbs[i,'Approach'], sbs[i,'Situation'])
temp[[2]] <- c(sbs[i,'Shot Outcome'], sbs[i,'Shot Number'], sbs[i,'Shot Type'],
               sbs[i,'Approach'], sbs[i,'Situation'])
print(temp)
