library(tidyverse)
library(dplyr)

ptp <- pbp %>% drop_na(return_1_win, return_2_win) %>% drop_na(serve_1_win, serve_2_win) %>%
  filter(!(Date == "2021-02-10" & Player == "Nick Kyrgios" & Opponent == "Ugo Humbert") &
           !(Date == "2021-06-30" & Player == "Oscar Otte" & Opponent == "Andy Murray") &
           !(Date == "2021-08-25" & Player == "Gabriella Price" & Opponent == "Katie Boulter") &
           !(Date == "2022-03-17" & Player == "Nick Kyrgios" & Opponent == "Rafael Nadal") &
           !(Date == "2022-08-23" & Player == "Nuria Parrizas Diaz" & Opponent == "Harriet Dart" ) &
           !(is.na(`Opponent Gamestyle`)))

y<- ptp %>% group_by(Date) %>% count("Date")

serveGames <- ptp %>% select(c('Player', 'serve_total','serve_1_win','serve_2_win'))%>% group_by(Player) 
retGames <- ptp %>% select(c('Player', 'return_total','return_1_win','return_2_win'))%>% group_by(Player) 

servePts <- serveGames %>% summarise(
  serveWs = mean((serve_1_win +serve_2_win)/serve_total)
)
retPts <- retGames %>% summarise(
  retWs = mean((return_1_win +return_2_win)/return_total)
)

muFs<- servePts %>% summarise(across(where(is.numeric), mean)) #DataIssues
muFr<- retPts %>% summarise(across(where(is.numeric), mean)) #DataIssues

h2h<-function(P1 = "Roger Federer", P2 = "Novak Djokovic"){
  muS1 <- servePts %>% filter(Player == P1) %>% select(serveWs)
  muS2 <- servePts %>% filter(Player == P2) %>% select(serveWs)
  muR1 <- retPts %>% filter(Player == P1) %>% select(retWs)
  muR2 <- retPts %>% filter(Player == P2) %>% select(retWs)
  t_muR1<- muFr - muR1
  t_muR2<- muFr - muR2
  pS1_2 <- muS1 + t_muR2
  pS2_1 <- muS2 + t_muR1
  return(simh2hM(pS1_2, pS2_1))
}

h2h()

simh2hG<-function(pS1_2 = 0.5, pS2_1 = 0.5, server = 1){
  score1 = 0
  score2 = 0
  while(score1!=4 & score2!=4){
    if(score1==3 & score2==3){
      score1 <- score1-1
      score2 <- score2-1
    }
    if(server == 1){
      r <- runif(1)
      if(r<pS1_2){
        score1<- score1+1
      }
      else{
        score2<-score2+1
      }
    }
    else{
      r <- runif(1)
      if(r<pS2_1){
        score2<- score2+1
      }
      else{
        score1<-score1+1
      }
    }
  }
  if(score1>score2){
    return(1)
  }
  else{
    return(2)
  }
}
simh2hG()

simh2hS<- function(pS1_2 = 0.5, pS2_1 = 0.5, server = 1){
  sets1<-0
  sets2<-0
  while(sets1!=6 & sets2!=6){
    res<-simh2hG(pS1_2, pS2_1, server)
    if(res==1){
      sets1<- sets1+1
    }
    else{
      sets2<-sets2+1
    }
    server<-(server+1) %% 2
  }
  if(abs(sets1-sets2)>1){
  return(c(sets1,sets2))
  }
  else{
  res<-simh2hG(pS1_2, pS2_1, server)
  if(res==1){
    sets1<- sets1+1
  }
  else{
    sets2<-sets2+1
  }
  server<-(server+1) %% 2
  }
  
  if(sets1 != sets2){
    return(c(sets1,sets2))
  }
  else{
    res<-simh2hT(pS1_2, pS2_1)
    if(res==1){
      sets1<- sets1+1
    }
    else{
      sets2<-sets2+1
    }
    return(c(sets1,sets2))
  }
}

simh2hT<- function(pS1_2 = 0.5, pS2_1 = 0.5, server = 1){
  score1 = 0
  score2 = 0
  
  while(score1!=7 & score2!=7){
    if(score1==6 & score2==6){
      score1 <- score1-1
      score2 <- score2-1
    }
    if(server < 2){
      r <- runif(1)
      if(r<pS1_2){
        score1<- score1+1
      }
      else{
        score2<-score2+1
      }
    }
    else{
      r <- runif(1)
      if(r<pS2_1){
        score2<- score2+1
      }
      else{
        score1<-score1+1
      }
    }
    server <-server+1 %%4
  }
  if(score1>score2){
    return(1)
  }
  else{
    return(2)
  }
}
  
simh2hM <- function(pS1_2 = 0.5, pS2_1 = 0.5, to=3){
  sets1<-0
  sets2<-0
  server <- 1
  while(sets1 != to & sets2 != to){
    res<-simh2hS(pS1_2,pS2_1,server)
    if(res[1] > res[2]){
      sets1<- sets1+1
    }
    else{
      sets2<-sets2+1
    }
    server<- (server+1) %% 2
  }
  return(c(sets1,sets2))
}

  