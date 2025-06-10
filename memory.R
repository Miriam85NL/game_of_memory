rm(list=ls())

#This code simulates the duration of a game of memory under three different "strategies"
#number of cards (even)

n=20

#number of simulations
N=100

#Aantal_beurten= number of rounds played. in one round, a player can turn two cards.
#vec_aantal = vector of length N with the total number of rounds it took to omplete a game of memory


#strategy 1: random picking cards
#if there are still n_dicht cards on the table, the probability that the second one
#is equal to the first is 1/(n_dicht-1)

sim_willekeurig<-function(n,N){
vec_aantal<-rep(0,N)
for (m in 1:N){
  aantal_beurten <-0
  #n_dicht= number of cards not matched yet
  n_dicht<-n
  while(n_dicht>0){
    if(runif(1)<(1/(n_dicht-1))){n_dicht<-(n_dicht-2)}
    aantal_beurten <-aantal_beurten +1
  }  
vec_aantal[m]<-aantal_beurten
}
return(vec_aantal)
}

#optimaal strategy. Perfect memory.
#vec_kaartjes = random array of n cards, the "pictures" on the cards are 1,2,3...n/2
#n_omgedraaid = number of matched cards by the players. 
#vec_bekend=vector with vector with pictures/numbers of "seen" cards
#i: the ith card of vector vec_kaarten is turned over

sim_opt<-function(n,N){
  vec_aantal<-rep(0,N)
  for (m in 1:N){
    aantal_beurten <-0
    vec_kaartjes<-sample(rep(1:(n/2), each=2))
    n_omgedraaid<-0
    vec_bekend<-c()
    i<-1
   
    while(n_omgedraaid<n){
      #if vec_bekend contains two matching cards, turn them over
      if(any(duplicated(vec_bekend))==TRUE){
        n_omgedraaid<-n_omgedraaid+2
        vec_bekend <- vec_bekend[ !vec_bekend == vec_kaartjes[i-1]]
        aantal_beurten <-aantal_beurten +1
      } else{
        #turn a card over
        vec_bekend <-append(vec_bekend,vec_kaartjes[i])
        if(any(duplicated(vec_bekend))==TRUE){
          #if similar to a card that was already turned over, match the pair
          n_omgedraaid<-n_omgedraaid+2
          vec_bekend <- vec_bekend[ !vec_bekend == vec_kaartjes[i]]
          i<-i+1
          aantal_beurten <-aantal_beurten +1
          } else{
            #turn another card over
            vec_bekend <-append(vec_bekend,vec_kaartjes[i+1]) 
            if(vec_kaartjes[i+1]==vec_kaartjes[i])
            {
              n_omgedraaid<-n_omgedraaid+2 
              vec_bekend <- vec_bekend[ !vec_bekend == vec_kaartjes[i]]
            }
            i<-i+2
            aantal_beurten <-aantal_beurten +1
          }
      } 
    }
   vec_aantal[m]<-aantal_beurten
    }
  return(vec_aantal)
}


#strategy 3: only remember the last turn (last two cards)
#geheugen=0 if two cards where matched in the previous round.
#geheugen=1 if cards where not matched in the previous round 
# and therefore the pictures/numbers on 2 cards are known.
#if geheugen=0, it's similar to strategy 1
#if geheugen = 1 and there are 4 of 2 cards left, you cab make a match
#if geheugen = 1 and there are more than 4 cards left, probability that first new card is
#equal to one of those 2 is 2/(n_dicht-2)
#if that is not the case, the probability that the second card is equal to the first is 
#1/(n_dicht-3)


sim_kortgeheugen<-function(n,N){
  vec_aantal<-rep(0,N)
  for (m in 1:N){
    aantal_beurten <-0
    n_dicht<-n
    geheugen=0
    while(n_dicht>0){
      
      if(geheugen==0)
       if(runif(1)<(1/(n_dicht-1))){
         n_dicht<-(n_dicht-2)
         geheugen=0
       }else{
           geheugen=1
       }else{
      
      if(geheugen==1){
        if(n_dicht<=4){
          n_dicht<-(n_dicht-2)
          geheugen=0 
        }else{
        if(runif(1)<(2/(n_dicht-2)+(1-2/(n_dicht-2))*1/(n_dicht-3))){
          n_dicht<-(n_dicht-2)
          geheugen=0
        }else{
          geheugen=1
        }  
        }
      }
       }
      aantal_beurten <-aantal_beurten +1
    }  
    vec_aantal[m]<-aantal_beurten
  }
  return(vec_aantal)
}

#create a data.frame with results of simulations of three strategies
data<-data.frame(n=integer(),strategie=character(),aantal=integer())

for (n in seq(10,70,2)){
  N=1000
  data<-rbind(data, data.frame(n=n,strategie="optimaal",aantal=sim_opt(n,N)))
  data<-rbind(data, data.frame(n=n,strategie="willekeurig",aantal=sim_willekeurig(n,N)))
  data<-rbind(data, data.frame(n=n,strategie="kort geheugen",aantal=sim_kortgeheugen(n,N)))
}

#compute average and standard deviation per strategy
library(plyr)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(data, varname="aantal", 
                    groupnames=c("n", "strategie"))

#save and load
setwd("C:\\Users\\looic\\OneDrive - HvA\\Documents\\Documents\\blogs\\Memory")
save(df2, file = "data.Rdata")

load(file = "data.Rdata")

#plot results

library(ggplot2)

ggplot(df2, aes(x=n, y=aantal, group=strategie, color=strategie)) + 
  geom_pointrange(aes(ymin=aantal-sd, ymax=aantal+sd))+
  ggtitle("Duur van een potje memory (verwachting en standaarddeviatie)") +
  xlab("aantal kaarten") + ylab("aantal beurten")+
  theme_classic(base_size = 15)

