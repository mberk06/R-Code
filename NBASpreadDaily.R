# Date: Sping 2018
# Author: Michael Berk
# Description: This file was used to clean NBA data then forecast NBA spread 

#This will be used to calcualte nba team totals every day for NBA
#read in file
file = read.csv("/Users/michaelberk/Documents/NBA Data/2017-2018TeamTotals.txt",header = F,sep = "@")
names(file) <- c("TEAM","OPPTEAM","FGM","FGA","X3PM","X3PA","FTA","FTM","OREB","DREB","REB","AST","TOV","STL","BLK","PF","PTS","P.M","HOME","DATE")

#clean file
d <- file
d$DATE <- as.Date(d$DATE,format='%m/%d/%Y')

#make the columns numeric
d$FGM <- as.numeric(d$FGM)
d$FGA <- as.numeric(d$FGA)
d$X3PM <- as.numeric(d$X3PM)
d$X3PA <- as.numeric(d$X3PA)
d$FTM <- as.numeric(d$FTM)
d$FTA <- as.numeric(d$FTA)
d$OREB <- as.numeric(d$OREB)
d$DREB <- as.numeric(d$DREB)
d$REB <- as.numeric(d$REB)
d$AST <- as.numeric(d$AST)
d$TOV <- as.numeric(d$TOV)
d$STL <- as.numeric(d$STL)
d$BLK <- as.numeric(d$BLK)
d$PF <- as.numeric(d$PF)
d$PTS <- as.numeric(d$PTS)
d$P.M <- as.numeric(d$P.M)

#add empty averages
emptyCol <- rep(NA, dim(d)[1])
emptyDF <- data.frame(FGM.AVG=emptyCol,FGA.AVG=emptyCol,X3PM.AVG=emptyCol,X3PA.AVG=emptyCol,FTA.AVG=emptyCol,FTM.AVG=emptyCol,OREB.AVG=emptyCol,DREB.AVG=emptyCol,REB.AVG=emptyCol,AST.AVG=emptyCol,TOV.AVG=emptyCol,STL.AVG=emptyCol,BLK.AVG=emptyCol,PF.AVG=emptyCol,PTS.AVG=emptyCol,P.M.AVG=emptyCol)
d <- cbind(d, emptyDF)

#add empty gamesBack averages
emptyCol <- rep(NA, dim(d)[1])
emptyDF <- data.frame(FGM.AVG.5=emptyCol,FGA.AVG.5=emptyCol,X3PM.AVG.5=emptyCol,X3PA.AVG.5=emptyCol,FTA.AVG.5=emptyCol,FTM.AVG.5=emptyCol,OREB.AVG.5=emptyCol,DREB.AVG.5=emptyCol,REB.AVG.5=emptyCol,AST.AVG.5=emptyCol,TOV.AVG.5=emptyCol,STL.AVG.5=emptyCol,BLK.AVG.5=emptyCol,PF.AVG.5=emptyCol,PTS.AVG.5=emptyCol,P.M.AVG.5=emptyCol)
d <- cbind(d, emptyDF)

#order the dataframe by date
d <- d[order(as.Date(d$DATE, format="%m/%d/%Y")),]

#create ID number
ID <- 1:dim(d)[1]
d <- cbind(ID, d)

#calculate averages up to a date (not including the date)
#the gamesBack parameter uses data from "gamesBack" number of games inclusive
averages <- function(d, date, gamesBack) {
  #subset based on date
  sub <- subset(d, d$DATE <= date)
  eachTeam <- unique(sub$TEAM)
  
  for (t in eachTeam) {
    #subset based on team
    teamSub <- subset(sub, sub$TEAM == t)
    
    #get the row that will be changed (last row)
    rowID <- teamSub[dim(teamSub)[1],]$ID
    teamSub <- subset(teamSub, teamSub$ID != rowID)
    
    #average
    if (gamesBack > 1000) {
      #edit the d file
      d[d$ID == rowID,]$FGM.AVG <- mean(as.numeric(teamSub$FGM))
      d[d$ID == rowID,]$FGA.AVG <- mean(as.numeric(teamSub$FGA))
      d[d$ID == rowID,]$X3PM.AVG <- mean(as.numeric(teamSub$X3PM))
      d[d$ID == rowID,]$X3PA.AVG <- mean(as.numeric(teamSub$X3PA))
      d[d$ID == rowID,]$FTM.AVG <- mean(as.numeric(teamSub$FTM))
      d[d$ID == rowID,]$FTA.AVG <- mean(as.numeric(teamSub$FTA))
      d[d$ID == rowID,]$OREB.AVG <- mean(as.numeric(teamSub$OREB))
      d[d$ID == rowID,]$DREB.AVG <- mean(as.numeric(teamSub$DREB))
      d[d$ID == rowID,]$REB.AVG <- mean(as.numeric(teamSub$REB))
      d[d$ID == rowID,]$AST.AVG <- mean(as.numeric(teamSub$AST))
      d[d$ID == rowID,]$TOV.AVG <- mean(as.numeric(teamSub$TOV))
      d[d$ID == rowID,]$STL.AVG <- mean(as.numeric(teamSub$STL))
      d[d$ID == rowID,]$BLK.AVG <- mean(as.numeric(teamSub$BLK))
      d[d$ID == rowID,]$PF.AVG <- mean(as.numeric(teamSub$PF))
      d[d$ID == rowID,]$PTS.AVG <- mean(as.numeric(teamSub$PTS))
      d[d$ID == rowID,]$P.M.AVG <- mean(as.numeric(teamSub$P.M))
    }
    #5 games back average
    else {
      #include only "gamesBack" number of games + 1
      teamSub[-((gamesBack + 1):dim(teamSub)[1]),]
      
      #get the row that will be changed (last row)
      rowID <- teamSub[dim(teamSub)[1],]$ID
      teamSub <- subset(teamSub, teamSub$ID != rowID)    
      
      #edit the d file
      if (dim(teamSub)[1] > 5) {
        d[d$ID == rowID,]$FGM.AVG.5 <- mean(as.numeric(teamSub$FGM))
        d[d$ID == rowID,]$FGA.AVG.5 <- mean(as.numeric(teamSub$FGA))
        d[d$ID == rowID,]$X3PM.AVG.5 <- mean(as.numeric(teamSub$X3PM))
        d[d$ID == rowID,]$X3PA.AVG.5 <- mean(as.numeric(teamSub$X3PA))
        d[d$ID == rowID,]$FTM.AVG.5 <- mean(as.numeric(teamSub$FTM))
        d[d$ID == rowID,]$FTA.AVG.5 <- mean(as.numeric(teamSub$FTA))
        d[d$ID == rowID,]$OREB.AVG.5 <- mean(as.numeric(teamSub$OREB))
        d[d$ID == rowID,]$DREB.AVG.5 <- mean(as.numeric(teamSub$DREB))
        d[d$ID == rowID,]$REB.AVG.5 <- mean(as.numeric(teamSub$REB))
        d[d$ID == rowID,]$AST.AVG.5 <- mean(as.numeric(teamSub$AST))
        d[d$ID == rowID,]$TOV.AVG.5 <- mean(as.numeric(teamSub$TOV))
        d[d$ID == rowID,]$STL.AVG.5 <- mean(as.numeric(teamSub$STL))
        d[d$ID == rowID,]$BLK.AVG.5 <- mean(as.numeric(teamSub$BLK))
        d[d$ID == rowID,]$PF.AVG.5 <- mean(as.numeric(teamSub$PF))
        d[d$ID == rowID,]$PTS.AVG.5 <- mean(as.numeric(teamSub$PTS))
        d[d$ID == rowID,]$P.M.AVG.5 <- mean(as.numeric(teamSub$P.M))
      }
    }
  }
  return (d)
}

#run for all dates for all games and 5 games back
dates <- unique(d$DATE)
for (date in dates) {
  d <- averages(d, date, 5)
}

for (date in dates) {
  d <- averages(d, date, 999999)
}

#replace NaN with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
d[is.nan(d)] <- 0
d[is.na(d)] <- 0

################################################
#Add Jason's stats
#Add opponent performance
d <- merge(d, d, by.x = c("DATE","TEAM"), by.y = c("DATE","OPPTEAM"))
names(d) <- c("DATE","TEAM","ID.TEAM","OPPTEAM","FGM.TEAM","FGA.TEAM","X3PM.TEAM",
              "X3PA.TEAM","FTA.TEAM","FTM.TEAM","OREB.TEAM","DREB.TEAM","REB.TEAM","AST.TEAM",
              "TOV.TEAM","STL.TEAM","BLK.TEAM","PF.TEAM","PTS.TEAM","P.M.TEAM","HOME.TEAM",
              "FGM.AVG.TEAM","FGA.AVG.TEAM","X3PM.AVG.TEAM","X3PA.AVG.TEAM","FTA.AVG.TEAM","FTM.AVG.TEAM","OREB.AVG.TEAM",
              "DREB.AVG.TEAM","REB.AVG.TEAM","AST.AVG.TEAM","TOV.AVG.TEAM","STL.AVG.TEAM","BLK.AVG.TEAM","PF.AVG.TEAM",
              "PTS.AVG.TEAM","P.M.AVG.TEAM","FGM.AVG.5.TEAM","FGA.AVG.5.TEAM","X3PM.AVG.5.TEAM","X3PA.AVG.5.TEAM","FTA.AVG.5.TEAM",
              "FTM.AVG.5.TEAM","OREB.AVG.5.TEAM","DREB.AVG.5.TEAM","REB.AVG.5.TEAM","AST.AVG.5.TEAM","TOV.AVG.5.TEAM","STL.AVG.5.TEAM",
              "BLK.AVG.5.TEAM","PF.AVG.5.TEAM","PTS.AVG.5.TEAM","P.M.AVG.5.TEAM","ID.OPP","TEAM","FGM.OPP",
              "FGA.OPP","X3PM.OPP","X3PA.OPP","FTA.OPP","FTM.OPP","OREB.OPP","DREB.OPP",
              "REB.OPP","AST.OPP","TOV.OPP","STL.OPP","BLK.OPP","PF.OPP","PTS.OPP",
              "P.M.OPP","HOME.OPP","FGM.AVG.OPP","FGA.AVG.OPP","X3PM.AVG.OPP","X3PA.AVG.OPP","FTA.AVG.OPP",
              "FTM.AVG.OPP","OREB.AVG.OPP","DREB.AVG.OPP","REB.AVG.OPP","AST.AVG.OPP","TOV.AVG.OPP","STL.AVG.OPP",
              "BLK.AVG.OPP","PF.AVG.OPP","PTS.AVG.OPP","P.M.AVG.OPP","FGM.AVG.5.OPP","FGA.AVG.5.OPP","X3PM.AVG.5.OPP",
              "X3PA.AVG.5.OPP","FTA.AVG.5.OPP","FTM.AVG.5.OPP","OREB.AVG.5.OPP","DREB.AVG.5.OPP","REB.AVG.5.OPP","AST.AVG.5.OPP",
              "TOV.AVG.5.OPP","STL.AVG.5.OPP","BLK.AVG.5.OPP","PF.AVG.5.OPP","PTS.AVG.5.OPP","P.M.AVG.5.OPP")
d <- subset(d, select= -c(ID.OPP))

#calcualte spread
SPREAD <- d$PTS.TEAM - d$PTS.OPP
d <- cbind(d, SPREAD)

#Get Eff FG% and NRtg% 
EFF.FG.TEAM <- (d$FGM.AVG.TEAM + (0.5*d$X3PM.AVG.TEAM))/d$FGA.AVG.TEAM #clean
#NRtg needs to only include values from games back (use season averages)
PTS.ALLOWED.AVG <- d$PTS.AVG.TEAM - (d$PTS.AVG.TEAM - d$PTS.AVG.OPP)
POS.AVG <- 0.5 * ((d$FGA.AVG.TEAM + 0.4 * d$FTA.AVG.TEAM - 1.07 * (d$OREB.AVG.TEAM / (d$OREB.AVG.TEAM + d$DREB.AVG.OPP)) * (d$FGA.AVG.TEAM - d$FGM.AVG.TEAM) + d$TOV.AVG.TEAM) + 
                    (d$FGA.AVG.OPP + 0.4 * d$FTA.AVG.OPP - 1.07 * (d$OREB.AVG.OPP / (d$OREB.AVG.OPP + d$DREB.AVG.TEAM)) * (d$FGA.AVG.OPP - d$FGM.AVG.OPP) + d$TOV.AVG.OPP))     
POS.AVG <- ifelse(POS.AVG < 70, mean(POS.AVG),POS.AVG) #this line is necessary because formula for POS gavef values that were too low
OFF.RTG.AVG <- (d$PTS.AVG.TEAM / POS.AVG) * 100
DEF.RTG.AVG <- (PTS.ALLOWED.AVG / POS.AVG) * 100
NRtg.TEAM.AVG <- OFF.RTG.AVG - DEF.RTG.AVG
d <- cbind(d, EFF.FG.TEAM, NRtg.TEAM.AVG)

#replace NaN with NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
d[is.nan(d)] <- 0
d[is.na(d)] <- 0

write.csv(d, "/Users/michaelberk/Documents/NBA Data/2017-2018NBABoxScoresAverages.csv")

#######################################################
#Analyze File
#2016-17 season
older <- read.csv("/Users/michaelberk/Documents/NBA Data/2015-2016NBABoxScoresAverages1.csv")
old <- read.csv("/Users/michaelberk/Documents/NBA Data/2016-2017NBABoxScoresAverages1.csv")
new <- read.csv("/Users/michaelberk/Documents/NBA Data/2017-2018NBABoxScoresAverages.csv")
allOld <- NULL
allOld <- rbind(older, old)
avs <- NULL
avs <- cbind(PTS.TEAM = allOld$PTS.TEAM, PTS.OPP = allOld$PTS.OPP, allOld[23:54], allOld[73:104], allOld[106:107])
avs <- cbind(PTS.TEAM = allOld$PTS.TEAM, PTS.OPP = allOld$PTS.OPP, allOld[23:54], allOld[73:104], allOld[106:107])
nonZeroAvs <- subset(avs, avs$REB.AVG.5.TEAM != 0)
nonZeroAvs <- subset(nonZeroAvs, nonZeroAvs$FGM.AVG.5.OPP != 0)
nonZeroAvs <- subset(nonZeroAvs, nonZeroAvs$AST.AVG.5.TEAM != 0)
nonZeroAvs <- subset(nonZeroAvs, nonZeroAvs$TOV.AVG.5.TEAM != 0)
nonZeroAvs <- subset(nonZeroAvs, nonZeroAvs$STL.AVG.5.TEAM != 0)
newAvs <- NULL
newAvs <- cbind(PTS.TEAM = new$PTS.TEAM, PTS.OPP = new$PTS.OPP, new[23:54], new[73:104], new[106:107])
nonZeroNew <- subset(newAvs, newAvs$REB.AVG.5.TEAM != 0)
nonZeroNew <- subset(nonZeroNew, nonZeroNew$FGM.AVG.5.OPP != 0)
nonZeroNew <- subset(nonZeroNew, nonZeroNew$AST.AVG.5.TEAM != 0)
nonZeroNew <- subset(nonZeroNew, nonZeroNew$TOV.AVG.5.TEAM != 0)
nonZeroNew <- subset(nonZeroNew, nonZeroNew$STL.AVG.5.TEAM != 0)
all <- NULL
all <- rbind(old, older, new)
#######################################################


#########################
#########################
#########################
#Linear Models
#6 var
six <- lm(SPREAD ~ FGM.TEAM + X3PM.TEAM + FTA.TEAM + FGM.OPP + X3PM.OPP + FTA.OPP, data = old)

#7 var
seven <- lm(SPREAD ~ FGM.TEAM + X3PM.TEAM + FTA.TEAM + DREB.TEAM + FGM.OPP + X3PM.OPP + FTA.OPP, data = old)

#8 var
eight <- lm(SPREAD ~ FGM.TEAM+FGA.TEAM+X3PM.TEAM+FTA.TEAM+TOV.TEAM+FGM.OPP+X3PM.OPP+FTA.OPP, data = old)

#########################
#reset general frame
newPred <- data.frame(TEAM = as.character(new$TEAM), OPPTEAM = as.character(new$OPPTEAM), SPREAD = new$SPREAD)
#########################
#6 var
predSix <- predict(six, newdata = new)
newPred <- cbind(newPred, PRED = predSix)
cor(newPred$PRED, newPred$SPREAD) #1

#7 var
predSeven <- predict(seven, newdata = new)
newPred <- cbind(newPred, PRED = predSeven)
cor(newPred$PRED, newPred$SPREAD) #1

#8 var
predEight <- predict(eight, newdata = new)
newPred <- cbind(newPred, PRED = predEight)
cor(newPred$PRED, newPred$SPREAD) #1

#########################
#Linear Models using backwards selection
#manual backwards selection
back <- lm(SPREAD ~ FGM.TEAM + FGA.TEAM + X3PM.TEAM + FTA.TEAM + TOV.TEAM + FGM.OPP + X3PM.OPP + FTA.OPP, data = old)

#exhaustive backwards, forwards, and seach (all 3 methods gave same model)
search <- lm(SPREAD ~ FGM.TEAM + X3PM.TEAM + FTA.TEAM + FGM.OPP + X3PM.OPP + FTA.OPP, data = old)

#Lasso/Elastic Net method (both gave the same model)
lasso.model <- lm(SPREAD ~ FGM.TEAM + X3PM.TEAM + FTA.TEAM + DREB.TEAM + FGM.OPP + X3PM.OPP + FTA.OPP + DREB.OPP,
                  data = old)

#########################
#reset general frame
newPred <- data.frame(TEAM = as.character(new$TEAM), OPPTEAM = as.character(new$OPPTEAM), SPREAD = new$SPREAD)
#########################

#manual backwards selection
predBack <- predict(back, newdata = new)
newPred <- cbind(newPred, PRED = predBack)
cor(newPred$PRED, newPred$SPREAD) #1

#exhaustive backwards, forwards, and seach
predSearch <- predict(search, newdata = new)
newPred <- cbind(newPred, PRED = predSearch)
cor(newPred$PRED, newPred$SPREAD) #1

#Lasso/Elastic Net method
predLasso <- predict(lasso.model, newdata = new)
newPred <- cbind(newPred, PRED = predLasso)
cor(newPred$PRED, newPred$SPREAD) #1

#######################
#Random Forests
require(randomForest)
set.seed(101)

#random forests model (best number is 25 or 21)
rf <- randomForest(SPREAD ~ FGM.TEAM+FGA.TEAM+X3PM.TEAM+X3PA.TEAM+FTA.TEAM+FTM.TEAM+OREB.TEAM+DREB.TEAM+AST.TEAM+
                     TOV.TEAM+STL.TEAM+BLK.TEAM+PF.TEAM+HOME.TEAM+FGM.OPP+FGA.OPP+X3PM.OPP+X3PA.OPP+FTA.OPP+FTM.OPP+
                     OREB.OPP+DREB.OPP+AST.OPP+TOV.OPP+STL.OPP+BLK.OPP+PF.OPP, data = old, mtry = 25)

#######################
#######################
#######################
#Find what predictors predcict the SPREAD predictors
#TWO VAR MODEL (PTS.TEAM, PTS.OPP) - they should have the same model because of te symmetry of the data

#######################################################
#regsubsets 
#is innacurate because of linear dependencies (should probably use random forest)
#ran it with "forward" selection instead of "exhaustive" selection to avoid linear dependencies (cannot use exhaustive)
library('leaps')
library('mgcv')
#FORWARD SELECTION
d <- nonZeroAvs[!(names(nonZeroAvs) %in% c("PTS.OPP","REB.AVG.TEAM","REB.AVG.5.TEAM","REB.AVG.OPP","REB.AVG.5.OPP","FGA.AVG.5.TEAM",
                             "X3PM.AVG.5.TEAM","DREB.AVG.5.TEAM","TOV.AVG.5.TEAM","PTS.AVG.5.TEAM","P.M.AVG.5.TEAM",
                             "AST.AVG.5.OPP","TOV.AVG.5.OPP","PTS.AVG.5.OPP","PTS.AVG.TEAM","PTS.AVG.OPP","FGM.AVG.5.TEAM",
                             "X3PA.AVG.5.TEAM","FTA.AVG.5.TEAM","FTM.AVG.5.TEAM","OREB.AVG.5.TEAM","AST.AVG.5.TEAM","STL.AVG.5.TEAM",
                             "BLK.AVG.5.TEAM","PF.AVG.5.TEAM","FGM.AVG.5.OPP","FGA.AVG.5.OPP","X3PM.AVG.5.OPP","X3PA.AVG.5.OPP"
                             ,"FTA.AVG.5.OPP","FTM.AVG.5.OPP","OREB.AVG.5.OPP","DREB.AVG.5.OPP","STL.AVG.5.OPP","BLK.AVG.5.OPP"
                             ,"PF.AVG.5.OPP","P.M.AVG.5.OPP"))]
reg <- regsubsets(PTS.TEAM ~., d, nvmax=66, method="forward")
sumReg <- summary(reg)
#graphics
par(mfrow=c(3,1), mar=c(2.5,4,0.5,1), mgp=c(1.5,0.5,0))     
par(mfrow=c(1,1))
plot(sumReg$cp, xlab="Number of predictors", 
     ylab="cp", col="red", type="p", pch=16)
plot(sumReg$bic, xlab="Number of predictors", 
     ylab="bic", col="blue", type="p", pch=16)
plot(sumReg$adjr2, xlab="Number of predictors", 
     ylab="adjr2", col="green", type="p", pch=16)
#select the model based on minimum cp
opt.size <- which.min(sumReg$cp) #31
opt.vars <- sumReg$which 
opt.vars <- opt.vars[opt.size,] #model
opt.vars <- opt.vars[opt.vars == T]
#lm
m <- lm(PTS.TEAM ~ FGM.AVG.TEAM+FGA.AVG.TEAM+X3PM.AVG.TEAM+X3PA.AVG.TEAM+FTA.AVG.TEAM+FTM.AVG.TEAM+OREB.AVG.TEAM+
          DREB.AVG.TEAM+AST.AVG.TEAM+BLK.AVG.TEAM+P.M.AVG.TEAM+FGM.AVG.OPP+X3PM.AVG.OPP+OREB.AVG.OPP+DREB.AVG.OPP+
          AST.AVG.OPP+BLK.AVG.OPP+P.M.AVG.OPP+EFF.FG.TEAM+NRtg.TEAM.AVG, data = d)
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1230851
#gam
m <- gam(PTS.TEAM~s(FGM.AVG.TEAM)+s(FGA.AVG.TEAM)+s(X3PM.AVG.TEAM)+s(X3PA.AVG.TEAM)+s(FTA.AVG.TEAM)+s(FTM.AVG.TEAM)+s(OREB.AVG.TEAM)+s(
  DREB.AVG.TEAM)+s(AST.AVG.TEAM)+s(BLK.AVG.TEAM)+s(P.M.AVG.TEAM)+s(FGM.AVG.OPP)+s(X3PM.AVG.OPP)+s(OREB.AVG.OPP)+s(DREB.AVG.OPP)+s(
    AST.AVG.OPP)+s(BLK.AVG.OPP)+s(P.M.AVG.OPP)+s(EFF.FG.TEAM)+s(NRtg.TEAM.AVG), data = d, family="gaussian")
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1228489

#BACKWARD SELECTION
d <- nonZeroAvs[!(names(nonZeroAvs) %in% c("PTS.OPP","REB.AVG.TEAM","REB.AVG.5.TEAM","REB.AVG.OPP","REB.AVG.5.OPP","FGA.AVG.5.TEAM",
                             "X3PM.AVG.5.TEAM","DREB.AVG.5.TEAM","TOV.AVG.5.TEAM","PTS.AVG.5.TEAM","P.M.AVG.5.TEAM",
                             "AST.AVG.5.OPP","TOV.AVG.5.OPP","PTS.AVG.5.OPP","PTS.AVG.TEAM","PTS.AVG.OPP","FGM.AVG.5.TEAM",
                             "X3PA.AVG.5.TEAM","FTA.AVG.5.TEAM","FTM.AVG.5.TEAM","OREB.AVG.5.TEAM","AST.AVG.5.TEAM","STL.AVG.5.TEAM",
                             "BLK.AVG.5.TEAM","PF.AVG.5.TEAM","FGM.AVG.5.OPP","FGA.AVG.5.OPP","X3PM.AVG.5.OPP","X3PA.AVG.5.OPP"
                             ,"FTA.AVG.5.OPP","FTM.AVG.5.OPP","OREB.AVG.5.OPP","DREB.AVG.5.OPP","STL.AVG.5.OPP","BLK.AVG.5.OPP"
                             ,"PF.AVG.5.OPP","P.M.AVG.5.OPP"))]
reg <- regsubsets(PTS.TEAM ~., d, nvmax=66, method="backward")
sumReg <- summary(reg)
#graphics
par(mfrow=c(3,1), mar=c(2.5,4,0.5,1), mgp=c(1.5,0.5,0))     
par(mfrow=c(1,1))
plot(sumReg$cp, xlab="Number of predictors", 
     ylab="cp", col="red", type="p", pch=16)
plot(sumReg$bic, xlab="Number of predictors", 
     ylab="bic", col="blue", type="p", pch=16)
plot(sumReg$adjr2, xlab="Number of predictors", 
     ylab="adjr2", col="green", type="p", pch=16)
#select the model based on minimum cp
opt.size <- which.min(sumReg$cp) #31
opt.vars <- sumReg$which 
opt.vars <- opt.vars[opt.size,] #model
opt.vars <- opt.vars[opt.vars == T]
#lm
m <- lm(PTS.TEAM ~ FGM.AVG.TEAM+FGA.AVG.TEAM+X3PM.AVG.TEAM+X3PA.AVG.TEAM+FTA.AVG.TEAM+FTM.AVG.TEAM+OREB.AVG.TEAM+
          DREB.AVG.TEAM+AST.AVG.TEAM+BLK.AVG.TEAM+FGM.AVG.OPP+FGA.AVG.OPP+X3PM.AVG.OPP+X3PA.AVG.OPP+FTA.AVG.OPP+
          FTM.AVG.OPP+TOV.AVG.OPP+BLK.AVG.OPP+PF.AVG.OPP+P.M.AVG.OPP+EFF.FG.TEAM+NRtg.TEAM.AVG, data = d)
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.119996
#gam
m <- gam(PTS.TEAM~s(FGM.AVG.TEAM)+s(FGA.AVG.TEAM)+s(X3PM.AVG.TEAM)+s(X3PA.AVG.TEAM)+s(FTA.AVG.TEAM)+s(FTM.AVG.TEAM)+s(OREB.AVG.TEAM)+s(
  DREB.AVG.TEAM)+s(AST.AVG.TEAM)+s(BLK.AVG.TEAM)+s(FGM.AVG.OPP)+s(FGA.AVG.OPP)+s(X3PM.AVG.OPP)+s(X3PA.AVG.OPP)+s(FTA.AVG.OPP)+s(
    FTM.AVG.OPP)+s(TOV.AVG.OPP)+s(BLK.AVG.OPP)+s(PF.AVG.OPP)+s(P.M.AVG.OPP)+s(EFF.FG.TEAM)+s(NRtg.TEAM.AVG), data = d, family="gaussian")
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1248782

#######################################################
#lasso
library('glmnet')

#create data and matrix
d <- nonZeroAvs[!(names(nonZeroAvs) %in% c("PTS.OPP","REB.AVG.TEAM","REB.AVG.5.TEAM","REB.AVG.OPP","REB.AVG.5.OPP"))]
Y <- d$PTS.TEAM #response
X <- as.matrix(d[,-1])

#get optimal lambda
cv <- cv.glmnet(X, Y)
plot(cv) #min = -6.15, max = -1.9
lam <- exp(-4) 

#get variables (using alpha = 1)
l <- glmnet(X, Y, alpha=1, family="gaussian", lambda = lam)
vars <- l$beta[,1][l$beta[,1] != 0]
vars <- vars > -100 #get rid of nums

#linear model LASSO
m <- lm(PTS.TEAM ~ FGM.AVG.TEAM+FGA.AVG.TEAM+X3PM.AVG.TEAM+X3PA.AVG.TEAM+FTM.AVG.TEAM+OREB.AVG.TEAM+DREB.AVG.TEAM+
          AST.AVG.TEAM+TOV.AVG.TEAM+BLK.AVG.TEAM+PF.AVG.TEAM+PTS.AVG.TEAM+P.M.AVG.TEAM+FGM.AVG.5.TEAM+
          FGA.AVG.5.TEAM+X3PM.AVG.5.TEAM+FTA.AVG.5.TEAM+OREB.AVG.5.TEAM+DREB.AVG.5.TEAM+AST.AVG.5.TEAM+TOV.AVG.5.TEAM+
          STL.AVG.5.TEAM+PTS.AVG.5.TEAM+P.M.AVG.5.TEAM+FGM.AVG.OPP+FGA.AVG.OPP+X3PM.AVG.OPP+FTA.AVG.OPP+
          FTM.AVG.OPP+OREB.AVG.OPP+AST.AVG.OPP+TOV.AVG.OPP+STL.AVG.OPP+BLK.AVG.OPP+PF.AVG.OPP+
          P.M.AVG.OPP+X3PM.AVG.5.OPP+FTA.AVG.5.OPP+OREB.AVG.5.OPP+TOV.AVG.5.OPP+BLK.AVG.5.OPP+PF.AVG.5.OPP+
          P.M.AVG.5.OPP+NRtg.TEAM.AVG, data = d)
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1283977
#gam
m <- gam(PTS.TEAM~s(FGM.AVG.TEAM)+s(FGA.AVG.TEAM)+s(X3PM.AVG.TEAM)+s(X3PA.AVG.TEAM)+s(FTM.AVG.TEAM)+s(OREB.AVG.TEAM)+s(DREB.AVG.TEAM)+s(
  AST.AVG.TEAM)+s(TOV.AVG.TEAM)+s(BLK.AVG.TEAM)+s(PF.AVG.TEAM)+s(PTS.AVG.TEAM)+s(P.M.AVG.TEAM)+s(FGM.AVG.5.TEAM)+s(
    FGA.AVG.5.TEAM)+s(X3PM.AVG.5.TEAM)+s(FTA.AVG.5.TEAM)+s(OREB.AVG.5.TEAM)+s(DREB.AVG.5.TEAM)+s(AST.AVG.5.TEAM)+s(TOV.AVG.5.TEAM)+s(
      STL.AVG.5.TEAM)+s(PTS.AVG.5.TEAM)+s(P.M.AVG.5.TEAM)+s(FGM.AVG.OPP)+s(FGA.AVG.OPP)+s(X3PM.AVG.OPP)+s(FTA.AVG.OPP)+s(
        FTM.AVG.OPP)+s(OREB.AVG.OPP)+s(AST.AVG.OPP)+s(TOV.AVG.OPP)+s(STL.AVG.OPP)+s(BLK.AVG.OPP)+s(PF.AVG.OPP)+s(
          P.M.AVG.OPP)+s(X3PM.AVG.5.OPP)+s(FTA.AVG.5.OPP)+s(OREB.AVG.5.OPP)+s(TOV.AVG.5.OPP)+s(BLK.AVG.5.OPP)+s(PF.AVG.5.OPP)+s(
            P.M.AVG.5.OPP)+s(NRtg.TEAM.AVG), data = d, family="gaussian")
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1014775

#get variables (using alpha = 0.5) ELASTIC NET
l <- glmnet(X, Y, alpha=.5, family="gaussian", lambda = lam)
vars <- l$beta[,1][l$beta[,1] != 0]
vars <- vars > -10000 #get rid of nums

#lm
m <- lm(PTS.TEAM ~ FGM.AVG.TEAM+FGA.AVG.TEAM+X3PM.AVG.TEAM+X3PA.AVG.TEAM+FTA.AVG.TEAM+FTM.AVG.TEAM+OREB.AVG.TEAM+
          DREB.AVG.TEAM+AST.AVG.TEAM+BLK.AVG.TEAM+PF.AVG.TEAM+PTS.AVG.TEAM+P.M.AVG.TEAM+FGM.AVG.5.TEAM+
          FGA.AVG.5.TEAM+X3PM.AVG.5.TEAM+FTA.AVG.5.TEAM+FTM.AVG.5.TEAM+OREB.AVG.5.TEAM+DREB.AVG.5.TEAM+AST.AVG.5.TEAM+
          TOV.AVG.5.TEAM+STL.AVG.5.TEAM+BLK.AVG.5.TEAM+PTS.AVG.5.TEAM+P.M.AVG.5.TEAM+FGM.AVG.OPP+FGA.AVG.OPP+
          X3PM.AVG.OPP+X3PA.AVG.OPP+FTA.AVG.OPP+FTM.AVG.OPP+DREB.AVG.OPP+AST.AVG.OPP+STL.AVG.OPP+
          BLK.AVG.OPP+PF.AVG.OPP+PTS.AVG.OPP+P.M.AVG.OPP+FGM.AVG.5.OPP+FTA.AVG.5.OPP+OREB.AVG.5.OPP+
          TOV.AVG.5.OPP+BLK.AVG.5.OPP+PF.AVG.5.OPP+PTS.AVG.5.OPP+P.M.AVG.5.OPP+EFF.FG.TEAM+NRtg.TEAM.AVG, data = d)
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1170803
#gam
m <- gam(PTS.TEAM~s(FGM.AVG.TEAM)+s(FGA.AVG.TEAM)+s(X3PM.AVG.TEAM)+s(X3PA.AVG.TEAM)+s(FTA.AVG.TEAM)+s(FTM.AVG.TEAM)+s(OREB.AVG.TEAM)+s(
  DREB.AVG.TEAM)+s(AST.AVG.TEAM)+s(BLK.AVG.TEAM)+s(PF.AVG.TEAM)+s(PTS.AVG.TEAM)+s(P.M.AVG.TEAM)+s(FGM.AVG.5.TEAM)+s(
    FGA.AVG.5.TEAM)+s(X3PM.AVG.5.TEAM)+s(FTA.AVG.5.TEAM)+s(FTM.AVG.5.TEAM)+s(OREB.AVG.5.TEAM)+s(DREB.AVG.5.TEAM)+s(AST.AVG.5.TEAM)+s(
      TOV.AVG.5.TEAM)+s(STL.AVG.5.TEAM)+s(BLK.AVG.5.TEAM)+s(PTS.AVG.5.TEAM)+s(P.M.AVG.5.TEAM)+s(FGM.AVG.OPP)+s(FGA.AVG.OPP)+s(
        X3PM.AVG.OPP)+s(X3PA.AVG.OPP)+s(FTA.AVG.OPP)+s(FTM.AVG.OPP)+s(DREB.AVG.OPP)+s(AST.AVG.OPP)+s(STL.AVG.OPP)+s(
          BLK.AVG.OPP)+s(PF.AVG.OPP)+s(PTS.AVG.OPP)+s(P.M.AVG.OPP)+s(FGM.AVG.5.OPP)+s(FTA.AVG.5.OPP)+s(OREB.AVG.5.OPP)+s(
            TOV.AVG.5.OPP)+s(BLK.AVG.5.OPP)+s(PF.AVG.5.OPP)+s(PTS.AVG.5.OPP)+s(P.M.AVG.5.OPP)+s(EFF.FG.TEAM)+s(NRtg.TEAM.AVG), data = d, family="gaussian")
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1002219

#get variables (using alpha = 0) RIDGE REGRESSION - less good
l <- glmnet(X, Y, alpha=0, family="gaussian", lambda = lam)
vars <- l$beta[,1][l$beta[,1] != 0]
vars <- vars > -1000000 #get rid of nums

m <- lm(PTS.TEAM ~ FGM.AVG.TEAM+FGA.AVG.TEAM+X3PM.AVG.TEAM+X3PA.AVG.TEAM+FTA.AVG.TEAM+FTM.AVG.TEAM+OREB.AVG.TEAM+
          DREB.AVG.TEAM+AST.AVG.TEAM+TOV.AVG.TEAM+STL.AVG.TEAM+BLK.AVG.TEAM+PF.AVG.TEAM+PTS.AVG.TEAM+
          P.M.AVG.TEAM+FGM.AVG.5.TEAM+FGA.AVG.5.TEAM+X3PM.AVG.5.TEAM+X3PA.AVG.5.TEAM+FTA.AVG.5.TEAM+FTM.AVG.5.TEAM+
          OREB.AVG.5.TEAM+DREB.AVG.5.TEAM+AST.AVG.5.TEAM+TOV.AVG.5.TEAM+STL.AVG.5.TEAM+BLK.AVG.5.TEAM+PF.AVG.5.TEAM+
          PTS.AVG.5.TEAM+P.M.AVG.5.TEAM+FGM.AVG.OPP+FGA.AVG.OPP+X3PM.AVG.OPP+X3PA.AVG.OPP+FTA.AVG.OPP+
          FTM.AVG.OPP+OREB.AVG.OPP+DREB.AVG.OPP+AST.AVG.OPP+TOV.AVG.OPP+STL.AVG.OPP+BLK.AVG.OPP+
          PF.AVG.OPP+PTS.AVG.OPP+P.M.AVG.OPP+FGM.AVG.5.OPP+FGA.AVG.5.OPP+X3PM.AVG.5.OPP+X3PA.AVG.5.OPP+
          FTA.AVG.5.OPP+FTM.AVG.5.OPP+OREB.AVG.5.OPP+DREB.AVG.5.OPP+AST.AVG.5.OPP+TOV.AVG.5.OPP+STL.AVG.5.OPP+
          BLK.AVG.5.OPP+PF.AVG.5.OPP+PTS.AVG.5.OPP+P.M.AVG.5.OPP+EFF.FG.TEAM+NRtg.TEAM.AVG, data = d)
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred,nonZeroNew$PTS.TEAM)^2 #0.1170803
#gam
m <- gam(PTS.TEAM~s(FGM.AVG.TEAM)+s(FGA.AVG.TEAM)+s(X3PM.AVG.TEAM)+s(X3PA.AVG.TEAM)+s(FTA.AVG.TEAM)+s(FTM.AVG.TEAM)+s(OREB.AVG.TEAM)+s(
  DREB.AVG.TEAM)+s(AST.AVG.TEAM)+s(TOV.AVG.TEAM)+s(STL.AVG.TEAM)+s(BLK.AVG.TEAM)+s(PF.AVG.TEAM)+s(PTS.AVG.TEAM)+s(
    P.M.AVG.TEAM)+s(FGM.AVG.5.TEAM)+s(FGA.AVG.5.TEAM)+s(X3PM.AVG.5.TEAM)+s(X3PA.AVG.5.TEAM)+s(FTA.AVG.5.TEAM)+s(FTM.AVG.5.TEAM)+s(
      OREB.AVG.5.TEAM)+s(DREB.AVG.5.TEAM)+s(AST.AVG.5.TEAM)+s(TOV.AVG.5.TEAM)+s(STL.AVG.5.TEAM)+s(BLK.AVG.5.TEAM)+s(PF.AVG.5.TEAM)+s(
        PTS.AVG.5.TEAM)+s(P.M.AVG.5.TEAM)+s(FGM.AVG.OPP)+s(FGA.AVG.OPP)+s(X3PM.AVG.OPP)+s(X3PA.AVG.OPP)+s(FTA.AVG.OPP)+s(
          FTM.AVG.OPP)+s(OREB.AVG.OPP)+s(DREB.AVG.OPP)+s(AST.AVG.OPP)+s(TOV.AVG.OPP)+s(STL.AVG.OPP)+s(BLK.AVG.OPP)+s(
            PF.AVG.OPP)+s(PTS.AVG.OPP)+s(P.M.AVG.OPP)+s(FGM.AVG.5.OPP)+s(FGA.AVG.5.OPP)+s(X3PM.AVG.5.OPP)+s(X3PA.AVG.5.OPP)+s(
              FTA.AVG.5.OPP)+s(FTM.AVG.5.OPP)+s(OREB.AVG.5.OPP)+s(DREB.AVG.5.OPP)+s(AST.AVG.5.OPP)+s(TOV.AVG.5.OPP)+s(STL.AVG.5.OPP)+s(
                BLK.AVG.5.OPP)+s(PF.AVG.5.OPP)+s(PTS.AVG.5.OPP)+s(P.M.AVG.5.OPP)+s(EFF.FG.TEAM)+s(NRtg.TEAM.AVG), data = d, family="gaussian")
pred <- predict(m, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1004316

##############################
#Random Forest
library(randomForest)

#set up the data
d <- nonZeroAvs[!(names(nonZeroAvs) %in% c("PTS.OPP"))]

#run RF and get best ntree
rf <- randomForest(PTS.TEAM ~. -PTS.TEAM, data = d, mtry = 5, ntree = 1000)
plot(rf) 
bestNumTrees <- 400
rf <- randomForest(PTS.TEAM ~. -PTS.TEAM, data = d, mtry = 5, ntree = bestNumTrees, nodesize = 3) 
rf$importance 
varImpPlot(rf)
partialPlot(rf,d,PTS.TEAM)

#predict
pred <- predict(rf, newdata = nonZeroNew)
c <- cor(pred, nonZeroNew$PTS.TEAM)^2 #0.1036931

#test most recent season
newest <- new[650:dim(new)[1],]
pred <- predict(rf, newdata = newest)
c <- cor(pred, newest$PTS.TEAM)^2 #0.1163076

##############################
#GAM
library('mgcv')

#set up d
d <- nonZeroAvs[!(names(nonZeroAvs) %in% c("PTS.OPP"))]

#model
m <- gam(PTS.TEAM~s(FGM.AVG.TEAM)+s(FGA.AVG.TEAM)+s(X3PM.AVG.TEAM)+s(X3PA.AVG.TEAM)+s(OREB.AVG.TEAM)+s(DREB.AVG.TEAM)+
           s(AST.AVG.TEAM)+s(TOV.AVG.TEAM)+s(BLK.AVG.TEAM)+s(PTS.AVG.TEAM)+s(P.M.AVG.TEAM)+s(FGA.AVG.5.TEAM)+s(X3PM.AVG.5.TEAM)+
           s(DREB.AVG.5.TEAM)+s(TOV.AVG.5.TEAM)+s(PTS.AVG.5.TEAM)+s(P.M.AVG.5.TEAM)+s(X3PM.AVG.OPP)+s(AST.AVG.OPP)+s(TOV.AVG.OPP)+
           s(P.M.AVG.OPP)+s(FGA.AVG.5.OPP)+s(OREB.AVG.5.OPP)+s(DREB.AVG.5.OPP)+s(AST.AVG.5.OPP)+s(TOV.AVG.5.OPP)+s(STL.AVG.5.OPP)+
           s(BLK.AVG.5.OPP)+s(PF.AVG.5.OPP)+s(PTS.AVG.5.OPP)+s(P.M.AVG.5.OPP), data = d, family="gaussian")

#predict
pred <- predict(m, newdata = new)
c <- cor(pred, new$PTS.TEAM)^2 #0.08718485


##############################
#Vif
library(car)

d <- nonZeroAvs[!(names(nonZeroAvs) %in% c("PTS.OPP","REB.AVG.TEAM","REB.AVG.5.TEAM","REB.AVG.OPP","REB.AVG.5.OPP","FGA.AVG.5.TEAM",
                             "X3PM.AVG.5.TEAM","DREB.AVG.5.TEAM","TOV.AVG.5.TEAM","PTS.AVG.5.TEAM","P.M.AVG.5.TEAM",
                             "AST.AVG.5.OPP","TOV.AVG.5.OPP","PTS.AVG.5.OPP","PTS.AVG.TEAM","PTS.AVG.OPP","FGM.AVG.5.TEAM",
                             "X3PA.AVG.5.TEAM","FTA.AVG.5.TEAM","FTM.AVG.5.TEAM","OREB.AVG.5.TEAM","AST.AVG.5.TEAM","STL.AVG.5.TEAM",
                             "BLK.AVG.5.TEAM","PF.AVG.5.TEAM","FGM.AVG.5.OPP","FGA.AVG.5.OPP","X3PM.AVG.5.OPP","X3PA.AVG.5.OPP"
                             ,"FTA.AVG.5.OPP","FTM.AVG.5.OPP","OREB.AVG.5.OPP","DREB.AVG.5.OPP","STL.AVG.5.OPP","BLK.AVG.5.OPP"
                             ,"PF.AVG.5.OPP","P.M.AVG.5.OPP"))]
m <- lm(PTS.TEAM ~. , data = d)

v <- vif(m)
##############################
#boosted binomial regression
library(gbm)
#Boosting splits up the dataframes based on best splits of a category
#The first tree uses the data, then the next trees use the residuals of the best fit line
#Agurments:
#1. Formula
#2. data = dataFrame
#3. n.trees = how many trees (number of iterations) - put a big number for now
#4. interaction.depth = number of levels in the tree
#5. n.minobsinnode = the minimum number of observations allowed in a single terminal node
#6. shrinkage = how much wieght is given to each new tree (don't change)
#7. bag.fraction = sample size of each new tree in percentage
out2<-gbm(PTS.TEAM ~.,
          data=d,n.trees=4000,interaction.depth=3,
          n.minobsinnode = 25,shrinkage=.001,bag.fraction=0.5,
          n.cores=1,distribution = list(name="quantile",
                                        alpha=0.25))

#graphs the best number of iterations and outputs the number
best.iter <- gbm.perf(out2, method = "OOB")

#prediction of points for each player with number of trees found above
pred <- predict(out2, new, n.trees = best.iter)
#plot the prediction with points
plot(pred, new$PTS.TEAM)
c <- cor(pred, new$PTS.TEAM)^2

#get a linear model for the predicted amount of points against the actual points scored (see how well you predicted)
reg <- lm(d$DKLG~prediction)
#put line of best fit
abline(-7.966,1.92)

#how much each amount of predicted points changes based on a change in a variable (holding everything else constant)
#the variable must be accessed by number (second parameter)
plot(out2, 1, best.iter)
plot(out2, 2, best.iter)
plot(out2, 6, best.iter)

################################
#visualizations
library(ggplot2)
#PTS.TEAM frequecy (normal distribution)
g<- ggplot(old, aes(x = PTS.TEAM)) + geom_histogram(binwidth = 3, fill = "white", col = 'black') +
  labs(x = "PPG", y = "Frequency", title = "Frequency of PPG", caption = "(2015-2016, 2016-2017 season data)")

#varaince by team
varianceByTeam <- data.frame(Team = unique(old$TEAM), varTEAM = rep(NA, times = length(unique(old$TEAM))), 
                             varOPP = rep(NA, times = length(unique(old$TEAM))))
for (i in 1:length(unique(old$TEAM))) {
   tempSub <- subset(old, old$TEAM == old$TEAM[i])
   varianceByTeam$varTEAM[i] <- var(tempSub$PTS.TEAM)
   tempSub <- subset(old, old$OPPTEAM == old$TEAM[i])
   varianceByTeam$varOPP[i] <- var(tempSub$PTS.TEAM)
}
