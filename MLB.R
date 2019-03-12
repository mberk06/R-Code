# Date: Spring 2017
# Author: Michael Berk
# Description: This file will be used to predict Draft Kings Points using prior data for MLB

#Read in files
Date <- "05.14"
PitcherHandedness <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/PitcherHandedness/PitcherHandedness",Date,".txt",sep=""),sep="@", stringsAsFactors = F)

Date <- toString(format(Sys.Date(), "%m.%d"))
Date <- "05.15"
AdvancedBatter <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/AdvancedBatter/AdvancedBatter",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
AdvancedPitcher <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/AdvancedPitcher/AdvancedPitcher",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
DKSalary <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/DKSalary/DKSalaries",Date,".csv",sep=""),sep=",", stringsAsFactors = F)
BatterPitcherMatchup <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/BatterPitcherMatchup/BatterPitcherMatchup",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
PitcherBatterMatchup <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/PitcherBatterMatchup/PitcherBatterMatchup",Date,".txt",sep=""),sep="@", stringsAsFactors = F)

YesterdayBatter <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/YesterdayBatter/YesterdayBatter",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
Last7Batter <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/Last7Batter/Last7Batter",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
LastMonthBatter <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/LastMonthBatter/LastMonthBatter",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
YTDBatterHome <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/YTDBatter/YTDBatterHome",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
YTDBatterAway <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/YTDBatter/YTDBatterAway",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
YTDBatterVsLeft <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/YTDBatter/YTDBatterVsLeft",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
YTDBatterVsRight <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/YTDBatter/YTDBatterVsRight",Date,".txt",sep=""),sep="@", stringsAsFactors = F)

PitcherYTDHome <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/PitcherTraditional/PitcherTraditionalHome",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
PitcherYTDAway <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/PitcherTraditional/PitcherTraditionalAway",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
PitcherYTDVsLeft <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/PitcherTraditional/PitcherTraditionalVsLeft",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
PitcherYTDVsRight <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/PitcherTraditional/PitcherTraditionalVsRight",Date,".txt",sep=""),sep="@", stringsAsFactors = F)

#CHECK TEAM STATS
TeamFielding <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamFielding/TeamFielding",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamHittingHome <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamHitting/TeamHittingHome",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamHittingAway <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamHitting/TeamHittingAway",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamHittingVsLeft <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamHitting/TeamHittingVsLeft",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamHittingVsRight <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamHitting/TeamHittingVsRight",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamPitchingHome <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamPitching/TeamPitchingHome",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamPitchingAway <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamPitching/TeamPitchingAway",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamPitchingVsLeft <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamPitching/TeamPitchingVsLeft",Date,".txt",sep=""),sep="@", stringsAsFactors = F)
TeamPitchingVsRight <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/TeamPitching/TeamPitchingVsRight",Date,".txt",sep=""),sep="@", stringsAsFactors = F)

Date <- "06.14"
BatterBox <- read.csv(paste("/Users/michaelberk/Documents/MLB/2017 Data/BatterBox/BatterBox",Date,".txt",sep=""),sep="@", stringsAsFactors = F)

###############################################################
################### Make Uppercase ############################
###############################################################
#Make DKSalary uppercase
library(dplyr)
DKSalary <- mutate_each(DKSalary, funs(toupper))
names(DKSalary) <- toupper(names(DKSalary))

#Make PitcherHandedness uppercase
PitcherHandedness <- mutate_each(PitcherHandedness, funs(toupper))
names(PitcherHandedness) <- toupper(names(PitcherHandedness))

###############################################################
######## Create Home/Away for DK and Clean Names ##############
###############################################################
at <- regexpr("@", DKSalary$GAMEINFO)
space <- regexpr(" ", DKSalary$GAMEINFO)
AWAY <- substr(DKSalary$GAMEINFO, 1, at - 1)
HOME <- substr(DKSalary$GAMEINFO, at + 1, space - 1)
ISHOME <- DKSalary$TEAMABBREV == HOME
DKSalary <- cbind(DKSalary, HOME, AWAY, ISHOME)

#Clean names
names(DKSalary) <- c("POSITION","NAME","SALARY","GAMEINFO","AVGPOINTSPERGAME","TEAM",
                     "HOME","AWAY","ISHOME")

###############################################################
############## Create First/Last Name for DK ##################
###############################################################
space <- regexpr(" ", DKSalary$NAME)
FIRST <- substr(DKSalary$NAME, 1, space - 1)
LAST <- substr(DKSalary$NAME, space + 1, length(DKSalary$NAME))
DKSalary <- cbind(DKSalary, FIRST, LAST)

###############################################################
############## Split DK into Batter/Pitcher ###################
###############################################################
DKSalaryBatter <- subset(DKSalary, DKSalary$POSITION != 'SP' & DKSalary$POSITION != 'RP')
DKSalaryPitcher <- subset(DKSalary, DKSalary$POSITION == 'SP' | DKSalary$POSITION == 'RP')

###############################################################
############## Get Handedness For Pitchers ####################
###############################################################
THROWS.PH <- substring(PitcherHandedness$HANDED, 8)
PitcherHandedness <- cbind(PitcherHandedness, THROWS.PH)

###############################################################
################### Create All Data List ######################
###############################################################
AllDataList <- list(AdvancedBatter,AdvancedPitcher,DKSalaryBatter,DKSalaryPitcher,BatterPitcherMatchup,
                    PitcherBatterMatchup,YesterdayBatter,Last7Batter,LastMonthBatter,YTDBatterHome,
                    YTDBatterAway,YTDBatterVsLeft,YTDBatterVsRight,PitcherYTDHome,
                    PitcherYTDAway,PitcherYTDVsLeft,PitcherYTDVsRight,
                    TeamFielding,TeamHittingHome,TeamHittingAway,TeamHittingVsLeft,TeamHittingVsRight,
                    TeamPitchingHome,TeamPitchingAway,TeamPitchingVsLeft,TeamPitchingVsRight,
                    PitcherHandedness)
ColumnExtensions <- c(".AB",".AP","","",".BPM",".PBM",".B.YEST",".B.LW",".B.LM",".B.YTD",
                      ".B.YTD",".B.YTD",".B.YTD",".P.YTD",".P.YTD",".P.YTD",".P.YTD",
                      ".TF",".TH.H",".TH.A",".TH.L",".TH.R",".TP.H",".TP.A",".TP.L",".TP.R",".PH")

###############################################################
############### Remove Commas and "-" #########################
###############################################################
#iterate through the data list
for (i in 1:length(AllDataList)) {
  #iterate through columns
  for (j in 1:dim(AllDataList[[i]])[2]) {
    AllDataList[[i]][,j] <- gsub(",","",AllDataList[[i]][,j])
    AllDataList[[i]][,j] <- gsub("-",NA,AllDataList[[i]][,j])
  }
}

###############################################################
############### Remove Duplicate Rows #########################
###############################################################
#Create remove duplicate columns function
RemoveDuplicateCols <- function(df) {
  #Check if the names are duplicates and remove them
  colsToStrike <- (grepl(".1", names(df)))
  df <- df[,!colsToStrike]
  colsToStrike <- (grepl(".2", names(df)))
  df <- df[,!colsToStrike]
  colsToStrike <- (grepl(".3", names(df)))
  #Return cleaned data
  return (df[,!colsToStrike])
}

#Call above func on data
for (i in 1:length(AllDataList)) {
  AllDataList[[i]] <- RemoveDuplicateCols(AllDataList[[i]])
}

###############################################################
################# Standardize Team Names ######################
###############################################################
#Create the team names
LongTeams <- c("ATLANTA BRAVES","ARIZONA DIAMONDBACKS","BALTIMORE ORIOLES","BOSTON RED SOX"
               ,"CHICAGO CUBS","CINCINNATI REDS","CLEVELAND INDIANS","COLORADO ROCKIES"
               ,"CHICAGO WHITE SOX","DETROIT TIGERS","KANSAS CITY ROYALS","HOUSTON ASTROS"
               ,"LOS ANGELES ANGELS","LOS ANGELES DODGERS","MIAMI MARLINS","MILWAUKEE BREWERS"
               ,"MINNESOTA TWINS","NEW YORK METS","NEW YORK YANKEES","OAKLAND ATHLETICS"
               ,"PHILADELPHIA PHILLIES","PITTSBURGH PIRATES","SAN DIEGO PADRES"
               ,"SAN FRANCISCO GIANTS","SEATTLE MARINERS","ST LOUIS CARDINALS","TAMPA BAY RAYS"
               ,"TEXAS RANGERS","TORONTO BLUE JAYS","WASHINGTON NATIONALS")
StandardTeams <- c("ATL","ARI","BAL","BOS","CHC","CIN","CLE","COL","CWS","DET","KC","HOU","LAA"
                   ,"LAD","MIA","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SD","SF","SEA","STL"
                   ,"TB","TEX","TOR","WAS")

#Replace LongTeams with Standard Teams
#Iterate through dataframes
for (i in 1:length(AllDataList)) {
  #Iterate through columns
  for (j in 1:dim(AllDataList[[i]])[2]) {
    #Iterate through teams
    for (l in 1:length(LongTeams)) {
      AllDataList[[i]][,j] <- gsub(LongTeams[l], StandardTeams[l], AllDataList[[i]][,j])
    }
  }
}

###############################################################
####################### Create Matcher Column  ################
###############################################################
#This column will be a unique id composed of "LastName/FirstLetterFirstName/TeamAbreviation"
#iterate through the data list
for (i in 1:length(AllDataList)) {
  #PLAYER ID
  FIRST <- paste("FIRST", ColumnExtensions[i], sep = "")
  if (FIRST %in% names(AllDataList[[i]])) {
    #Get first letter and create new column
    column <- as.vector(unlist(subset(AllDataList[[i]], select = FIRST)))
    FIRST.LETTER.FIRST.NAME <- substring(column, 1, 1)
    AllDataList[[i]] <- cbind(AllDataList[[i]], FIRST.LETTER.FIRST.NAME)
  }

  #PITCHER ID
  PITCHERFIRST <- paste("PITCHERFIRST", ColumnExtensions[i], sep = "")
  if (PITCHERFIRST %in% names(AllDataList[[i]])) {
    #Get first letter and create new column
    column <- as.vector(unlist(subset(AllDataList[[i]], select = PITCHERFIRST)))
    PITCHER.FIRST.LETTER.FIRST.NAME <- substring(column, 1, 1)
    AllDataList[[i]] <- cbind(AllDataList[[i]], PITCHER.FIRST.LETTER.FIRST.NAME)
  }
}

#Create unique id
for (i in 1:length(AllDataList)) {
  PLAYERID <- c()
  if ("FIRST.LETTER.FIRST.NAME" %in% names(AllDataList[[i]])) {
    temp <- AllDataList[[i]]
    LASTNAME <- paste("LAST", ColumnExtensions[i], sep = "")
    LastNameColumn <- as.vector(unlist(subset(AllDataList[[i]], select = LASTNAME)))
    for (j in 1:length(LastNameColumn)) { 
      id <- paste(LastNameColumn[j], as.character(temp$FIRST.LETTER.FIRST.NAME[j]), sep = "/")
      TEAMNAME <- paste("TEAM", ColumnExtensions[i], sep = "")
      TeamColumn <- as.vector(unlist(subset(AllDataList[[i]], select = TEAMNAME)))
      id <- paste(id, TeamColumn[j], sep = "/")
      PLAYERID <- c(PLAYERID,  id)
    } 
    AllDataList[[i]] <- cbind(AllDataList[[i]], PLAYERID)
  }
  
  PITCHERID <- c()
  if ("PITCHER.FIRST.LETTER.FIRST.NAME" %in% names(AllDataList[[i]])) {
    temp <- AllDataList[[i]]
    PITCHERLAST <- paste("PITCHERLAST", ColumnExtensions[i], sep = "")
    PitcherLastColumn <- as.vector(unlist(subset(AllDataList[[i]], select = PITCHERLAST)))
    for (j in 1:length(PitcherLastColumn)) {
      id <- paste(PitcherLastColumn[j], as.character(temp$PITCHER.FIRST.LETTER.FIRST.NAME[j]), sep = "/")
      OPP <- paste("OPP", ColumnExtensions[i], sep = "")
      OppColumn <- as.vector(unlist(subset(AllDataList[[i]], select = OPP)))
      id <- paste(id, OppColumn[j], sep = "/")
      PITCHERID <- c(PITCHERID,  id)
    } 
    AllDataList[[i]] <- cbind(AllDataList[[i]], PITCHERID)
  }
}

###############################################################
############### Make Cols Numeric #############################
###############################################################
#iterate through the data list
for (i in 1:length(AllDataList)) {
  #iterate through columns
  for (j in 1:dim(AllDataList[[i]])[2]) {
    #check if column has letters in it
    if (!(T %in% grepl("[[:alpha:]]", AllDataList[[i]][,j]))) {
      AllDataList[[i]][,j] <- as.numeric(AllDataList[[i]][,j])
    } 
  }
}

###############################################################
############## Create DK Points in YesterdayBatter ############
###############################################################
#THIS MODEL DOES NOT INCLUDE 2B AND 3B BECAUSE THEY ARE NOT IN THE YESTERDAYBATTER FILE
#Batter
H <- AllDataList[[7]]$H.B.YEST - AllDataList[[7]]$HR.B.YEST
HR <- AllDataList[[7]]$HR.B.YEST
RBI <- AllDataList[[7]]$RBI.B.YEST
R <- AllDataList[[7]]$R.B.YEST
BB <- AllDataList[[7]]$BB.B.YEST
HBP <- AllDataList[[7]]$HBP.B.YEST
SB <- AllDataList[[7]]$SB.B.YEST
DKPOINTS <- (H*3) + (HR*10) + (RBI*2) + (R*2) + (BB*2) + (HBP*2) + (SB*5)
AllDataList[[7]] <- cbind(DKPOINTS, AllDataList[[7]])

###############################################################
############## Create DK Points in YesterdayPitcher ###########
###############################################################
#TO DO THIS, YOU NEED TO SCRAPE THE BOX SCORE AND CREATE GAMES BACK FILES LIKE WITH NBA

###############################################################
################# Merge Batter Files ##########################
###############################################################
#Test merges
Batters <- merge(AllDataList[[3]], AllDataList[[7]], by = "PLAYERID")
Batters <- merge(Batters, AllDataList[[9]], by = "PLAYERID")
Batters <- merge(Batters, AllDataList[[1]], by = "PLAYERID")
#MAKE BATTERPITCHERMATCHUP have more players - see if you can select all players instead of qualifiers

#Merge DKSalaryBatters with BatterPitcherMatchup on "PLAYERID" 
#(BatterPitcherMatchup is limiting the number of columns - only has 115)
Batters <- merge(AllDataList[[3]], AllDataList[[5]], by = "PLAYERID")

#Merge BatterPitcherMatchup with PitcherHandedness based on ID for Pitchers
Batters <- merge(Batters, AllDataList[[27]], by.x = "PITCHERID", by.y = "PLAYERID")

#Merge Batters with YTD vsleft/vsright on "PLAYERID" - use BatterPitcherMatchup
ThrowsRight <- subset(Batters, Batters$THROWS == 'R')
ThrowsLeft <- subset(Batters, Batters$THROWS == 'L')
mergeRight <- merge(ThrowsRight, AllDataList[[13]], by = "PLAYERID")
mergeLeft <- merge(ThrowsLeft, AllDataList[[12]], by = "PLAYERID")
Batters <- rbind(mergeRight, mergeLeft)

#Merge Batters with YTD Home/Away on "PLAYERID" - use isHome in DKSalary
HomeBatters <- subset(Batters, Batters$ISHOME == T)
AwayBatters <- subset(Batters, Batters$ISHOME == F)
mergeHome <- merge(HomeBatters, AllDataList[[10]], by = "PLAYERID")
mergeAway <- merge(AwayBatters, AllDataList[[11]], by = "PLAYERID")
Batters <- rbind(mergeHome, mergeAway)

#Merge Batters with Yesterday on "PLAYERID"
Batters <- merge(AllDataList[[7]], Batters, by = "PLAYERID")

#Merge Batters with LastWeek on "PLAYERID"
Batters <- merge(Batters, AllDataList[[8]], by = "PLAYERID")

#Merge Batters with LastMonth on "PLAYERID"
Batters <- merge(Batters, AllDataList[[9]], by = "PLAYERID")

#Merge Batteers with Advanced on "PLAYERID"
Batters <- merge(Batters, AllDataList[[1]], by = "PLAYERID")

###############################################################
################# Merge Pitcher Files #########################
###############################################################
#Test merges
Pitchers <- merge(AllDataList[[4]], AllDataList[[2]], by = "PLAYERID")

########################################################################
##################### Machine Learning analysis ########################
#boosted quiantile regression (25th percentile)
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
attach(Pitchers)
out <-
  gbm(
    DKPOINTS ~ AVGPOINTSPERGAME + RBI.B.YEST + HR.B.YEST + AVG.AB + K.AB + BABIP.AB,
    data = Batters,
    n.trees = 3000,
    interaction.depth = 1,
    n.minobsinnode = 15,
    shrinkage = .001,
    bag.fraction = 0.5,
    n.cores = 1,
    distribution = list(name = "quantile",
                        alpha = 0.35)
  )
detach(Batters)

#graphs the best number of iterations and outputs the number
best.iter <- gbm.perf(out, method = "OOB")

#prediction of points for each player with number of trees found above
prediction <- predict(out, Batters, n.trees = 1000)
#plot the prediction with points
plot(prediction, Batters$DKPOINTS)
cor(prediction, Batters$DKPOINTS)

#get a linear model for the predicted amount of points against the actual points scored (see how well you predicted)
reg <- lm(Batters$DKPOINTS ~ prediction)
#put line of best fit
abline(reg[[1]][1], reg[[1]][2])

#how much each amount of predicted points changes based on a change in a variable (holding everything else constant)
#the variable must be accessed by number (second parameter)
# plot(out, 1, best.iter)
# plot(out, 2, best.iter)
# plot(out, 3, best.iter)
# plot(out, 4, best.iter)
# plot(out, 7, best.iter)

#create a dataframe with the predicted outputs
Player <- as.vector(d$PLAYER)
Salary <- as.vector(d$SALARY)
PointsPerDollar <-
  as.vector(1000 * (prediction / as.numeric(d$SALARY)))
Position <- as.vector(d$POSITION)
bet <- NULL
bet <-
  data.frame(Player, Salary, PointsPerDollar, Position, prediction) #cbind might be wrong
View(bet)

#Ways to see which values are not in both datasets
#subset(AllDataList[[5]], !(AllDataList[[5]]$PLAYERID %in% Batters$PLAYERID))
#setdiff(AllDataList[[5]]$PLAYERID, Batters$PLAYERID)
















