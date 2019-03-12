# Date: Winter 2016
# Author: Michael Berk
# Description: Analyze nba box scores and stats to predict performances

########################################################
################# read in the files ####################
daysBack <- 20
dayList <- list()

#Create team array
nbaTeams <-
  c(
    "BOSTON CELTICS",
    "BROOKLYN NETS",
    "NEW YORK KNICKS",
    "PHILADELPHIA 76ERS",
    "TORONTO RAPTORS",
    "GOLDEN STATE WARRIORS",
    "LOS ANGELES CLIPPERS",
    "LOS ANGELES LAKERS",
    "PHOENIX SUNS",
    "SACRAMENTO KINGS",
    "CHICAGO BULLS",
    "CLEVELAND CAVALIERS",
    "DETROIT PISTONS",
    "INDIANA PACERS",
    "MILWAUKEE BUCKS",
    "DALLAS MAVERICKS",
    "HOUSTON ROCKETS",
    "MEMPHIS GRIZZLIES",
    "NEW ORLEANS PELICANS",
    "SAN ANTONIO SPURS",
    "ATLANTA HAWKS",
    "CHARLOTTE HORNETS",
    "MIAMI HEAT",
    "ORLANDO MAGIC",
    "WASHINGTON WIZARDS",
    "DENVER NUGGETS",
    "MINNESOTA TIMBERWOLVES",
    "OKLAHOMA CITY THUNDER",
    "PORTLAND TRAIL BLAZERS",
    "UTAH JAZZ"
  )

#Create short team array
shortNBATeams <-
  c(
    "BOS",
    "BKN",
    "NY",
    "PHI",
    "TOR",
    "GS",
    "LAC",
    "LAL",
    "PHO",
    "SAC",
    "CHI",
    "CLE",
    "DET",
    "IND",
    "MIL",
    "DAL",
    "HOU",
    "MEM",
    "NO",
    "SA",
    "ATL",
    "CHA",
    "MIA",
    "ORL",
    "WAS",
    "DEN",
    "MIN",
    "OKC",
    "POR",
    "UTA"
  )

for (i in 1:daysBack) {
  #loop through the days until daysBack (inclusive of daysBack)
  day <- toString(format(Sys.Date() - i, "%m.%d"))
  box <-
    read.csv(
      paste(
        "/Users/michaelberk/Documents/NBA Data/2016-2017 Season/boxScore/boxScore",
        day,
        ".txt",
        sep = ""
      ),
      sep = "@",
      stringsAsFactors = F
    )
  injury <-
    read.csv(
      paste(
        "/Users/michaelberk/Documents/NBA Data/2016-2017 Season/injury/injury",
        day,
        ".txt",
        sep = ""
      ),
      sep = "@",
      stringsAsFactors = F
    )
  
  #Clean injury
  #Remove position from injuryArray
  injury$PLAYER <- sub(" G ", "", injury$PLAYER)
  injury$PLAYER <- sub(" F ", "", injury$PLAYER)
  injury$PLAYER <- sub(" C ", "", injury$PLAYER)
  
  #replace make NA's a factor
  injury$SIDELINED <- lapply(injury$SIDELINED, as.factor)
  
  #Get rid of the teams in injuryArray
  for (t in nbaTeams) {
    injury$PLAYER <- sub(t, "", injury$PLAYER)
  }
  
  #get rid of extra spaces
  injury$PLAYER <- sub("     ", "", injury$PLAYER)
  
  ################################################################
  ###################### Clean Box ###############################
  if (day != 11.24) {
    box$PLAYER <- paste(box$PLAYER, "CCC")
    box$PLAYER <- gsub(" G CCC", "", box$PLAYER)
    box$PLAYER <- gsub(" F CCC", "", box$PLAYER)
    box$PLAYER <- gsub(" C CCC", "", box$PLAYER)
    box$PLAYER <- gsub(" CCC", "", box$PLAYER)
  }
  box$MIN <- as.numeric(box$MIN)
  
  #Add double double and trible double stats
  box$PTS <- as.numeric(box$PTS)
  DDptsBox <- ifelse(box$PTS >= 10, 1, 0)
  DDblkBox <- ifelse(box$BLKS >= 10, 1, 0)
  DDstlBox <- ifelse(box$STLS >= 10, 1, 0)
  DDrebBox <- ifelse(box$RB >= 10, 1, 0)
  DDastBox <- ifelse(box$AST >= 10, 1, 0)
  DD <-
    ifelse((DDptsBox + DDblkBox + DDstlBox + DDrebBox + DDastBox) >= 2, 1, 0)
  TD <-
    ifelse((DDptsBox + DDblkBox + DDstlBox + DDrebBox + DDastBox) >= 3, 1, 0)
  box <- cbind(box, DD, TD)
  
  #Add DK (Draft King Points)
  DK <-
    box$PTS + box$X3PM * 0.5 + box$RB * 1.25 + box$AST * 1.5 + box$STLS * 2 + box$BLKS *
    2 - box$TO * 0.5 +
    box$DD * 1.5 + box$TD * 3
  box <- cbind(box, DK)
  
  #Create games back identifier
  DaysBack <- rep(i, times = dim(box)[1])
  box <- cbind(DaysBack, box)
  
  #Make factors
  box$TEAM <- lapply(box$TEAM, as.character)
  box$OPPTEAM <- lapply(box$OPPTEAM, as.character)
  box$TEAM <- lapply(box$TEAM, as.factor)
  box$OPPTEAM <- lapply(box$OPPTEAM, as.factor)
  
  
  #############################################################################
  ################# Create daysFile and add it to dayList #####################
  daysFile <- NULL
  daysFile <- box
  #daysFile <- merge(box, injury, by = "PLAYER", all.x = T)
  
  #unlist everything in daysFile
  OPPTEAM <- unlist(daysFile$OPPTEAM)
  TEAM <- unlist(daysFile$TEAM)
  SIDELINED <- unlist(daysFile$SIDELINED)
  daysFile$OPPTEAM <- NULL
  daysFile$TEAM <- NULL
  daysFile$SIDELINED <- NULL
  #daysFile <- cbind(daysFile, OPPTEAM, SIDELINED, TEAM)
  daysFile <- cbind(daysFile, OPPTEAM, TEAM)
  
  #continue merging
  daysFile <- data.frame(daysFile)
  dayList[[i]] <- daysFile
}

#read in salary
library(dplyr)
zero <- toString(format(Sys.Date(), "%m.%d"))
salary <-
  read.csv(
    paste(
      "/Users/michaelberk/Documents/NBA Data/2016-2017 Season/salary/DKSalaries",
      zero,
      ".csv",
      sep = ""
    ),
    stringsAsFactors = F
  )
names(salary) <- toupper(names(salary)) #make names uppercase
names(salary)[2] <- "PLAYER" #change NAME to PLAYER
salary <- mutate_each(salary, funs(toupper)) #make columns upeprcase

#Clean salary
at <- regexpr("@", salary$GAMEINFO)
space <- regexpr(" ", salary$GAMEINFO)
AWAY <- substr(salary$GAMEINFO, 1, at - 1)
HOME <- substr(salary$GAMEINFO, at + 1, space - 1)

#change salay teams to full name
for (i in 1:30) {
  AWAY <- gsub(shortNBATeams[i], nbaTeams[i], AWAY)
  HOME <- gsub(shortNBATeams[i], nbaTeams[i], HOME)
  salary$TEAMABBREV <-
    gsub(shortNBATeams[i], nbaTeams[i], salary$TEAMABBREV)
}

salary <- cbind(salary, HOME, AWAY)
salary <- salary[-4]

#create OPPTEAM and cbind to salary
OPPTEAM <- c()
salary$TEAMABBREV <- as.character(salary$TEAMABBREV)
salary$AWAY <- as.character(salary$AWAY)
salary$HOME <- as.character(salary$HOME)
for (i in 1:dim(salary)[1]) {
  if (salary$TEAMABBREV[i] == salary$HOME[i]) {
    OPPTEAM[i] <- salary$AWAY[i]
  } else {
    OPPTEAM[i] <- salary$HOME[i]
  }
}
salary$TEAMABBREV <- as.factor(salary$TEAMABBREV)
salary$AWAY <- as.factor(salary$AWAY)
salary$HOME <- as.factor(salary$HOME)
salary <- cbind(salary, OPPTEAM)
salary$OPPTEAM <- as.factor(salary$OPPTEAM)

#rename TEAMABRVV
colnames(salary)[5] <- "TEAM"

#read in opp team
one <- toString(format(Sys.Date() - 1, "%m.%d"))
oppTeam <-
  read.csv(
    paste(
      "/Users/michaelberk/Documents/NBA Data/2016-2017 Season/oppTeam/oppTeam",
      one,
      ".txt",
      sep = ""
    ),
    sep = "@",
    stringsAsFactors = F
  )

#Clean oppTeam
oppTeam$OPPTEAM <-
  sub("LOS ANGELES CLIPPERS", "LA CLIPPERS", oppTeam$OPPTEAM)
oppTeam$OPPTEAM <-
  sub("LOS ANGELES LAKERS", "LA LAKERS", oppTeam$OPPTEAM)

#Read in advanced
advanced <-
  read.csv(
    paste(
      "/Users/michaelberk/Documents/NBA Data/2016-2017 Season/advanced/advanced",
      one,
      ".txt",
      sep = ""
    ),
    sep = "@"
  )

#########################################################################
########################### Make game files #############################
gamesBack <- 4
gameList <- list()

#create blank line for players that did not play
blank <- dayList[[1]][1, ]
blank$PLAYER <- "MICHAEL BERK"

#Create an array of every player in the nba called "playerArray"
playerArray <- c()
for (i in dayList) {
  playerArray <- c(playerArray, i$PLAYER)
  playerArray <- unique(playerArray)
}

#populate list with blanks's so that you can rbind
for (i in 1:gamesBack) {
  gameList[[i]] <- blank
}

for (i in playerArray) {
  #iterate through the list of all NBA players
  playerData <- data.frame()
  for (j in dayList) {
    #iterate through dayList
    #subset the data based on name - only one player will be in frame
    dataFrame <- as.data.frame(j)
    dataFrame <- subset(dataFrame, dataFrame$PLAYER == i)
    if (dim(playerData)[1] == 0) {
      #check if playerData is empty
      playerData <- dataFrame
    } else {
      playerData <- rbind(playerData, dataFrame)
    }
  }
  #add players into gameList
  playerData <-
    playerData[order(playerData$DaysBack), ] #order the player data file
  if (dim(playerData)[1] >= gamesBack) {
    for (l in 1:gamesBack) {
      if (!is.na(playerData[l, 1])) {
        #check if player is in playerData
        d <-
          as.data.frame(gameList[[l]]) #convert gameList to dataFrame
        d <- rbind(d, playerData[l, ])
        d <-
          subset(d, d$PLAYER != "MICHAEL BERK") #Remove MICHAEL BERK from list
        gameList[[l]] <- d #add value into gameList
        
      } else {
        d <- as.data.frame(gameList[[l]])
        d <- rbind(d, blank)
        d <-
          subset(d, d$PLAYER != "MICHAEL BERK") #Remove MICHAEL BERK from list
        gameList[[l]] <- d
      }
    }
  }
}

#######################################################################
###################### Set up the data frame for analysis #############
#append days back to column names so you can reference specific game stats
for (i in 1:length(gameList)) {
  colnames(gameList[[i]]) <-
    paste(colnames(gameList[[i]]), i, sep = ".")
  colnames(gameList[[i]])[2] <- "PLAYER"
}

#Create "d" datafile
d <- as.data.frame(gameList[[1]])
for (i in 2:length(gameList)) {
  d <- merge(d, as.data.frame(gameList[[i]]), by = "PLAYER")
}

#add big d salary
d <- merge(d, advanced, by = "PLAYER", all.x = T)

#clean player names
d$PLAYER <- gsub(",", "", d$PLAYER)
d$PLAYER <- gsub("KELLY OUBRE", "KELLY OUBRE JR.", d$PLAYER)
d$PLAYER <- gsub("CJ ", "C.J. ", d$PLAYER)
d$PLAYER <- gsub("LJ ", "L.J. ", d$PLAYER)
d$PLAYER <- gsub("TJ ", "T.J. ", d$PLAYER)
d$PLAYER <- gsub("DJ ", "D.J. ", d$PLAYER)
d$PLAYER <- gsub("JJ ", "J.J. ", d$PLAYER)
d$PLAYER <- gsub("PJ ", "P.J. ", d$PLAYER)
d$PLAYER <- gsub("AJ ", "A.J. ", d$PLAYER)
d$PLAYER <- gsub("KJ ", "K.J. ", d$PLAYER)
d$PLAYER <- gsub("PJ ", "P.J. ", d$PLAYER)
d$PLAYER <- gsub("RJ ", "R.J. ", d$PLAYER)
d$PLAYER <- gsub(" JR.", " J.R.", d$PLAYER)
d$PLAYER <- gsub(" JR", " J.R.", d$PLAYER)
d$PLAYER <- gsub("TA.J. ", "TAJ ", d$PLAYER)
d$PLAYER <-
  gsub("TIMOTHE LUWAWU", "TIMOTHE LUWAWU-CABARROT", d$PLAYER)
d$PLAYER <- gsub("NENE", "NENE HILARIO", d$PLAYER)

#merge salary and d
d <-
  merge(as.data.frame(salary),
        as.data.frame(d),
        by = "PLAYER",
        all = F)

#make sure d datatypes are correct
d$SALARY <- as.numeric(d$SALARY)
for (i in 1:dim(d)[2]) {
  if (class(d[, i]) == "character") {
    d[, i] <- as.factor(d[, i])
  }
}

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
attach(d)
out <-
  gbm(
    DK.1 ~ MIN.2 + MIN.3 + MIN.4 + PTS.2 + PTS.3 + PTS.4 + RB.2 + RB.3 + RB.4 +
      STLS.2 + STLS.3 + STLS.4
    + BLKS.2 + BLKS.3 + BLKS.4 + AST.2 + AST.3 + AST.4 + X3P.2 + X3P.3 +
      X3P.4 + X3PM.2 + X3PM.3 + X3PM.4
    + TSP + PACE + OFFRATING + TRB + POSITION + SALARY,
    data = d,
    n.trees = 10000,
    interaction.depth = 3,
    n.minobsinnode = 25,
    shrinkage = .001,
    bag.fraction = 0.5,
    n.cores = 1,
    distribution = list(name = "quantile",
                        alpha = 0.35)
  )
detach(d)

#graphs the best number of iterations and outputs the number
best.iter <- gbm.perf(out, method = "OOB")

#prediction of points for each player with number of trees found above
prediction <- predict(out, d, n.trees = best.iter)
#plot the prediction with points
plot(prediction, d$DK.1)
cor(prediction, d$DK.1)

#get a linear model for the predicted amount of points against the actual points scored (see how well you predicted)
reg <- lm(d$DK.1 ~ prediction)
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

######################################################
################## optimize lineup ###################
# o <- d
# at <- regexpr("/", o$POSITION)
# ifelse(at == -1, )
# for (i in 1:dim(d)[1]) {
#   if (length(o$POSITION) > 2) {
#     print()
#   }
# }
# twoPos <- function() {
#
# }
