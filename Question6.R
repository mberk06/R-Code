# Date: Spring 2018
# Author: Michael Berk
# Description: 
#	This file will be used to address the question: 
#   For sites listed as bleached or RKC in one year, is there recovery 
#   of those sites as there was in 1998-99. Some think that cumulative 
#   impacts on reefs are making recovery difficult.

#Rephrase: If a reef exhibits bleaching, what is the health of the reef after bleaching
#Bleaching will be measured by Organism$Organism.Code - Bleaching (% of population) or Bleaching (% of population)
#Health of the reef will be measured by characteristics in notes

#data is loaded in testing file
###########################################################
#create new data
d <- Organism
d <- as.data.table(d)
d <- as.data.table(lapply(d, toupper))

###########################################################
#indicate in d if the reef is the first instance of bleaching
# firstOccurance <- subset(bleached, bleached$OccuranceNumber == 1)
# secondOccurance <- subset(bleached, bleached$OccuranceNumber == 2)
# thirdOccurance <- subset(bleached, bleached$OccuranceNumber == 3)
# fourthOccurance <- subset(bleached, bleached$OccuranceNumber == 4)
# fifthOccurance <- subset(bleached, bleached$OccuranceNumber == 5)
# sixthOccurance <- subset(bleached, bleached$OccuranceNumber == 6)
# seventhOccurance <- subset(bleached, bleached$OccuranceNumber == 7)
# eigthOccurance <- subset(bleached, bleached$OccuranceNumber == 8)
# 
# isFirstOccurance <- ifelse((d$Reef.Name %in% firstOccurance$Reef.Name) & (d$Date %in% firstOccurance$Date), T, F)
# d <- cbind(d, isFirstOccurance)
# 
# #indicate in d if the reef is the second instance of bleaching
# secondOccurance <- subset(bleached, bleached$OccuranceNumber == 2)
# isSecondOccurance <- ifelse((d$Reef.Name %in% secondOccurance$Reef.Name) & (d$Date %in% secondOccurance$Date) & (d$isFirstOccurance == F), T, F)
# d <- cbind(d, isSecondOccurance)
# 
# 
# mergeTable <- data.table(Reef.Name = bleached$Reef.Name, Date = bleached$Date, OccuranceNumber = bleached$OccuranceNumber)
# df_dups <- d[,c("Reef.Name", "Date")]
# d  <- d[!duplicated(df_dups),]
# d <- merge(d, mergeTable, by = c("Reef.Name", "Date"),all.x = T)
###########################################################
#Create a data file with each row as a unique reef and date (only include relevant Organism.Codes)

#Clean d
#trim whitespace
d$Organism.Code <- trimws(d$Organism.Code)

#make numeric
d$S1 <- as.numeric(d$S1)
d$S2 <- as.numeric(d$S2)
d$S3 <- as.numeric(d$S3)
d$S4 <- as.numeric(d$S4)

#Recode NA's to 0's
d[is.na(d$S1)]$S1 <- 0
d[is.na(d$S2)]$S2 <- 0
d[is.na(d$S3)]$S3 <- 0
d[is.na(d$S4)]$S4 <- 0

#Add lattitude and longitude
Longitude <- as.numeric(paste(d$Longitude.Degrees, d$Longitude.Minutes, d$Longitude.Seconds, sep = ""))
Latitude <- as.numeric(paste(d$Latitude.Degrees, d$Latitude.Minutes, d$Latitude.Seconds, sep = ""))
d <- cbind(Longitude, Latitude, d)

# BLEACHING (% OF COLONY)
tempSub <- subset(d, d$Organism.Code == "BLEACHING (% OF COLONY)")
bc <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Colony.Bleaching.Percent = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# BLEACHING (% OF POPULATION)
tempSub <- subset(d, d$Organism.Code == "BLEACHING (% OF POPULATION)")
bp <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Population.Bleaching.Percent = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# GROUPER TOTAL
tempSub <- subset(d, d$Organism.Code == "GROUPER TOTAL")
gt <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Grouper.Total = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# BUMPHEAD PARROT
tempSub <- subset(d, d$Organism.Code == "BUMPHEAD PARROT")
bpt <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Bumphead.Parrot = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# PARROTFISH
tempSub <- subset(d, d$Organism.Code == "PARROTFISH")
pt <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Parrot = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# HUMPHEAD WRASSE
tempSub <- subset(d, d$Organism.Code == "HUMPHEAD WRASSE")
hwt <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Humphead.Wrasse = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# MORAY EEL
tempSub <- subset(d, d$Organism.Code == "MORAY EEL")
morayt <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Moray.Eel = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# SNAPPER
tempSub <- subset(d, d$Organism.Code == "SNAPPER")
st <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Snapper = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# TRASH GENERAL
tempSub <- subset(d, d$Organism.Code == "TRASH GENERAL")
tg <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Trash.General = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))
# TRASH FISH NETS 
tempSub <- subset(d, d$Organism.Code == "TRASH FISH NETS")
tfn <- na.omit(data.table(Name.Date = paste(tempSub$Reef.Name, as.character(tempSub$Date), sep = "."), Trash.Fish.Nets = (tempSub$S1+tempSub$S2+tempSub$S3+tempSub$S4)))

#clean percentages
bc$Colony.Bleaching.Percent <-  ifelse(bc$Colony.Bleaching.Percent > 100, bc$Colony.Bleaching.Percent / 10, bc$Colony.Bleaching.Percent)
bp$Population.Bleaching.Percent <- ifelse(bp$Population.Bleaching.Percent > 100, bp$Population.Bleaching.Percent / 10, bp$Population.Bleaching.Percent)
bp$Population.Bleaching.Percent <- ifelse(bp$Population.Bleaching.Percent > 0 & bp$Population.Bleaching.Percent < 1, bp$Population.Bleaching.Percent * 100, bp$Population.Bleaching.Percent)
bp$Population.Bleaching.Percent <- ifelse(bp$Population.Bleaching.Percent > 0 & bp$Population.Bleaching.Percent < 1, bp$Population.Bleaching.Percent * 100, bp$Population.Bleaching.Percent)
bc$Colony.Bleaching.Percent<- ifelse(bc$Colony.Bleaching.Percent > 0 & bc$Colony.Bleaching.Percent < 1,bc$Colony.Bleaching.Percent  * 100, bc$Colony.Bleaching.Percent)
bc$Colony.Bleaching.Percent<- ifelse(bc$Colony.Bleaching.Percent > 0 & bc$Colony.Bleaching.Percent < 1,bc$Colony.Bleaching.Percent  * 100, bc$Colony.Bleaching.Percent)


#set up full data file
d <- as.data.table(d)
Name.Date <- paste(d$Reef.Name, as.character(d$Date), sep = ".")
d <- cbind(d, Name.Date)
d <- d[order(d$Organism.Code)]
dd <- d[!duplicated(d$Name.Date),]
dd <- dd[,c("Organism.Code","S1","S2","S3","S4"):=NULL]

#purpose: take in df, move duplicates to one row, and return cleaned df
#parameters: data frame
#return: data frame
sumDuplicates <- function(df) {
  #create df that will be edited
  editDF <- df
  #create return df with unique name columns
  returnDF <- df
  returnDF <- returnDF[!duplicated(returnDF$Name.Date),]

  #for every row
  for (i in 1:dim(returnDF)[1]) {
    currentRow <- returnDF[i,]
    #subset the data so that only the current NameDate is shown
    tempDF <- subset(editDF, editDF$Name.Date == currentRow$Name.Date)
    #sum all values in the subset and insert the value into return df
    returnDF[i,2] <- mean(unlist(tempDF[,2]))
    #delete the rows with corresponding name date
    editDF <- subset(editDF, editDF$Name.Date != currentRow$Name.Date)
  }

  #return df
  return (returnDF)
}

#sum for all datasets
bc <- sumDuplicates(bc)
bp <- sumDuplicates(bp)
gt <- sumDuplicates(gt)
bpt <- sumDuplicates(bpt)
pt <- sumDuplicates(pt)
hwt <- sumDuplicates(hwt)
morayt <- sumDuplicates(morayt)
st <- sumDuplicates(st)
tg <- sumDuplicates(tg)
tfn <- sumDuplicates(tfn)

dd <- merge(dd, bc, by = "Name.Date", all.x = T)
dd <- merge(dd, bp, by = "Name.Date", all.x = T)
dd <- merge(dd, gt, by = "Name.Date", all.x = T)
dd <- merge(dd, bpt, by = "Name.Date", all.x = T)
dd <- merge(dd, pt, by = "Name.Date", all.x = T)
dd <- merge(dd, hwt, by = "Name.Date", all.x = T)
dd <- merge(dd, morayt, by = "Name.Date", all.x = T)
dd <- merge(dd, st, by = "Name.Date", all.x = T)
dd <- merge(dd, tg, by = "Name.Date", all.x = T)
dd <- merge(dd, tfn, by = "Name.Date", all.x = T)

###########################################################
#number the occurances
#create a df that only includes sites that exhibit bleaching or have exhibited bleaching before

#find all sites that exhibited bleaching (bleachedCodes does no include % of colony)
bleached <- subset(dd, dd$Colony.Bleaching.Percent > 0 | dd$Population.Bleaching.Percent > 0)
bleachedReefNames <- unique(bleached$Reef.Name)

#add occurance number column and ID
OccuranceNumber <- rep(0, dim(dd)[1])
dd <- cbind(dd, OccuranceNumber)

#replace NA with -1
dd[is.na(dd)] <- -1

#edit occurance number
for (reef in unique(dd$Reef.Name)) {
  #subset to individual reef
  tempSubset <- subset(dd, dd$Reef.Name == reef)
  #order the reef based on date
  ordered <- tempSubset[order(as.Date(tempSubset$Date, format = "%d-%b-%y")),]
  #append occurance number to each occurance
  hasFoundBleaching <- F
  occNum <- 1
  for (i in 1:dim(ordered)[1]) {
    #if prior rows have exhibited bleaching
    if (!hasFoundBleaching) {
      #if the first instance of bleaching is the current row
      if ((ordered$Colony.Bleaching.Percent[i] > 0) | (ordered$Population.Bleaching.Percent[i] > 0)) {
        
        #insert occurance number into dd
        idOfRow <- ordered$ID[i]
        dd[dd$ID == idOfRow,]$OccuranceNumber <- occNum
        
        #add one to occNum indicate reef bleaching row has been found
        occNum <- occNum + 1
        hasFoundBleaching <- T
      }
    }
    #if prior rows have not exhibited bleaching
    else {
      #insert occurance number into dd
      idOfRow <- ordered$ID[i]
      dd[dd$ID == idOfRow,]$OccuranceNumber <- occNum
      
      #iterate occNum
      occNum <- occNum + 1
    }
    
  }
}

#replace -1 with NA
dd[dd == -1] <- NA

#final data set
totaldd <- dd
dd <- subset(dd, dd$OccuranceNumber != 0)
############################################################
#Analyize from 11/7 Notes
#add site data
S <- Site
Name.Date <- paste(S$Reef.Name, as.character(S$Date), sep = ".")
S <- cbind(Name.Date, S)
S <- subset(S, S$Name.Date %in% dd$Name.Date)

#Distance.from.shore..m.
dfs <- sumDuplicates(na.omit(data.table(Name.Date = S$Name.Date, Distance.from.shore..m. = as.numeric(S$Distance.from.shore..m.))))
#Depth..m.
depth <- sumDuplicates(na.omit(data.table(Name.Date = S$Name.Date, Depth..m. = as.numeric(S$Depth..m.))))
#Water.temp.at.surface
st <- sumDuplicates(na.omit(data.table(Name.Date = S$Name.Date, Water.temp.at.surface = as.numeric(S$Water.temp.at.surface))))
#Water.temp.at.5m
fiveT <- sumDuplicates(na.omit(data.table(Name.Date = S$Name.Date, Water.temp.at.5m = as.numeric(S$Water.temp.at.5m))))
#Water.temp.at.10m
tenT <- sumDuplicates(na.omit(data.table(Name.Date = S$Name.Date, Water.temp.at.10m = as.numeric(S$Water.temp.at.10m))))
#Sheltered.or.exposed.
soe <- na.omit(data.table(Name.Date = S$Name.Date, Sheltered.or.exposed. = S$Sheltered.or.exposed.))
soe <- soe[!duplicated(soe$Name.Date),]
#Any.major.storms.in.last.years.
storm <- na.omit(data.table(Name.Date = S$Name.Date, Any.major.storms.in.last.years. = S$Any.major.storms.in.last.years.))
storm <- storm[!duplicated(storm$Name.Date),]
#Dynamite.Fishing.
dynamite <- na.omit(data.table(Name.Date = S$Name.Date, Dynamite.Fishing. = S$Dynamite.Fishing.))
dynamite <- dynamite[!duplicated(dynamite$Name.Date),]
#Poison.Fishing.
pf <- na.omit(data.table(Name.Date = S$Name.Date, Poison.Fishing. = S$Poison.Fishing.))
pf <- pf[!duplicated(pf$Name.Date),]
#Tourist.diving.snorkeling
tds <- na.omit(data.table(Name.Date = S$Name.Date, Tourist.diving.snorkeling = S$Tourist.diving.snorkeling))
tds <- tds[!duplicated(tds$Name.Date),]
#Sewage.pollution
sp <- na.omit(data.table(Name.Date = S$Name.Date, Sewage.pollution = S$Sewage.pollution))
sp <- sp[!duplicated(sp$Name.Date),]
#Industrial.pollution
ip <- na.omit(data.table(Name.Date = S$Name.Date, Industrial.pollution = S$Industrial.pollution))
ip <- ip[!duplicated(ip$Name.Date),]
#Commercial.fishing
cf <- na.omit(data.table(Name.Date = S$Name.Date, Commercial.fishing = S$Commercial.fishing))
cf <- cf[!duplicated(cf$Name.Date),]
#Is.site.protected.
isp <- na.omit(data.table(Name.Date = S$Name.Date, Is.site.protected. = S$Is.site.protected.))
isp <- isp[!duplicated(isp$Name.Date),]
#Is.protection.enforced.
ipe <- na.omit(data.table(Name.Date = S$Name.Date, Is.protection.enforced. = S$Is.protection.enforced.))
ipe <- ipe[!duplicated(ipe$Name.Date),]
#Harvest.of.inverts.for.food
hif <- na.omit(data.table(Name.Date = S$Name.Date, Harvest.of.inverts.for.food. = S$Harvest.of.inverts.for.food))
hif <- hif[!duplicated(hif$Name.Date),]
#Harvest.of.inverts.for.curio
hic <- na.omit(data.table(Name.Date = S$Name.Date, Harvest.of.inverts.for.curio. = S$Harvest.of.inverts.for.curio))
hic <- hic[!duplicated(hic$Name.Date),]
#Aquarium.fish.collection
afc <- na.omit(data.table(Name.Date = S$Name.Date, Aquarium.fish.collection = S$Aquarium.fish.collection))
afc <- afc[!duplicated(afc$Name.Date),]
#Anchoring
a <- na.omit(data.table(Name.Date = S$Name.Date, Anchoring = S$Anchoring))
a[a$Anchoring != "NO",]$Anchoring <- "YES"
a <- a[!duplicated(a$Name.Date),]
#Diving
div <- na.omit(data.table(Name.Date = S$Name.Date, Diving = S$Diving))
div[div$Diving != "NO"]$Diving <- "YES"
div <- div[!duplicated(div$Name.Date),]

tempdd <- dd
dd <- tempdd

#Merge the files
dd <- merge(dd, dfs, by = "Name.Date", all.x = T)
dd <- merge(dd, depth, by = "Name.Date", all.x = T)
dd <- merge(dd, st, by = "Name.Date", all.x = T)
dd <- merge(dd, fiveT, by = "Name.Date", all.x = T)
dd <- merge(dd, tenT, by = "Name.Date", all.x = T)
dd <- merge(dd, soe, by = "Name.Date", all.x = T)
dd <- merge(dd, storm, by = "Name.Date", all.x = T)
dd <- merge(dd, dynamite, by = "Name.Date", all.x = T)
dd <- merge(dd, pf, by = "Name.Date", all.x = T)
dd <- merge(dd, tds, by = "Name.Date", all.x = T)
dd <- merge(dd, sp, by = "Name.Date", all.x = T)
dd <- merge(dd, ip, by = "Name.Date", all.x = T)
dd <- merge(dd, cf, by = "Name.Date", all.x = T)
dd <- merge(dd, isp, by = "Name.Date", all.x = T)
dd <- merge(dd, ipe, by = "Name.Date", all.x = T)
dd <- merge(dd, hif, by = "Name.Date", all.x = T)
dd <- merge(dd, hic, by = "Name.Date", all.x = T)
dd <- merge(dd, afc, by = "Name.Date", all.x = T)
dd <- merge(dd, a, by = "Name.Date", all.x = T)
dd <- merge(dd, div, by = "Name.Date", all.x = T)
#############################################
#Final clean of dd columns
dd$Year <- as.numeric(dd$Year)
require(lubridate)
dd$Date <- yday(dd$Date)
dd$Depth <- as.numeric(dd$Depth)
dd[is.na(dd$Colony.Bleaching.Percent)]$Colony.Bleaching.Percent <- 0
dd[is.na(dd$Population.Bleaching.Percent)]$Population.Bleaching.Percent <- 0
dd[is.na(dd$Grouper.Total)]$Grouper.Total <- 0
dd[is.na(dd$Parrot)]$Parrot <- 0
dd[is.na(dd$Bumphead.Parrot)]$Bumphead.Parrot <- 0
dd[is.na(dd$Humphead.Wrasse)]$Humphead.Wrasse <- 0
dd[is.na(dd$Snapper)]$Snapper <- 0
dd[is.na(dd$Moray.Eel)]$Moray.Eel <- 0
dd[is.na(dd$Trash.General)]$Trash.General <- 0
dd[is.na(dd$Trash.Fish.Nets)]$Trash.Fish.Nets <- 0
dd[dd$Water.temp.at.surface == 0]$Water.temp.at.surface <- NA
dd[dd$Water.temp.at.5m == 0]$Water.temp.at.5m <- NA
dd[dd$Water.temp.at.10m == 0]$Water.temp.at.10m <- NA
#clean factors 
#######Tough judgement call for sheltered######
#is 0 sheltered or exposed
dd$Sheltered.or.exposed. <- ifelse(dd$Sheltered.or.exposed. == "" | dd$Sheltered.or.exposed. == 0 | dd$Sheltered.or.exposed. == "NONE", "EXPOSED",dd$Sheltered.or.exposed.)
dd$Sheltered.or.exposed. <- ifelse(dd$Sheltered.or.exposed. == "SHELTERED" | dd$Sheltered.or.exposed. == "A" | dd$Sheltered.or.exposed. == "ALWAYS", "ALWAYS SHELTERED", dd$Sheltered.or.exposed.)
dd$Sheltered.or.exposed. <- as.factor(dd$Sheltered.or.exposed.)
levels(dd$Sheltered.or.exposed.) <- c("ALWAYS SHELTERED", "SOMETIMES", "EXPOSED")
dd$Any.major.storms.in.last.years. <- ifelse(dd$Any.major.storms.in.last.years. == "" | dd$Any.major.storms.in.last.years. == 0 | dd$Any.major.storms.in.last.years. == "NO", F, dd$Any.major.storms.in.last.years.)
dd$Any.major.storms.in.last.years. <- ifelse(dd$Any.major.storms.in.last.years. == "YES", T, dd$Any.major.storms.in.last.years.)
dd$Any.major.storms.in.last.years. <- ifelse(dd$Any.major.storms.in.last.years. == "UNKNOWN", NA, dd$Any.major.storms.in.last.years.)
dd$Any.major.storms.in.last.years. <- as.factor(dd$Any.major.storms.in.last.years.)
dd$Dynamite.Fishing. <- as.factor(ifelse(dd$Dynamite.Fishing. == 0 | dd$Dynamite.Fishing. == "", "NONE", dd$Dynamite.Fishing.))
levels(dd$Dynamite.Fishing.) <- c("HIGH", "MODERATE","LOW","NONE")
dd$Poison.Fishing. <- as.factor(ifelse(dd$Poison.Fishing. == "" | dd$Poison.Fishing. == 0, "NONE", dd$Poison.Fishing.))
levels(dd$Poison.Fishing.) <- c("HIGH","MODERATE","LOW","NONE")
dd$Tourist.diving.snorkeling <- as.factor(ifelse(dd$Tourist.diving.snorkeling == "", "NONE",dd$Tourist.diving.snorkeling))
levels(dd$Tourist.diving.snorkeling) <- c("HIGH","MODERATE","LOW","NONE")
dd$Sewage.pollution <- as.factor(ifelse(dd$Sewage.pollution == "" | dd$Sewage.pollution == 0, "NONE",dd$Sewage.pollution))
levels(dd$Sewage.pollution) <- c("HIGH","MODERATE","LOW","NONE")
dd$Industrial.pollution <- as.factor(ifelse(dd$Industrial.pollution == "" | dd$Industrial.pollution == 0, "NONE",dd$Industrial.pollution))
levels(dd$Industrial.pollution) <- c("HIGH","MODERATE","LOW","NONE")
dd$Commercial.fishing <- as.factor(ifelse(dd$Commercial.fishing == "" | dd$Commercial.fishing == 0, "NONE",dd$Commercial.fishing))
levels(dd$Commercial.fishing) <- c("HIGH","MODERATE","LOW","NONE")
dd$Is.protection.enforced. <- as.factor(ifelse(dd$Is.protection.enforced. == "" | dd$Is.protection.enforced. == 0 | dd$Is.protection.enforced. == "SOMETIMES"
                                               | dd$Is.protection.enforced. == "SOME, INFORMALLY"| dd$Is.protection.enforced. == "SOMEWHAT, UNOFFICIALLY"| 
                                                 dd$Is.protection.enforced. == "SOME, UNOFFICIALLY", "NO", dd$Is.protection.enforced.))
levels(dd$Is.protection.enforced.) <- c("YES","NO")
dd$Is.site.protected. <- as.factor(ifelse(dd$Is.site.protected. == 0, T, F))
dd$Harvest.of.inverts.for.curio. <- as.factor(ifelse(dd$Harvest.of.inverts.for.curio. == "" | dd$Harvest.of.inverts.for.curio. == 0, "NONE",dd$Harvest.of.inverts.for.curio.))
levels(dd$Harvest.of.inverts.for.curio.) <- c("HIGH","MODERATE","LOW","NONE")
dd$Harvest.of.inverts.for.food. <- as.factor(ifelse(dd$Harvest.of.inverts.for.food. == "" | dd$Harvest.of.inverts.for.food. == 0, "NONE",dd$Harvest.of.inverts.for.food.))
levels(dd$Harvest.of.inverts.for.food.) <- c("HIGH","MODERATE","LOW","NONE")
dd$Aquarium.fish.collection <- as.factor(ifelse(dd$Aquarium.fish.collection == "" | dd$Aquarium.fish.collection == 0, "NONE",dd$Aquarium.fish.collection))
levels(dd$Aquarium.fish.collection) <- c("HIGH","MODERATE","LOW","NONE")
dd$Anchoring <- as.factor(ifelse(dd$Anchoring == 0, "NO",dd$Anchoring))
levels(dd$Anchoring) <- c("YES","NO")
dd$Diving <- as.factor(ifelse(dd$Diving == 0, "NO",dd$Diving))
levels(dd$Diving) <- c("YES","NO")

#write to file
write.csv(dd, "/Users/michaelberk/Documents/R Scripts/Reef Check/Q6.csv")


######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
#Analyze file
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
dd <- read.csv("/Users/michaelberk/Documents/R Scripts/Reef Check/Q6.csv")

#subset the data into first and second occurance
first <- subset(dd, dd$OccuranceNumber == 1)
second <- subset(dd, dd$OccuranceNumber == 2)
first <- first[first$Reef.Name %in% second$Reef.Name, ]

#create time from prior occurance
dd$Date <- as.Date(dd$Date)
#timeFromPrior <- as.numeric(second$Date - first$Date)
#second <- cbind(second, timeFromPrior)
#first <- cbind(first, timeFromPrior = rep(0, dim(first)[1]))
both <- rbind(first, second)

##############################################
#analyze first vs second
#Run lm() on all columns T1 and T2 value - finds out what is correlated with population bleaching
sameTime <- lm(Population.Bleaching.Percent ~ Is.protection.enforced., data = both) #"NO" Estimate Std. = 4.6250 ***
sameTime <- lm(Population.Bleaching.Percent ~ Is.site.protected., data = both) #"T" Estimate 1.7566 .
sameTime <- lm(Population.Bleaching.Percent ~ Commercial.fishing, data = both) #Moderate 3.8775 *
sameTime <- lm(Population.Bleaching.Percent ~ Industrial.pollution, data = both) #NONE 2.63534 *
sameTime <- lm(Population.Bleaching.Percent ~ Sewage.pollution, data = both) #MODERATE 3.982 *, NONE 2.346 .
sameTime <- lm(Population.Bleaching.Percent ~ Tourist.diving.snorkeling, data = both) #MODERATE 2.620 ., NONE 5.992 ***
sameTime <- lm(Population.Bleaching.Percent ~ Poison.Fishing., data = both) #NONE 2.5203 *
sameTime <- lm(Population.Bleaching.Percent ~ Dynamite.Fishing., data = both) #HIGH 8.433 *, NONE 2.411 .
sameTime <- lm(Population.Bleaching.Percent ~ Water.temp.at.surface, data = both) #1.1704 **
sameTime <- lm(Population.Bleaching.Percent ~ Water.temp.at.5m, data = both) #0.7776 .
sameTime <- lm(Population.Bleaching.Percent ~ Water.temp.at.10m, data = both) #1.3060 **
sameTime <- lm(Population.Bleaching.Percent ~ Sheltered.or.exposed., data = both) #ALWAYS SHELTERED 5.5607 ***, EXPOSED 2.8069 *
sameTime <- lm(Population.Bleaching.Percent ~ Depth..m., data = both) #No
sameTime <- lm(Population.Bleaching.Percent ~ Distance.from.shore..m., data = both) #NO
sameTime <- lm(Population.Bleaching.Percent ~ Trash.Fish.Nets, data = both) #1.6850 ***
sameTime <- lm(Population.Bleaching.Percent ~ Trash.General, data = both) #0.6780 *
sameTime <- lm(Population.Bleaching.Percent ~ Humphead.Wrasse, data = both) #NO
sameTime <- lm(Population.Bleaching.Percent ~ Snapper, data = both)  #NO
sameTime <- lm(Population.Bleaching.Percent ~ Moray.Eel, data = both) #NO
sameTime <- lm(Population.Bleaching.Percent ~ Parrot, data = both) #NO
sameTime <- lm(Population.Bleaching.Percent ~ Bumphead.Parrot, data = both)  #NO
sameTime <- lm(Population.Bleaching.Percent ~ Grouper.Total, data = both) #NO
sameTime <- lm(Population.Bleaching.Percent ~ Colony.Bleaching.Percent, data = both) # 0.1718 ***
summary(sameTime)


#Run lm() on all columns (T2 ~ T1) - finds out what predicts bleaching
difTime <- lm(second$Population.Bleaching.Percent ~ Is.protection.enforced., data = first) #no
difTime <- lm(second$Population.Bleaching.Percent ~ Is.site.protected., data = first) #no
difTime <- lm(second$Population.Bleaching.Percent ~ Commercial.fishing, data = first) #LOW -4.2227 *
difTime <- lm(second$Population.Bleaching.Percent ~ Industrial.pollution, data = first) #LOW -4.9823 *, NONE -3.0419 .
difTime <- lm(second$Population.Bleaching.Percent ~ Sewage.pollution, data = first) #LOW -3.719 *, MODERATE -5.501 *, NONE -2.926 .
difTime <- lm(second$Population.Bleaching.Percent ~ Tourist.diving.snorkeling, data = first) #HIGH -3.5057 ., LOW -5.2151 **, MODERATE -4.1406 *
difTime <- lm(second$Population.Bleaching.Percent ~ Poison.Fishing., data = first) #LOW -6.314 *, MODERATE -7.865 *, NONE -3.518 *
difTime <- lm(second$Population.Bleaching.Percent ~ Dynamite.Fishing., data = first) #LOW -7.471 **, MODERATE -4.958 ., NONE -3.849 *
difTime <- lm(second$Population.Bleaching.Percent ~ Water.temp.at.surface, data = first) #no
difTime <- lm(second$Population.Bleaching.Percent ~ Water.temp.at.5m, data = first) #no 
difTime <- lm(second$Population.Bleaching.Percent ~ Water.temp.at.10m, data = first) #no
difTime <- lm(second$Population.Bleaching.Percent ~ Sheltered.or.exposed., data = first) #SOMETIMES -2.991 .
difTime <- lm(second$Population.Bleaching.Percent ~ Depth..m., data = first) #no
difTime <- lm(second$Population.Bleaching.Percent ~ Distance.from.shore..m., data = first) #3.026e-05 .
difTime <- lm(second$Population.Bleaching.Percent ~ Trash.Fish.Nets, data = first) #0.9428 *
difTime <- lm(second$Population.Bleaching.Percent ~ Trash.General, data = first) #no
difTime <- lm(second$Population.Bleaching.Percent ~ Humphead.Wrasse, data = first) #NO
difTime <- lm(second$Population.Bleaching.Percent ~ Snapper, data = first)  #NO
difTime <- lm(second$Population.Bleaching.Percent ~ Moray.Eel, data = first) #NO
difTime <- lm(second$Population.Bleaching.Percent ~ Parrot, data = first) # 0.06043 *
difTime <- lm(second$Population.Bleaching.Percent ~ Bumphead.Parrot, data = first)  #NO
difTime <- lm(second$Population.Bleaching.Percent ~ Grouper.Total, data = first)  #NO
difTime <- lm(second$Population.Bleaching.Percent ~ Colony.Bleaching.Percent, data = first) #0.04841 *
difTime <- lm(second$Population.Bleaching.Percent ~ Population.Bleaching.Percent, data = first) #0.12261 ***
difTime <- lm(second$Population.Bleaching.Percent ~ timeFromPrior, data = second) #NO
summary(difTime)

###########################################################
#third occurance and first occurance

#subset the data into first and third occurance
first <- subset(dd, dd$OccuranceNumber == 1)
third <- subset(dd, dd$OccuranceNumber == 3)
first <- first[first$Reef.Name %in% third$Reef.Name, ]

#create time from prior occurance
first$Date <- as.Date(first$Date)
third$Date <- as.Date(third$Date)
timeFromPrior <- as.numeric(third$Date - first$Date)
third <- cbind(third, timeFromPrior)
first <- cbind(first, timeFromPrior = rep(0, dim(first)[1]))
both <- rbind(first, third)

#Run lm() on all columns (T3 ~ T1) - find what predicts bleaching more long term
difTime <- lm(third$Population.Bleaching.Percent ~ Is.protection.enforced., data = first) #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Is.site.protected., data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Commercial.fishing, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Industrial.pollution, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Sewage.pollution, data = first) #"MODERATE" -6.14 . 
difTime <- lm(third$Population.Bleaching.Percent ~ Tourist.diving.snorkeling, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Poison.Fishing., data = first) #"LOW" -7.727 .
difTime <- lm(third$Population.Bleaching.Percent ~ Dynamite.Fishing., data = first) #"LOW" -8.575 ., "MODERATE" -7.685 .
difTime <- lm(third$Population.Bleaching.Percent ~ Water.temp.at.5m, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Water.temp.at.surface, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Sheltered.or.exposed., data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Water.temp.at.10m, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Depth..m., data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Distance.from.shore..m., data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Trash.Fish.Nets, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Trash.General, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Humphead.Wrasse, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Snapper, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Moray.Eel, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Parrot, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Bumphead.Parrot, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Grouper.Total, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Colony.Bleaching.Percent, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ Population.Bleaching.Percent, data = first)  #no dig vars
difTime <- lm(third$Population.Bleaching.Percent ~ timeFromPrior, data = third)  #no dig vars
summary(difTime)

###########################################################
#Multiple regression (bleaching ~ everything) - using dataset that has all instances of bleaching
#Same time
everything <- lm(Population.Bleaching.Percent ~ Latitude+
                   Year+Date+
                   Colony.Bleaching.Percent+
                   Grouper.Total+Bumphead.Parrot+
                   Parrot+Humphead.Wrasse+Moray.Eel+
                   Snapper+Trash.General+Trash.Fish.Nets+
                   OccuranceNumber+Distance.from.shore..m.+Depth..m.+
                   Water.temp.at.surface+Water.temp.at.5m+Water.temp.at.10m+
                   Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                   Poison.Fishing.+Sewage.pollution+
                   Industrial.pollution+Commercial.fishing+Is.site.protected.+
                   Is.protection.enforced.+Harvest.of.inverts.for.food.+Harvest.of.inverts.for.curio.+
                   Aquarium.fish.collection+Anchoring+Diving, data = dd)
#sig values:
#Trash.General  7.838e-01 *
#Trash.Fishnets 1.060e+00 **
#Water.temp.at.surface 2.264e+000 ***
#Water.temp.at.5m -3.620e+00 **
#Water.temp.at.10m 2.870e+00 *
#Is.protection.enforcedNO 9.029e+00 .
###########################################################
#vif 
library(car)
#vif same time
vif(everything)
#below variables are the best variables bsaed on vif
everything <- lm(Population.Bleaching.Percent ~ Latitude+
                   Date+
                   Colony.Bleaching.Percent+
                   Grouper.Total+Bumphead.Parrot+
                   Parrot+Humphead.Wrasse+Moray.Eel+
                   Snapper+Trash.General+Trash.Fish.Nets+
                   Distance.from.shore..m.+Depth..m.+
                   Water.temp.at.surface+Water.temp.at.5m+Water.temp.at.10m+
                   Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                   Poison.Fishing.+Sewage.pollution+
                   Industrial.pollution+Commercial.fishing+
                   Is.protection.enforced.+Harvest.of.inverts.for.food.+Harvest.of.inverts.for.curio.+
                   Aquarium.fish.collection+Anchoring+Diving, data = dd)
#Date: neg 0.04581 * 
#Colony Bleach: pos < 2e-16 ***
#Sewage Pollution MODERATE: neg 0.01211 *
#Harvest for food MODERATE: neg 0.06726 .
#Harvest for food NONE: neg 0.00603 ** 
#Diving YES: neg 0.00130 **

#vif T2 ~ T1 (with fish counts)
vif(everything)
everything <- lm(second$Population.Bleaching.Percent ~ Latitude+
                   Year+
                   Colony.Bleaching.Percent+
                   Grouper.Total+Bumphead.Parrot+
                   Parrot+Humphead.Wrasse+Moray.Eel+
                   Snapper+Trash.General+Trash.Fish.Nets+
                   Distance.from.shore..m.+Depth..m.+
                   Water.temp.at.10m+
                   Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                   Sewage.pollution+
                   Industrial.pollution+Commercial.fishing+
                   Is.protection.enforced.+Harvest.of.inverts.for.food.+
                   Aquarium.fish.collection+Anchoring+Diving, data = first)

#Any.major.storms.in.last.years.TRUE   4.468e+00 *
#Trash.Fish.Nets                       1.436e+00 *
#Humphead.Wrasse                       1.200e+01 ***
#R-squaredL 0.1916
everything <- lm(second$Population.Bleaching.Percent ~ Latitude+
                   Year+
                   Colony.Bleaching.Percent+
                   Trash.General+Trash.Fish.Nets+
                   Distance.from.shore..m.+Depth..m.+
                   Water.temp.at.10m+
                   Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                   Sewage.pollution+
                   Industrial.pollution+Commercial.fishing+
                   Is.protection.enforced.+Harvest.of.inverts.for.food.+
                   Aquarium.fish.collection+Anchoring+Diving, data = first)
#Trash.Fish.Nets                       1.384e+00 *
#R-squared: 0.1296

library('mgcv')
everything <- gam(second$Population.Bleaching.Percent ~ Latitude+
                    Year+
                    s(Colony.Bleaching.Percent, sp = 0.1)+
                    Grouper.Total+Bumphead.Parrot+
                    Parrot+Moray.Eel+
                    Snapper+Trash.General+Trash.Fish.Nets+
                    Distance.from.shore..m.+Depth..m.+
                    s(Water.temp.at.10m, sp = .1)+
                    Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                    Sewage.pollution+
                    Industrial.pollution+Commercial.fishing+
                    Is.protection.enforced.+Harvest.of.inverts.for.food.+
                    Aquarium.fish.collection+Anchoring+Diving, data = first, family = "gaussian")

#####################################
#GAM analysis
library('mgcv')
everything <- gam(second$Population.Bleaching.Percent ~ s(Latitude, sp = .025)+
                    s(Grouper.Total, sp=.025)+s(Bumphead.Parrot,sp=.025)+
                    s(Parrot,sp = .025)+s(Moray.Eel,sp=.025)+
                    s(Snapper,sp=.025)+
                    s(Year, sp=.025)+
                    s(Colony.Bleaching.Percent, sp = 0.025)+
                    s(Trash.General, sp = .025) +s(Trash.Fish.Nets, sp = .025)+
                    s(Distance.from.shore..m., sp = .025)+s(Depth..m.,sp = .025)+
                    s(Water.temp.at.10m, sp = .025)+
                    Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                    Sewage.pollution+
                    Industrial.pollution+Commercial.fishing+
                    Is.protection.enforced.+Harvest.of.inverts.for.food.+
                    Aquarium.fish.collection+Anchoring+Diving, data = first, family = "gaussian")
#s(Trash.Fish.Nets)          4.015 *


#without fish count
everything <- gam(second$Population.Bleaching.Percent ~ s(Latitude, sp = .025)+
                    s(Year, sp=.025)+
                    s(Colony.Bleaching.Percent, sp = .025)+s(Distance.from.shore..m., sp = .025)+
                    s(Trash.General, sp = .025) +s(Trash.Fish.Nets, sp = .025)+
                    s(Depth..m.,sp = .005)+
                    s(Water.temp.at.10m, sp = .005)+
                    Sheltered.or.exposed.+Any.major.storms.in.last.years.+Dynamite.Fishing.+
                    Sewage.pollution+
                    Industrial.pollution+Commercial.fishing+
                    Is.protection.enforced.+Harvest.of.inverts.for.food.+
                    Aquarium.fish.collection+Anchoring+Diving, data = first, family = "gaussian")
#s(Trash.Fish.Nets)          4.086  4.865 2.114  0.0575 .
#s(Depth..m.)                7.489  8.359 1.675  0.0850 .

###########################################################
#check differneces between first and second
#see if there is trend in averages
mean(na.omit(first$Population.Bleaching.Percent)) - mean(na.omit(second$Population.Bleaching.Percent)) #goes down 6.364566

#count the number of improvmenets and worsenings
subPop <- first$Population.Bleaching.Percent - second$Population.Bleaching.Percent
subCol <- first$Colony.Bleaching.Percent - second$Colony.Bleaching.Percent
improvedPop <- ifelse(subPop > 0, F, T) #F = 512, T = 249
improvedCol <- ifelse(subCol > 0, F, T) #F = 512, T = 249


#check how many reefs recovered completely (bleaching = 0)
table(second$Population.Bleaching.Percent == 0 & second$Colony.Bleaching.Percent == 0)
table(second$Population.Bleaching.Percent < 10 & second$Colony.Bleaching.Percent < 10)


#count the percentage of reefs bleached for each year (use dd and ddTotal) - a = 1999, t = 2017
dd$Date <- as.Date(dd$Date)
NumberOfBleaches <- ifelse(dd$Population.Bleaching.Percent > 0, T, F)
a <- length(subset(dd, (dd$Date > as.Date("1998-1-1")) & (dd$Date < as.Date("1999-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("1998-1-1")) & (totaldd$Date < as.Date("1999-1-1")))$Population.Bleaching.Percent)
b <- length(subset(dd, (dd$Date > as.Date("1999-1-1")) & (dd$Date < as.Date("2000-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("1999-1-1")) & (totaldd$Date < as.Date("2000-1-1")))$Population.Bleaching.Percent)
c <- length(subset(dd, (dd$Date > as.Date("2000-1-1")) & (dd$Date < as.Date("2001-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2000-1-1")) & (totaldd$Date < as.Date("2001-1-1")))$Population.Bleaching.Percent)
d <- length(subset(dd, (dd$Date > as.Date("2001-1-1")) & (dd$Date < as.Date("2002-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2001-1-1")) & (totaldd$Date < as.Date("2002-1-1")))$Population.Bleaching.Percent)
e <- length(subset(dd, (dd$Date > as.Date("2002-1-1")) & (dd$Date < as.Date("2003-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2002-1-1")) & (totaldd$Date < as.Date("2003-1-1")))$Population.Bleaching.Percent)
f <- length(subset(dd, (dd$Date > as.Date("2003-1-1")) & (dd$Date < as.Date("2004-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2003-1-1")) & (totaldd$Date < as.Date("2004-1-1")))$Population.Bleaching.Percent)
g <- length(subset(dd, (dd$Date > as.Date("2004-1-1")) & (dd$Date < as.Date("2005-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2004-1-1")) & (totaldd$Date < as.Date("2005-1-1")))$Population.Bleaching.Percent)
h <- length(subset(dd, (dd$Date > as.Date("2005-1-1")) & (dd$Date < as.Date("2006-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2005-1-1")) & (totaldd$Date < as.Date("2006-1-1")))$Population.Bleaching.Percent)
i <- length(subset(dd, (dd$Date > as.Date("2006-1-1")) & (dd$Date < as.Date("2007-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2006-1-1")) & (totaldd$Date < as.Date("2007-1-1")))$Population.Bleaching.Percent)
j <- length(subset(dd, (dd$Date > as.Date("2007-1-1")) & (dd$Date < as.Date("2008-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2007-1-1")) & (totaldd$Date < as.Date("2008-1-1")))$Population.Bleaching.Percent)
k <- length(subset(dd, (dd$Date > as.Date("2008-1-1")) & (dd$Date < as.Date("2009-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2008-1-1")) & (totaldd$Date < as.Date("2009-1-1")))$Population.Bleaching.Percent)
l <- length(subset(dd, (dd$Date > as.Date("2009-1-1")) & (dd$Date < as.Date("2010-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2009-1-1")) & (totaldd$Date < as.Date("2010-1-1")))$Population.Bleaching.Percent)
m <- length(subset(dd, (dd$Date > as.Date("2010-1-1")) & (dd$Date < as.Date("2011-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2010-1-1")) & (totaldd$Date < as.Date("2011-1-1")))$Population.Bleaching.Percent)
n <- length(subset(dd, (dd$Date > as.Date("2011-1-1")) & (dd$Date < as.Date("2012-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2011-1-1")) & (totaldd$Date < as.Date("2012-1-1")))$Population.Bleaching.Percent)
o <- length(subset(dd, (dd$Date > as.Date("2012-1-1")) & (dd$Date < as.Date("2013-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2011-1-1")) & (totaldd$Date < as.Date("2012-1-1")))$Population.Bleaching.Percent)
p <- length(subset(dd, (dd$Date > as.Date("2013-1-1")) & (dd$Date < as.Date("2014-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2013-1-1")) & (totaldd$Date < as.Date("2014-1-1")))$Population.Bleaching.Percent)
q <- length(subset(dd, (dd$Date > as.Date("2014-1-1")) & (dd$Date < as.Date("2015-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2014-1-1")) & (totaldd$Date < as.Date("2015-1-1")))$Population.Bleaching.Percent)
r <- length(subset(dd, (dd$Date > as.Date("2015-1-1")) & (dd$Date < as.Date("2016-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2015-1-1")) & (totaldd$Date < as.Date("2016-1-1")))$Population.Bleaching.Percent)
s <- length(subset(dd, (dd$Date > as.Date("2016-1-1")) & (dd$Date < as.Date("2017-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2016-1-1")) & (totaldd$Date < as.Date("2017-1-1")))$Population.Bleaching.Percent)
t <- length(subset(dd, (dd$Date > as.Date("2017-1-1")) & (dd$Date < as.Date("2018-1-1")))$Population.Bleaching.Percent)/
  length(subset(totaldd, (totaldd$Date > as.Date("2017-1-1")) & (totaldd$Date < as.Date("2018-1-1")))$Population.Bleaching.Percent)
graphDF <- data.frame(Year = c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012,2013,2014,2015,2016,2017),
                         PercentageOfReefs = c(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
library(ggplot2)
ggplot(graphDF, aes(x = Year, y = PercentageOfReefs)) + 
  labs(title = "Percentage of Reefs That Exhibit Bleaching", x= "Year", y= "Total Reefs (%)")+ 
  geom_point(shape = 15) + geom_line() 
#plot(graphDF, main = "Percentage of Reefs Exhibiting Bleaching")

###########################################################
#create a map of all of the datapoints collected
#THIS DOES NOT WORK - THE LAT AND LON HAVE LOTS OF ERRORS
library(ggmap)
library(maps)
latD <- ifelse(dd$Latitude.Cardinal.Direction == "S", -(as.numeric(dd$Latitude.Degrees)), as.numeric(dd$Latitude.Degrees))

lat<-as.numeric(paste0(latD,".", dd$Latitude.Minutes, dd$Latitude.Seconds*100))*90

longD <- ifelse(dd$Longitude.Cardinal.Direction == "W", -(as.numeric(dd$Longitude.Degrees)), as.numeric(dd$Longitude.Degrees.x))

long<-as.numeric(paste0(longD,".", dd$Longitude.Minutes, dd$Longitude.Seconds*100))*180


pointsDF <- data.frame(as.numeric(long),as.numeric(lat))
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")
base_world_messy <- p + geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup
mapPoints <- base_world + geom_point(aes(as.numeric(long),as.numeric(lat)), data = pointsDF)
mapPoints



