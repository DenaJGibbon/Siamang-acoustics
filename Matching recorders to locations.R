# Next meeting:
# We want to be able to move clips to new folders based on file names
# This is to verify the detections at night are false positive
# We want to see how many detections we are getting per hour
# Siamang calls are long and might be able to look at one per hour
# Want to start modeling

# Read in library
library(stringr)

# Load location data
LocationData <- read.csv('data/LOKASI SIKUNDUR.csv')
head(LocationData)

# Format dates in same format as Swift output
TempDatesStart <- str_split_fixed(LocationData$First.file.date..UTC.,pattern = '/', n=3)
LocationData$First.file.date_reformat <-  as.numeric(paste(TempDatesStart[,3],TempDatesStart[,2],TempDatesStart[,1], sep=''))

TempDatesEnd <- str_split_fixed(LocationData$Last.file.date..UTC.,pattern = '/', n=3)
LocationData$Last.file.date_reformat <- as.numeric( paste(TempDatesEnd[,3],TempDatesEnd[,2],TempDatesEnd[,1], sep=''))

# Now read in selection table data
SikundurFilesCombined <- read.csv('data/SikundurFilesCombined.csv')
head(SikundurFilesCombined)
nrow(SikundurFilesCombined)

SikundurFilesCombined$Recorder <- str_split_fixed(SikundurFilesCombined$TempName, pattern = '_', n=3)[,1]
SikundurFilesCombined$Date <- as.numeric(str_split_fixed(SikundurFilesCombined$TempName, pattern = '_', n=3)[,2])

# We have Recorder and Date which we can use to match with the location data
# In location data want to match up with the corresponding Point.Name

# We got stuck on the subsetting by date and we want to match point name
UniquePointNames <- unique(LocationData$Point.Name)

SikundurFilesDF <- data.frame()

for( i in 1:length(UniquePointNames)){
 TempLocationData  <- subset(LocationData,Point.Name == UniquePointNames[i])

 for(j in 1:nrow(TempLocationData)){

SikundurFilesSubsetUnit <- subset(SikundurFilesCombined,Recorder==TempLocationData[j,]$Unit[1])

SikundurFilesSubsetUnit$Point.Name <-  TempLocationData[which(TempLocationData$First.file.date_reformat %in%  SikundurFilesSubsetUnit$Date ),]$Point.Name

SikundurFilesDF <- rbind.data.frame(SikundurFilesDF,SikundurFilesSubsetUnit )

 }
}

SikundurFilesDF[1,]
