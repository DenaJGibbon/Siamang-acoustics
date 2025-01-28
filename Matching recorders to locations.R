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

SikundurFilesCombined$Recorder <- str_split_fixed(SikundurFilesCombined$TempName, pattern = '_', n=3)[,1]
SikundurFilesCombined$Date <- as.numeric(str_split_fixed(SikundurFilesCombined$TempName, pattern = '_', n=3)[,2])

# We have Recorder and Date which we can use to match with the location data
# In location data want to match up with the corresponding Point.Name

# We got stuck on the subsetting by date and we want to match point name
UniquePointNames <- unique(LocationData$Point.Name)

for( i in 1:length(UniquePointNames)){
 TempLocationData  <- subset(LocationData,Point.Name == UniquePointNames[i])

 for(j in 1:nrow(TempLocationData)){

 SikundurFilesSubsetUnit <- subset(SikundurFilesCombined,Recorder==TempLocationData[j,]$Unit[1])

 SikundurFilesSubsetUnit[between(SikundurFilesSubsetUnit$Date,
         TempLocationData$First.file.date_reformat[1],
         TempLocationData$Last.file.date_reformat[1]),]

 between(SikundurFilesSubsetUnit$Date, TempLocationData$First.file.date_reformat,
         TempLocationData$Last.file.date_reformat)


 SikundurFilesSubsetDate <- subset(SikundurFilesSubsetUnit,
                                   Date >= TempLocationData[j,]$First.file.date_reformat )

 SikundurFilesSubsetDate <- subset(SikundurFilesSubsetDate,Date <= TempLocationData[j,]$Last.file.date_reformat)
 range(SikundurFilesSubsetDate$Date)

 }
}

