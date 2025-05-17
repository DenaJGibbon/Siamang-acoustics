# Tutorial: https://ourcodingclub.github.io/tutorials/mixed-models/#what
library(tidyr)
library(ggpubr)
library(stringr)
library(lubridate)
library(hms)
library(dplyr)


# Part 1a. Read in and plot detections ----------------------------------------------
# Read in the BirdNET detection files
SikundurFiles <- list.files("data/evaluationfiles/", pattern = '.csv', recursive=T, full.names=T)
length(SikundurFiles) # 5249

# Initialize an empty list to store data frames
SikundurFilesList <- list()

# Loop through files and process them
for(a in 1:length(SikundurFiles)) {
  print(a)
  TempFile <- read.csv(SikundurFiles[a])
  TempName <- basename(TempFile$Filename)
  TempFile$TempName <- TempName
  # Store the processed data frame in the list
  SikundurFilesList[[a]] <- TempFile
}

# Combine all data frames from the list into a single data frame
SikundurFilesCombined <- bind_rows(SikundurFilesList)

# View the result
head(SikundurFilesCombined)
tail(SikundurFilesCombined)

TempVals <-str_split_fixed(SikundurFilesCombined$TempName,pattern='_',n=6)

SikundurFilesCombined$Unit <- TempVals[,3]
SikundurFilesCombined$Date <- as.Date(TempVals[,4], format="%Y%m%d")
SikundurFilesCombined$Time <- str_split_fixed(SikundurFilesCombined$Start.Time,' ',n=2)[,2]
SikundurFilesCombined$Time <- str_replace_all(SikundurFilesCombined$Time ,':','')

SikundurFilesCombined$Month <- as.numeric(substr(SikundurFilesCombined$Date,6,7))

# Need to adjust hour for Unit003
unique(SikundurFilesCombined$Unit)

# Standardize Unit names
SikundurFilesCombined$Unit_clean <- SikundurFilesCombined$Unit %>%
  tolower() %>%                 # Convert to lowercase
  gsub("unit ", "unit", .) %>%   # Remove space after "unit"
  gsub("unit_", "unit", .) %>%   # Remove underscore after "unit"
  gsub("_000$", "", .)           # Remove "_000" if present

# Check unique values after cleaning
unique(SikundurFilesCombined$Unit_clean)

#What is the range of confidence scores?
range(SikundurFilesCombined$Confidence)

# Update class

SikundurFilesCombined$VerifiedClass <- ifelse(SikundurFilesCombined$Positive.or.Negative == 'Positive',
       SikundurFilesCombined$Species, 'NOISE')

table(SikundurFilesCombined$VerifiedClass )

# Plot for all confidence
gghistogram(data=SikundurFilesCombined, x='Date', facet.by = 'VerifiedClass')

head(SikundurFilesCombined)
# part 1b. Add files without detections --------------------------------------------

AllProcessedFiles <- basename(list.files('data/DEPLOYMENT/',
                                         recursive = T))

# Step 1: Extract core identifier from SikundurFilesDF$Filename
# Assuming that processed file names follow the pattern: Unit001_YYYYMMDD_HHMMSS.BirdNET.selection.table.txt
SikundurFilesCombined$CoreID <-
  str_split_fixed(SikundurFilesCombined$Filename,
                  pattern = '_', n=3)[,3]

IdSplits<-
  str_split_fixed(SikundurFilesCombined$CoreID,
                  pattern = '_', 4)

WavNames <-
  paste(IdSplits[,1],IdSplits[,2],IdSplits[,3],sep = '_')

# Step 2: Extract base names from AllProcessedFiles (remove extension for matching)
ProcessedIDs <- str_remove(AllProcessedFiles, "\\.BirdNET\\.selection\\.table\\.txt")

ProcessedButNoDetections <-
  ProcessedIDs[-which(ProcessedIDs %in% unique(WavNames) )]

length(ProcessedButNoDetections)

Filename<-
  str_split_fixed(ProcessedButNoDetections,
                  pattern = '_', 4)

Unit_clean <- Filename[,1]
Date <- Filename[,2]
Time <- Filename[,3]

Date <- as.Date(Date, format="%Y%m%d")
Month <- as.numeric(substr(Date,6,7))

CoreID <- ProcessedButNoDetections

CombinedNoDetections <-
  cbind.data.frame(Date,Month,Time,Unit_clean,CoreID)

CombinedNoDetections$VerifiedClass <- 'NA'

CombinedNoDetections$Unit_clean <- CombinedNoDetections$Unit_clean %>%
  tolower()


SikundurFilesCombined <-
  SikundurFilesCombined[,c("Date","Month","Time", "Unit_clean","VerifiedClass","CoreID" )]

CombinedAllDetects <- rbind.data.frame(SikundurFilesCombined,CombinedNoDetections)

head(CombinedAllDetects)

write.csv(CombinedAllDetects,'data/CombinedAllDetects.csv',row.names = F)
#CombinedAllDetects <-read.csv('data/CombinedAllDetects.csv')

# Part 2. Match recorder ID to location --------------------------

# Load location data
LocationData <- read.csv('data/LOKASI SIKUNDUR.csv')
head(LocationData)

LocationData$Unit <- LocationData$Unit %>%
  tolower()

# Format dates in same format as Swift output
TempDatesStart <- str_split_fixed(LocationData$First.file.date..UTC.,pattern = '/', n=3)
LocationData$First.file.date_reformat <-  as.numeric(paste(TempDatesStart[,3],TempDatesStart[,2],TempDatesStart[,1], sep=''))

TempDatesEnd <- str_split_fixed(LocationData$Last.file.date..UTC.,pattern = '/', n=3)
LocationData$Last.file.date_reformat <- as.numeric( paste(TempDatesEnd[,3],TempDatesEnd[,2],TempDatesEnd[,1], sep=''))

# Now read in selection table data
head(CombinedAllDetects)

# We have Recorder and Date which we can use to match with the location data
# In location data want to match up with the corresponding Point.Name

# We got stuck on the subsetting by date and we want to match point name
UniquePointNames <- unique(LocationData$Point.Name)

SikundurFilesDF <- data.frame()

for( i in 1:length(UniquePointNames)){
  TempLocationData  <- subset(LocationData,Point.Name == UniquePointNames[i])

  for(j in 1:nrow(TempLocationData)){

    TempLocationDataSingle <-TempLocationData[j,]

    SikundurFilesSubsetUnit <-
      subset(CombinedAllDetects,Unit_clean==TempLocationData[j,]$Unit[1])

    SikundurFilesSubsetUnit$Date <-  str_replace_all(SikundurFilesSubsetUnit$Date,'-','')

    # Filter for dates within the range
    FilteredSikundur <- SikundurFilesSubsetUnit %>%
      filter(Date >= TempLocationDataSingle$First.file.date_reformat & Date <= TempLocationDataSingle$Last.file.date_reformat)

    if(nrow(FilteredSikundur) > 0){
    FilteredSikundur$point.name <- TempLocationDataSingle$Point.Name
    FilteredSikundur$LAT..decimal.degrees. <- TempLocationDataSingle$LAT..decimal.degrees.
    FilteredSikundur$LON..decimal.degrees. <- TempLocationDataSingle$LON..decimal.degrees.

    SikundurFilesDF <- rbind.data.frame(SikundurFilesDF,FilteredSikundur )
    }

  }
}

nrow(SikundurFilesDF)

unique(SikundurFilesDF$point.name)

# Part 4. Read in weather data --------------------------------------------
WeatherData <-
  read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/Weather_Data_Sikundur.csv',fileEncoding = "UTF-8")

# Convert month names to numeric values
WeatherData$month_numeric <- match(WeatherData$Months, month.name)

# Zero-pad the days to two digits
WeatherData$month_numeric  <- sprintf("%02d", WeatherData$month_numeric )

# Zero-pad the days to two digits
WeatherData$Day <- sprintf("%02d", WeatherData$Date)

# View the result
WeatherData$Day

# Make same format
WeatherData$Date_adjusted <-
  paste(WeatherData$Year,WeatherData$month_numeric,WeatherData$Day,sep='')
WeatherData$Date_adjusted

SikundurFilesDFAddWeather <- left_join(SikundurFilesDF, WeatherData,
                                       by = c("Date" = "Date_adjusted"))

# Check output
head(SikundurFilesDFAddWeather)

write.csv(SikundurFilesDFAddWeather,'data/SikundurFilesDFAddWeather.csv',row.names = F)

# Convert the 't_max' column to numeric format to ensure calculations are accurate.
SikundurFilesDFAddWeather$t_max <- as.numeric(SikundurFilesDFAddWeather$t_max)

# Aggregate data by Date, Common.Name, and point.name
SikundurFilesDFAddWeather_sumdetections <- SikundurFilesDFAddWeather %>%
  group_by(Date, VerifiedClass, point.name) %>%  # Group data by Date, species name, and point name
  summarise(
    Detection_Count = n(),       # Count the number of detections per group
    t_max = max(t_max),          # Retain the maximum temperature of the day
    t_min = max(t_min),          # Retain the minimum temperature of the day
    precip_am = max(precip_am),  # Retain the maximum morning precipitation
    precip_pm = max(precip_pm),  # Retain the maximum afternoon precipitation
    .groups = "drop"             # Ungroup after summarizing to prevent unexpected behavior
  )

# View the last few rows of the summarized dataset
tail(SikundurFilesDFAddWeather_sumdetections)

# View the first few rows of the summarized dataset
head(SikundurFilesDFAddWeather_sumdetections)

# Scatter plot: Relationship between morning precipitation and detection counts for each species
ggpubr::ggscatter(
  data = SikundurFilesDFAddWeather_sumdetections,
  color = 'VerifiedClass',      # Color points by species
  x = 'precip_am',            # X-axis: Morning precipitation
  y = 'Detection_Count',      # Y-axis: Detection count
  facet.by = 'VerifiedClass',   # Create separate plots for each species
  scales = 'free',            # Allow different scales per species
  add = c("reg.line")         # Add a regression line to each plot
)

# Scatter plot: Relationship between afternoon precipitation and detection counts for each species
ggpubr::ggscatter(
  data = SikundurFilesDFAddWeather_sumdetections,
  color = 'VerifiedClass',      # Color points by species
  x = 'precip_pm',            # X-axis: Afternoon precipitation
  y = 'Detection_Count',      # Y-axis: Detection count
  facet.by = 'VerifiedClass',   # Create separate plots for each species
  scales = 'free',            # Allow different scales per species
  add = c("reg.line")         # Add a regression line to each plot
)

# Reshape data to a wide format, where each species gets its own column
SikundurFilesDFAddWeather_wide <- SikundurFilesDFAddWeather_sumdetections %>%
  pivot_wider(
    names_from = VerifiedClass,   # Each species becomes a separate column
    values_from = Detection_Count,  # Values are detection counts for each species
    values_fill = list(Detection_Count = 0)  # Fill missing values with 0
  )

# This is by date
head(SikundurFilesDFAddWeather_wide)

write.csv(SikundurFilesDFAddWeather_wide,'data/SikundurFilesDFAddWeather_wide_verifieddetects.csv',row.names = FALSE)

# Scatter plot: Relationship between Chainsaw and Gibbon detections
ggpubr::ggscatter(
  data = SikundurFilesDFAddWeather_wide,
  x = 'Chainsaw',  # X-axis: Chainsaw detections
  y = 'Gibbon',    # Y-axis: Gibbon detections
  scales = 'free', # Allow different scales
  add = c("reg.line")  # Add a regression line
)

ggpubr::ggscatter(
  data = SikundurFilesDFAddWeather_wide,
  x = 'Chainsaw',  # X-axis: Chainsaw detections
  y = 'Siamang',    # Y-axis: Gibbon detections
  scales = 'free', # Allow different scales
  add = c("reg.line")  # Add a regression line
)
