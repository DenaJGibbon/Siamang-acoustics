# Tutorial: https://ourcodingclub.github.io/tutorials/mixed-models/#what
library(tidyr)
library(ggpubr)
library(stringr)
library(lubridate)
library(hms)
library(dplyr)

# Version 3 focuses on 24 hour period

# Part 1. Read in and plot detections ----------------------------------------------
# Read in the BirdNET detection files
SikundurFiles <- list.files("/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/DEPLOYMENT/", pattern = '.txt', recursive=T, full.names=T)
length(SikundurFiles) # 5249

# Initialize an empty list to store data frames
SikundurFilesList <- list()

# Loop through files and process them
for(a in 1:length(SikundurFiles)) {
  print(a)

  TempFile <- read.delim(SikundurFiles[a])
  TempName <- basename(SikundurFiles[a])
  TempName <- str_split_fixed(TempName, pattern = '.BirdNET', n = 2)[, 1]
  TempFile$TempName <- TempName
  # Store the processed data frame in the list
  SikundurFilesList[[a]] <- TempFile
}

# Combine all data frames from the list into a single data frame
SikundurFilesCombined <- bind_rows(SikundurFilesList)

# View the result
head(SikundurFilesCombined)
tail(SikundurFilesCombined)
table(SikundurFilesCombined$Common.Name)

TempVals <-str_split_fixed(SikundurFilesCombined$TempName,pattern='_',n=3)

SikundurFilesCombined$Unit <- TempVals[,1]
SikundurFilesCombined$Date <- as.Date(TempVals[,2], format="%Y%m%d")

# Check result
head(SikundurFilesCombined$Date )

SikundurFilesCombined$Month <- as.numeric(substr(SikundurFilesCombined$Date,5,6))

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

# Plot for all confidence
gghistogram(data=SikundurFilesCombined, x='Date', facet.by = 'Common.Name')

# What if we focus on high confidence for now?
SikundurFilesCombinedHighConf <- subset(SikundurFilesCombined,Confidence >=0.99)

# Plot for high confidence
gghistogram(data=SikundurFilesCombined, x='Date', facet.by = 'Common.Name')
# NOTE still lots of false positives

# # Let's focus only on detections during the day
# DetectionsDaylightHours <- subset(SikundurFilesCombinedHighConf, Hour >=5 & Hour <=18)
# gghistogram(data=DetectionsDaylightHours, x='time_numeric', facet.by = 'Common.Name')


# Part 2. Place holder to remove false positives --------------------------

# Part 3. Match recorder ID to location --------------------------

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
head(SikundurFilesCombined)

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
      subset(SikundurFilesCombined,Unit_clean==TempLocationData[j,]$Unit[1])

    SikundurFilesSubsetUnit$Date <-  str_replace_all(SikundurFilesSubsetUnit$Date,'-','')

    # Filter for dates within the range
    FilteredSikundur <- SikundurFilesSubsetUnit %>%
      filter(Date >= TempLocationDataSingle$First.file.date_reformat & Date <= TempLocationDataSingle$Last.file.date_reformat)

    if(nrow(FilteredSikundur) > 0){
    FilteredSikundur$point.name <- TempLocationDataSingle$Point.Name


    SikundurFilesDF <- rbind.data.frame(SikundurFilesDF,FilteredSikundur )
    }

  }
}

nrow(SikundurFilesDF)

# Part 4. Read in weather data --------------------------------------------
WeatherData <-
  read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/Weather_Data_Sikundur.csv',fileEncoding = "UTF-8")

# Convert month names to numeric values
WeatherData$month_numeric <- match(WeatherData$Months, month.name)

# Display the result
WeatherData$month_numeric

# Zero-pad the days to two digits
WeatherData$Date <- sprintf("%02d", WeatherData$Date)

# View the result
WeatherData$Date

# Make same format
WeatherData$Date_adjusted <-
  paste(WeatherData$Year,WeatherData$month_numeric,WeatherData$Date,sep='')

# Combine with weather
SikundurFilesDFAddWeather <-
  merge(SikundurFilesDF,WeatherData, by.x='Date', by.y='Date_adjusted')

# Check output
head(SikundurFilesDFAddWeather)

# Combine for each date and common.name
SikundurFilesDFAddWeather_sumdetections <- SikundurFilesDFAddWeather %>%
  group_by(Date, Common.Name,point.name) %>%
  summarise(
    Detection_Count = n(),
    t_max = first(t_max),       # Keep max temperature
    t_min = first(t_min),       # Keep min temperature
    precip_am = first(precip_am),  # Keep morning precipitation
    precip_pm = first(precip_pm),  # Keep afternoon precipitation
    .groups = "drop"
  )

# View result
head(SikundurFilesDFAddWeather_sumdetections)

library(ggplot2)

# Convert Date to Date format
SikundurFilesDFAddWeather_sumdetections$Date <- as.Date(SikundurFilesDFAddWeather_sumdetections$Date, format = "%Y%m%d")

library(ggplot2)

# Convert Date to Date format
SikundurFilesDFAddWeather_sumdetections$Date <- as.Date(SikundurFilesDFAddWeather_sumdetections$Date, format = "%Y%m%d")

SikundurFilesDFAddWeather_rmvnocall <-
  subset(SikundurFilesDFAddWeather_sumdetections,Common.Name !='nocall')

# Create the faceted plot
ggplot(SikundurFilesDFAddWeather_rmvnocall, aes(x = Date)) +
  geom_col(aes(y = Detection_Count), position = "dodge") +  # Bar plot for detections
  geom_line(aes(y = precip_am * 10, group = 1), color = "blue", size = 1) +  # Line plot for rainfall
  geom_point(aes(y = precip_am * 10), color = "blue", size = 2) +  # Points for rainfall
  scale_y_continuous(
    name = "Detection Count",
    sec.axis = sec_axis(~ . / 10, name = "Rainfall (mm)")  # Secondary axis for rain
  ) +
  labs(title = "Detections and Rainfall Over Time",
       x = "Date",
       y = "Detections") +
  facet_wrap(~ Common.Name, scales = "free_y") +  # Facet by species
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
