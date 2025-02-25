# Tutorial: https://ourcodingclub.github.io/tutorials/mixed-models/#what
library(tidyr)
library(ggpubr)
library(stringr)
library(lubridate)
library(hms)

# Read in combined detections to prepare for modeling
AllDetectionsDF <-
  read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/Sikundur_All_detections_updated_new2.csv')

# Check output
head(AllDetectionsDF)

parsed_time <- format(as.POSIXct(AllDetectionsDF$Datetime, format="%m/%d/%Y %H:%M"), "%H:%M")

print(parsed_time)  # Output: "07:46"

# Convert time column to a proper datetime format (assuming it's already extracted)
AllDetectionsDF$time <- hms::parse_hm(parsed_time) # Parses "HH:MM" format

# Convert to numeric (hours + fraction of minutes)
AllDetectionsDF$time_numeric <- hour(AllDetectionsDF$time) + minute(AllDetectionsDF$time) / 60

# Plot histogram with continuous x-axis
gghistogram(data=AllDetectionsDF, x="time_numeric", facet.by="Common.Name", bins=24)+
  xlim(0,23)

# Need to adjust hour for Unit003
unique(AllDetectionsDF$Unit)

# Standardize Unit names
AllDetectionsDF$Unit_clean <- AllDetectionsDF$Unit %>%
  tolower() %>%                 # Convert to lowercase
  gsub("unit ", "unit", .) %>%   # Remove space after "unit"
  gsub("unit_", "unit", .) %>%   # Remove underscore after "unit"
  gsub("_000$", "", .)           # Remove "_000" if present

# Check unique values after cleaning
unique(AllDetectionsDF$Unit_clean)

#What is the range of confidence scores?
range(AllDetectionsDF$Confidence)

# Plot for all confidence
gghistogram(data=AllDetectionsDF, x='time', facet.by = 'Common.Name')

# What if we focus on high confidence for now?
AllDetectionsDFHighConf <- subset(AllDetectionsDF,Confidence >=0.99)

# Plot for high confidence
gghistogram(data=AllDetectionsDF, x='time_numeric', facet.by = 'Common.Name')
# NOTE still lots of false positives

# Let's focus only on detections during the day
DetectionsDaylightHours <- subset(AllDetectionsDFHighConf, Hour >=5 & Hour <=18)
gghistogram(data=DetectionsDaylightHours, x='time_numeric', facet.by = 'Common.Name')


