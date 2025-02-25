# Tutorial: https://ourcodingclub.github.io/tutorials/mixed-models/#what
library(tidyr)
library(ggpubr)
library(stringr)

# Read in combined detections to prepare for modeling
AllDetectionsDF <-
  read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/Sikundur_All_detections_updated_new2.csv')

# Check output
head(AllDetectionsDF)

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

# Now subset unit003
AllDetectionsDFUnit003 <- subset(AllDetectionsDF, Unit_clean=="unit003")

TempName <-str_split_fixed(AllDetectionsDFUnit003$Begin.Path,pattern = 'All\\\\Unit',n=2)[,2]
HourRecStart <- as.numeric(substr(str_split_fixed(TempName,pattern = '_',n=3)[,3],1,2))

HoursSinceStart <- round(AllDetectionsDFUnit003$Begin.Time..s./(60*60),0)
AllDetectionsDFUnit003$Hour <- HourRecStart+HoursSinceStart

AllDetectionsDFNoUnit003 <- subset(AllDetectionsDF, Unit_clean!="unit003")

AllDetectionsDFStdTime <- rbind.data.frame(AllDetectionsDFNoUnit003,AllDetectionsDFUnit003)

# What is the range of confidence scores?
range(AllDetectionsDFStdTime$Confidence)

# Plot for all confidence
gghistogram(data=AllDetectionsDFStdTime, x='Hour', facet.by = 'Common.Name')

# What if we focus on high confidence for now?
AllDetectionsDFStdTimeHighConf <- subset(AllDetectionsDFStdTime,Confidence >=0.99)

# Plot for high confidence
gghistogram(data=AllDetectionsDFStdTime, x='Hour', facet.by = 'Common.Name')
# NOTE still lots of false positives

# Let's focus only on detections during the day
DetectionsDaylightHours <- subset(AllDetectionsDFStdTimeHighConf, Hour >=5 & Hour <=18)
gghistogram(data=DetectionsDaylightHours, x='Hour', facet.by = 'Common.Name')
