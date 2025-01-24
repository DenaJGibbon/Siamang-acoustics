library(stringr)
library(ggpubr)
library(dplyr)

# Read in the BirdNET detection files
SikundurFiles <- list.files("/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/Output", pattern = '.txt', recursive=T, full.names=T)
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
table(SikundurFilesCombined$Common.Name)

# Save the output to a CSV
write.csv(SikundurFilesCombined,'data/SikundurFilesCombined.csv',row.names = F)

# Can read in the data
SikundurFilesCombined <- read.csv('data/SikundurFilesCombined.csv')

# Here we remove all non-detections and subset above confidence 0.92
SikundurFilesCombined <- subset(SikundurFilesCombined,Common.Name !='nocall' & Confidence >= 0.99 )

# Check structure of data
nrow(SikundurFilesCombined)
head(SikundurFilesCombined)


TempVals <-str_split_fixed(SikundurFilesCombined$TempName,pattern='_',n=3)

SikundurFilesCombined$Recorder <- TempVals[,1]
SikundurFilesCombined$Date <- TempVals[,2]
SikundurFilesCombined$Month <- as.numeric(substr(SikundurFilesCombined$Date,5,6))
SikundurFilesCombined$Date <- as.Date(SikundurFilesCombined$Date, format = "%Y%m%d")
SikundurFilesCombined$Hour <- substr(TempVals[,3],1,2)

SikundurFilesCombined$Hour <- as.numeric((SikundurFilesCombined$Hour))

ggpubr::gghistogram(data=SikundurFilesCombined,x='Hour', facet.by="Common.Name",stat="count")

ggpubr::gghistogram(data=SikundurFilesCombined,x='Hour', facet.by="Recorder",stat="count")

ggpubr::gghistogram(data=SikundurFilesCombined,x='Date', stat="count")

