# Load relevant libraries
library(stringr)

# NOTE: Change the directory to the location of your clips
WavFilePaths <- list.files('data/FolderWithClips/',
           full.names = TRUE)

# Look at the structure of the first path
# We use basename to ignore the directory info
WavFilePaths[1]
basename(WavFilePaths[1])
#"0.553_32_Unit001_20230208_095358_1629.0s_1632.0s.wav"

# Now we use str_split_fixed to extract time information
TimeInformation <- str_split_fixed(basename(WavFilePaths), pattern = '_',n=6)[,5]

# Verify that this value matches the time in the full file path
TimeInformation[1]
#"095358"

# If it appears that all is working as expected, you can move forward with identifying recordings at night
# Create a new directory where we will save our clips
NewDirectory <- 'data/FolderWithSortedClips/'

# You can use this command to see where the data folder will be saved
getwd()

# Extract the hour info from the sound file name
Hour <- as.numeric(substr(TimeInformation,1,2))

# Here we identify all times after 6:00 and before 18:00
DayTimeIndices <- which( Hour > 6 & Hour < 18)

# We create a new directory for the daytime clips
DirectoryDaytime <- paste(NewDirectory,'/Daytime/',sep='')

# We then create the directory
dir.create(DirectoryDaytime)

# Then we copy the clips into that folder
# Extract the paths for the clips
DaytimeClipsFullPaths <- WavFilePaths[DayTimeIndices]

# Specify the new location
DaytimeOutPutFullPaths <- paste(DirectoryDaytime, basename(WavFilePaths[DayTimeIndices]),sep='')

# This command copies from the original folder to the new one
file.copy(from=DaytimeClipsFullPaths,
          to=DaytimeOutPutFullPaths )

# We create a new directory for the Nighttime clips
DirectoryNighttime <- paste(NewDirectory,'/Nighttime/',sep='')

# We then create the directory
dir.create(DirectoryNighttime)

# Then we copy the clips into that folder
# Extract the paths for the clips
NighttimeClipsFullPaths <- WavFilePaths[-DayTimeIndices]

# Specify the new location
NighttimeOutPutFullPaths <- paste(DirectoryNighttime, basename(WavFilePaths[-DayTimeIndices]),sep='')

# This command copies from the original folder to the new one
file.copy(from=NighttimeClipsFullPaths,
          to=NighttimeOutPutFullPaths )

