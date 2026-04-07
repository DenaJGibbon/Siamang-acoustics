# Load libraries ----------------------------------------------------------
library(data.table)
library(ggpubr)
# Load data
Chainsaw_detections <- read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/evaluationfiles/CHAINSAW_Positive_detections.csv')
Siamang_detections <- read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/evaluationfiles/SIAMANG_Positive_detections.csv')
Largibbon_detections <- read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/evaluationfiles/LARGIBBON_Positive_detections.csv')

head(Chainsaw_detections)

head(Siamang_detections)


# Calculate siamang overlap ------------------------------------------------------
# Prepare data ------------------------------------------------------------
ChainsawDT <- as.data.table(Chainsaw_detections)
SiamangDT  <- as.data.table(Siamang_detections)

ChainsawDT[, Start.Time := as.POSIXct(Start.Time)]
SiamangDT[,  Start.Time := as.POSIXct(Start.Time)]

ChainsawDT[, Date := as.Date(Start.Time)]
SiamangDT[,  Date := as.Date(Start.Time)]

ChainsawDT <- ChainsawDT[, .(Unit, Date, Start.Time)]
SiamangDT  <- SiamangDT[,  .(Unit, Date, Start.Time)]


# Function to count one-to-one matches within 3 s -----------------------
count_matches <- function(times1, times2, max_diff = 12) {
  times1 <- sort(as.numeric(times1))
  times2 <- sort(as.numeric(times2))

  i <- 1
  j <- 1
  matches <- 0

  while(i <= length(times1) & j <= length(times2)) {
    d <- times1[i] - times2[j]

    if(abs(d) <= max_diff) {
      matches <- matches + 1
      i <- i + 1
      j <- j + 1
    } else if(d < -max_diff) {
      i <- i + 1
    } else {
      j <- j + 1
    }
  }

  return(matches)
}


# Calculate observed overlap ---------------------------------------------
SharedDates <- merge(
  unique(ChainsawDT[, .(Unit, Date)]),
  unique(SiamangDT[, .(Unit, Date)]),
  by = c("Unit", "Date")
)

ObservedOverlap <- 0

for(a in 1:nrow(SharedDates)) {
  TempUnit <- SharedDates$Unit[a]
  TempDate <- SharedDates$Date[a]

  ChainsawTemp <- ChainsawDT[Unit == TempUnit & Date == TempDate]
  SiamangTemp  <- SiamangDT[Unit == TempUnit & Date == TempDate]

  ObservedOverlap <- ObservedOverlap + count_matches(
    ChainsawTemp$Start.Time,
    SiamangTemp$Start.Time,
    max_diff = 12
  )
}

ObservedOverlap


# Prepare randomization frame --------------------------------------------
ChainsawCounts <- ChainsawDT[, .(NChainsaw = .N), by = .(Unit, Date)]

SimulationFrame <- merge(
  ChainsawCounts,
  unique(SiamangDT[, .(Unit, Date)]),
  by = c("Unit", "Date")
)

SimulationFrame[, DayStart := as.POSIXct(paste(Date, "00:00:00"))]
SimulationFrame[, DayEnd   := as.POSIXct(paste(Date, "23:59:59"))]


# Randomization loop ------------------------------------------------------
set.seed(33)

Niterations <- 1000
SimOverlapVals <- numeric(Niterations)

for(z in 1:Niterations) {
  print(z)

  SimOverlap <- 0

  for(a in 1:nrow(SimulationFrame)) {
    TempUnit      <- SimulationFrame$Unit[a]
    TempDate      <- SimulationFrame$Date[a]
    TempNChainsaw <- SimulationFrame$NChainsaw[a]
    TempStart     <- SimulationFrame$DayStart[a]
    TempEnd       <- SimulationFrame$DayEnd[a]

    SiamangFixed <- SiamangDT[Unit == TempUnit & Date == TempDate, Start.Time]
    PossibleTimes <- seq(from = TempStart, to = TempEnd, by = "1 sec")

    SimChainsawTimes <- sample(PossibleTimes,
                               size = TempNChainsaw,
                               replace = TRUE)

    SimOverlap <- SimOverlap + count_matches(
      SimChainsawTimes,
      SiamangFixed,
      max_diff = 12
    )
  }

  SimOverlapVals[z] <- SimOverlap
}

# Plot null distribution --------------------------------------------------
mean_null <- mean(SimOverlapVals)

ggpubr::ggdensity(SimOverlapVals,
                  main = "Null distribution of siamang overlap",
                  xlab = "Overlap count",
                  border = "black") +
  geom_vline(xintercept = ObservedOverlap, col = "red", lwd = 1) +
  geom_vline(xintercept = mean_null, col = "black", linetype = "dashed") +
  annotate("text", x = ObservedOverlap-3, y = 0.05,
           label = "Observed", vjust = -0.5, col = "red") +
  annotate("text", x = mean_null-3, y = 0.05,
           label = "Expected", vjust = -0.5, col = "black")


p_greater <- sum(SimOverlapVals >= ObservedOverlap) / length(SimOverlapVals)
p_less    <- sum(SimOverlapVals <= ObservedOverlap) / length(SimOverlapVals)

p_greater
p_less

# Gibbon overlap ----------------------------------------------------------
ChainsawDT <- as.data.table(Chainsaw_detections)
LargibbonDT  <- as.data.table(Largibbon_detections)

ChainsawDT[, Start.Time := as.POSIXct(Start.Time)]
LargibbonDT[,  Start.Time := as.POSIXct(Start.Time)]

ChainsawDT[, Date := as.Date(Start.Time)]
LargibbonDT[,  Date := as.Date(Start.Time)]

ChainsawDT <- ChainsawDT[, .(Unit, Date, Start.Time)]
LargibbonDT  <- LargibbonDT[,  .(Unit, Date, Start.Time)]


# Function to count one-to-one matches within 3 s -----------------------
count_matches <- function(times1, times2, max_diff = 12) {
  times1 <- sort(as.numeric(times1))
  times2 <- sort(as.numeric(times2))

  i <- 1
  j <- 1
  matches <- 0

  while(i <= length(times1) & j <= length(times2)) {
    d <- times1[i] - times2[j]

    if(abs(d) <= max_diff) {
      matches <- matches + 1
      i <- i + 1
      j <- j + 1
    } else if(d < -max_diff) {
      i <- i + 1
    } else {
      j <- j + 1
    }
  }

  return(matches)
}


# Calculate observed overlap ---------------------------------------------
SharedDates <- merge(
  unique(ChainsawDT[, .(Unit, Date)]),
  unique(LargibbonDT[, .(Unit, Date)]),
  by = c("Unit", "Date")
)

ObservedOverlap <- 0

for(a in 1:nrow(SharedDates)) {
  TempUnit <- SharedDates$Unit[a]
  TempDate <- SharedDates$Date[a]

  ChainsawTemp <- ChainsawDT[Unit == TempUnit & Date == TempDate]
  LargibbonTemp  <- LargibbonDT[Unit == TempUnit & Date == TempDate]

  ObservedOverlap <- ObservedOverlap + count_matches(
    ChainsawTemp$Start.Time,
    LargibbonTemp$Start.Time,
    max_diff = 12
  )
}

ObservedOverlap


# Prepare randomization frame --------------------------------------------
ChainsawCounts <- ChainsawDT[, .(NChainsaw = .N), by = .(Unit, Date)]

SimulationFrame <- merge(
  ChainsawCounts,
  unique(LargibbonDT[, .(Unit, Date)]),
  by = c("Unit", "Date")
)

SimulationFrame[, DayStart := as.POSIXct(paste(Date, "00:00:00"))]
SimulationFrame[, DayEnd   := as.POSIXct(paste(Date, "23:59:59"))]


# Randomization loop ------------------------------------------------------
set.seed(33)

Niterations <- 1000
SimOverlapVals <- numeric(Niterations)

for(z in 1:Niterations) {
  print(z)

  SimOverlap <- 0

  for(a in 1:nrow(SimulationFrame)) {
    TempUnit      <- SimulationFrame$Unit[a]
    TempDate      <- SimulationFrame$Date[a]
    TempNChainsaw <- SimulationFrame$NChainsaw[a]
    TempStart     <- SimulationFrame$DayStart[a]
    TempEnd       <- SimulationFrame$DayEnd[a]

    LargibbonFixed <- LargibbonDT[Unit == TempUnit & Date == TempDate, Start.Time]
    PossibleTimes <- seq(from = TempStart, to = TempEnd, by = "1 sec")

    SimChainsawTimes <- sample(PossibleTimes,
                               size = TempNChainsaw,
                               replace = TRUE)

    SimOverlap <- SimOverlap + count_matches(
      SimChainsawTimes,
      LargibbonFixed,
      max_diff = 12
    )
  }

  SimOverlapVals[z] <- SimOverlap
}




# Plot null distribution --------------------------------------------------
mean_null <- mean(SimOverlapVals)

ggpubr::ggdensity(SimOverlapVals,
                  main = "Null distribution of lar gibbon overlap",
                  xlab = "Overlap count",
                  border = "black") +
  geom_vline(xintercept = ObservedOverlap, col = "red", lwd = 1) +
  geom_vline(xintercept = mean_null, col = "black", linetype = "dashed") +
  annotate("text", x = ObservedOverlap-3, y = 0.05,
           label = "Observed", vjust = -0.5, col = "red") +
  annotate("text", x = mean_null-3, y = 0.05,
           label = "Expected", vjust = -0.5, col = "black")


# Summarize results -------------------------------------------------------
mean(SimOverlapVals)
median(SimOverlapVals)
sd(SimOverlapVals)

p_greater <- sum(SimOverlapVals >= ObservedOverlap) / length(SimOverlapVals)
p_less    <- sum(SimOverlapVals <= ObservedOverlap) / length(SimOverlapVals)

p_greater
p_less
