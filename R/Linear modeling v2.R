# Load required packages
library(dplyr)
library(lubridate)
library(stringr)
library(lme4)     # for GLMMs
library(bbmle)    # for model comparison
library(ggpubr)   # for gghistogram (if not loaded yet)
library(DHARMa)

# Load data
SikundurFilesDFAddWeather <- read.csv('data/SikundurFilesDFAddWeather.csv')

# Inspect unique point names (locations) and class distribution
unique(SikundurFilesDFAddWeather$point.name)
table(SikundurFilesDFAddWeather$VerifiedClass)
head(SikundurFilesDFAddWeather)

SikundurFilesDFAddWeather$Time <-
  str_pad(SikundurFilesDFAddWeather$Time, width = 6, pad = "0")  # "07"

# Add 'Hour' column by extracting hour from 'Time'
SikundurFilesDFAddWeather$Hour <- as.numeric(substr(SikundurFilesDFAddWeather$Time, 1, 2))

# Plot histogram of detections per hour by class (visual inspection)
# Create dodged bar plot
ggplot(SikundurFilesDFAddWeather, aes(x = Hour, fill = VerifiedClass)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ point.name) +
  theme_minimal() +
  labs(title = "Detections per Hour by Species",
       x = "Hour of Day",
       y = "Count of Detections",
       fill = "Class")

# Filter to morning hours where gibbons are most active (e.g., 5 AM to 10 AM)
#SikundurFilesDFAddWeather <- subset(SikundurFilesDFAddWeather, Hour >= 6 & Hour <= 12)

# --- Bin data by hour and flag class types for modeling ---
SikundurBinned <- SikundurFilesDFAddWeather %>%
  mutate(
    # Combine Date and Time into full datetime object
    DateTime = ymd_hms(paste(Date, str_sub(Time, 1, 2), str_sub(Time, 3, 4), str_sub(Time, 5, 6), sep = ":")),
    # Bin to the nearest hour
    HourBin = floor_date(DateTime, unit = "hour"),
    # Create binary flags for each sound class
    isGibbon   = VerifiedClass == "LARGIBBON",
    isSiamang  = VerifiedClass == "SIAMANG",
    isChainsaw = VerifiedClass == "CHAINSAW"
  )

# View sample of binned data
head(SikundurBinned)

SikundurBinned <- SikundurBinned %>%
  mutate(
    isGibbon    = ifelse(is.na(isGibbon), 0, isGibbon),
    isSiamang   = ifelse(is.na(isSiamang), 0, isSiamang),
    isChainsaw  = ifelse(is.na(isChainsaw), 0, isChainsaw)
  )

# --- Summarize counts per hour per point/unit for modeling ---
HourlySummary <- SikundurBinned %>%
  group_by(Unit_clean, HourBin, Date, point.name) %>%
  summarize(
    GibbonCalls    = sum(isGibbon, na.rm = T),
    SiamangCalls   = sum(isSiamang, na.rm = T),
    ChainsawEvents = sum(isChainsaw, na.rm = T),
    precip_am =max(precip_am),
    TotalDetections = n(),  # could be used as an offset or effort proxy
    .groups = "drop"
  )

HourlySummary <- HourlySummary %>%
  arrange(Unit_clean, HourBin) %>%
  group_by(Unit_clean) %>%
  mutate(
    ChainsawEvents_lag1 = lag(ChainsawEvents, n = 1, default = 0)
  ) %>%
  ungroup()

head(HourlySummary)

HourlySummary <- HourlySummary %>%
  mutate(Hour = hour(HourBin))


HourlySummary <- subset(HourlySummary, Hour >=6 & Hour <=10)

# Inspect range of GibbonCalls
HourlySummary$GibbonCalls <- ifelse(HourlySummary$GibbonCalls==0,0,1)

# Inspect range of GibbonCalls
HourlySummary$SiamangCalls <- ifelse(HourlySummary$SiamangCalls==0,0,1)

HourlySummary$ChainsawEvents <- ifelse(HourlySummary$ChainsawEvents==0,0,1)

# Drop rows with any NAs in relevant variables
# HourlySummary <- HourlySummary %>%
#   filter(!is.na(precip_am), !is.na(GibbonCalls),!is.na(SiamangCalls), !is.na(ChainsawEvents))

# --- Mixed effects models for gibbons ---
# Null model: only random effect of point
mod_mixed_gibbon_null <- glmer(GibbonCalls ~ (1 | point.name),
                               family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_gibbon <- glmer(GibbonCalls ~ ChainsawEvents + (1 | point.name),
                          family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_gibbon_chainsawlag<- glmer(GibbonCalls ~   ChainsawEvents_lag1 + (1 | point.name),
                                   family = binomial(link = "logit"), data = HourlySummary)

# Compare models using AICc
AICctab(mod_mixed_gibbon_null, mod_mixed_gibbon,
        mod_mixed_gibbon_chainsawlag)


simulationOutput_gibbon <- simulateResiduals(fittedModel = mod_mixed_gibbon, plot = F)
plot(simulationOutput_gibbon)

sjPlot::plot_model(mod_mixed_gibbon)

# --- Repeat for Siamangs ---
mod_mixed_siamang_null <- glmer(SiamangCalls ~ (1 | point.name),
                                family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_siamang <- glmer(SiamangCalls ~ ChainsawEvents + (1 | point.name/Date),
                           family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_siamang_rain <- glmer(SiamangCalls ~  ChainsawEvents+ precip_am + (1 | point.name),
                               family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_siamang_rainonly <- glmer(SiamangCalls ~   precip_am + (1 | point.name),
                                   family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_siamang_chainsawlag<- glmer(SiamangCalls ~   ChainsawEvents_lag1 + precip_am+ (1 | point.name),
                                     family = binomial(link = "logit"), data = HourlySummary)

# Compare models using AICc
AICctab(mod_mixed_siamang_null, mod_mixed_siamang,mod_mixed_siamang_rain,
        mod_mixed_siamang_rainonly,mod_mixed_siamang_chainsawlag)


simulationOutput_siamang <- simulateResiduals(fittedModel = mod_mixed_siamang, plot = F)
plot(simulationOutput_siamang)

sjPlot::plot_model(mod_mixed_gibbon_rainonly,type = "est")

sjPlot::plot_model(mod_mixed_siamang_rain,type = "est")
