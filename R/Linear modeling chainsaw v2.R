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

SikundurFilesDFAddWeather$Time <-substr(SikundurFilesDFAddWeather$Time,1,4)

# Add 'Hour' column by extracting hour from 'Time'
SikundurFilesDFAddWeather$Hour <- as.numeric(substr(SikundurFilesDFAddWeather$Time, 1, 2))

# Plot histogram of detections per hour by class (visual inspection)
ForPlot <- subset(SikundurFilesDFAddWeather,VerifiedClass != 'NOISE')

# Convert numeric time (HHMM) to proper time using lubridate::hm
ForPlot <- ForPlot %>%
  mutate(
    # Ensure Time is character with 4 digits
    Time_str = sprintf("%04d", as.integer(Time)),
    # Parse into time object (hh:mm format)
    Time_hms = hm(paste0(substr(Time_str, 1, 2), ":", substr(Time_str, 3, 4)))
  )



ggplot(ForPlot, aes(x = Time_hms, fill = VerifiedClass)) +
  geom_histogram(binwidth = 300) +  # 5-minute bins
  scale_x_time(name = "Time of Day") +
  facet_wrap(~ VerifiedClass, nrow = 3) +
  theme_minimal() +
  labs(title = "",
       x = "Hour of Day",
       y = "Count of Detections",
       fill = "Class")+ guides(fill='none')


ggplot(ForPlot, aes(x = Time_hms, fill = VerifiedClass)) +
  geom_histogram(binwidth = 300) +  # 5-minute bins
  scale_x_time(name = "Time of Day") +
  facet_wrap(~ point.name, nrow = 3) +
  theme_minimal() +
  labs(title = "",
       x = "Hour of Day",
       y = "Count of Detections",
       fill = "Class")+  # 5-minute bins
  geom_vline(xintercept = as_hms("07:00:00"), linetype = "dashed", color = "black")

# --- Bin data by hour and flag class types for modeling ---
SikundurBinned <- SikundurFilesDFAddWeather %>%
  mutate(
    # Pad Time to 4 digits in case some lost leading zeros (e.g., "300" → "0300")
    Time_str = str_pad(Time, width = 4, side = "left", pad = "0"),

    # Create datetime: assumes seconds = 00
    DateTime = ymd_hms(paste0(
      Date, " ",
      str_sub(Time_str, 1, 2), ":",  # hour
      str_sub(Time_str, 3, 4), ":00" # minute
    )),

    # Bin to hour
    HourBin = floor_date(DateTime, unit = "hour"),

    # Flags
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
    GibbonDetections    = sum(isGibbon, na.rm = T),
    SiamangDetections   = sum(isSiamang, na.rm = T),
    ChainsawEvents = sum(isChainsaw, na.rm = T),
   precip_am = max(precip_am),
    TotalHours = n(),  # effort proxy
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

# Inspect range of GibbonDetections
HourlySummary$GibbonDetections <- ifelse(HourlySummary$GibbonDetections==0,0,1)

# Inspect range of GibbonDetections
HourlySummary$SiamangDetections <- ifelse(HourlySummary$SiamangDetections==0,0,1)

HourlySummary$ChainsawEvents <- ifelse(HourlySummary$ChainsawEvents==0,0,1)

HourlySummary$Hour <- factor(hour(HourlySummary$HourBin))

nrow(HourlySummary)

# # Drop rows with any NAs in relevant variables
# HourlySummary <- HourlySummary %>%
#   filter(!is.na(precip_am), !is.na(GibbonDetections),!is.na(SiamangDetections), !is.na(ChainsawEvents))

# --- Mixed effects models for gibbons ---
# Null model: only random effect of point
mod_mixed_gibbon_null <- glmer(GibbonDetections ~  Hour + (1 | point.name),
                               family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_gibbon <- glmer(GibbonDetections ~ ChainsawEvents + Hour+ (1 | point.name),
                          family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_gibbon_siamang <- glmer(GibbonDetections ~ SiamangDetections + Hour + (1 | point.name),
                                  family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_gibbon_chainsawlag<- glmer(GibbonDetections ~   ChainsawEvents_lag1 + Hour + (1 | point.name),
                                   family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_gibbon_rain<- glmer(GibbonDetections ~   precip_am + Hour +   (1 | point.name),
                                     family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_gibbon_rain_siamang<- glmer(GibbonDetections ~   precip_am + Hour + SiamangDetections  +  (1 | point.name),
                              family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_gibbon_full <- glmer(GibbonDetections ~   precip_am + Hour + SiamangDetections +  ChainsawEvents + (1 | point.name),
                                      family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_gibbon_updated <- glmer(GibbonDetections ~   precip_am + Hour + SiamangDetections +  ChainsawEvents_lag1 + (1 | point.name),
                               family = binomial(link = "logit"), data = HourlySummary)

# Compare models using AICc
AICctab(mod_mixed_gibbon_null, mod_mixed_gibbon,mod_mixed_gibbon_rain_siamang,
        mod_mixed_gibbon_siamang,
        mod_mixed_gibbon_chainsawlag,mod_mixed_gibbon_rain,
        mod_mixed_gibbon_full,mod_mixed_gibbon_updated)

summary(mod_mixed_gibbon_rain)
simulationOutput_gibbon <- simulateResiduals(fittedModel = mod_mixed_gibbon_rain_siamang, plot = F)
plot(simulationOutput_gibbon)

sjPlot::plot_model(mod_mixed_gibbon_rain,type='eff',
                  terms='precip_am')
GibbonPlots <-
  sjPlot::plot_model(mod_mixed_gibbon_rain,type='pred',
                   title = NULL)

GibbonPlots$precip_am +theme_bw()+xlab('Morning rainfall \n (mm)')
GibbonPlots$Hour +theme_bw()+xlab('Hour \n (local time)')

GibbonPlotsPred <-
  sjPlot::plot_model(mod_mixed_gibbon_rain,type='est')+theme_bw()

GibbonPlotsPred

# --- Repeat for Siamangs ---
mod_mixed_siamang_null <- glmer(SiamangDetections ~  Hour + (1 | point.name),
                                family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_siamang <- glmer(SiamangDetections ~ ChainsawEvents + Hour + (1 | point.name),
                           family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_siamang_gibbon <- glmer(SiamangDetections ~ GibbonDetections + Hour + (1 | point.name),
                           family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_siamang_chainsawlag<- glmer(SiamangDetections ~   ChainsawEvents_lag1 +  Hour + (1 | point.name),
                                     family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_siamang_rain<- glmer(SiamangDetections ~   precip_am + Hour +   (1 | point.name),
                              family = binomial(link = "logit"), data = HourlySummary)

# Main model: chainsaw as fixed effect, point as random effect
mod_mixed_siamang_rain_gibbon<- glmer( SiamangDetections  ~   precip_am + Hour + GibbonDetections + (1 | point.name),
                                      family = binomial(link = "logit"), data = HourlySummary)

mod_mixed_siamangfull <- glmer(SiamangDetections ~   precip_am + Hour + GibbonDetections +  ChainsawEvents + (1 | point.name),
                               family = binomial(link = "logit"), data = HourlySummary)

# Compare models using AICc
AICctab(mod_mixed_siamang_null,
        mod_mixed_siamang,mod_mixed_siamang_chainsawlag,
        mod_mixed_siamang_gibbon,mod_mixed_siamang_rain,
        mod_mixed_siamang_rain_gibbon,mod_mixed_siamangfull)

summary(mod_mixed_siamang)
simulationOutput_siamang <- simulateResiduals(fittedModel = mod_mixed_siamang_gibbon, plot = F)
plot(simulationOutput_siamang)

sjPlot::plot_model(mod_mixed_gibbon_rain,type = "pred")
sjPlot::plot_model(mod_mixed_gibbon_rain,type = "re")

Siamangplots <- sjPlot::plot_model(mod_mixed_siamang,type = "pred")
Siamangplots$ChainsawEvents+theme_bw()
Siamangplots$Hour+theme_bw()

sjPlot::plot_model(mod_mixed_siamang,type = "pred")
sjPlot::plot_model(mod_mixed_siamang,type = "est")+theme_bw()

sjPlot::plot_model(mod_mixed_siamang,type = "re")

summary(mod_mixed_gibbon_rain)
summary(mod_mixed_siamang)

table(HourlySummary$GibbonDetections,HourlySummary$SiamangDetections)




