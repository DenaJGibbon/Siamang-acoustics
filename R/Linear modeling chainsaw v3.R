# LOAD LIBRARIES ----------------------------------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(lme4)
library(bbmle)
library(DHARMa)
library(ggplot2)
library(scales)
library(sjPlot)

# LOAD DATA ---------------------------------------------------------------
SikundurFilesDFAddWeather <- read.csv("data/SikundurFilesDFAddWeather.csv")

# CLEAN TIME AND CREATE DATETIME -----------------------------------------
SikundurFilesDFAddWeather <- SikundurFilesDFAddWeather %>%
  mutate(
    Time_str = str_pad(as.character(Time), width = 6, pad = "0"),
    Time_str = substr(Time_str, 1, 4),
    DateTime = ymd_hms(paste0(
      Date, " ",
      str_sub(Time_str, 1, 2), ":",
      str_sub(Time_str, 3, 4), ":00"
    )),
    Hour = hour(DateTime)
  )

# PLOT DIEL ACTIVITY BY CLASS --------------------------------------------
ForPlot <- SikundurFilesDFAddWeather %>%
  filter(VerifiedClass != "NOISE") %>%
  mutate(
    Time_hms = hm(paste0(str_sub(Time_str, 1, 2), ":", str_sub(Time_str, 3, 4)))
  )

ggplot(ForPlot, aes(x = Time_hms, fill = VerifiedClass)) +
  geom_density() +
  scale_x_time(
    name = "Time of Day",
    labels = function(x) {
      lab <- scales::label_time("%H:%M")(x)
      lab[lab == "00:00"] <- ""
      lab
    }
  ) +
  facet_wrap(~ VerifiedClass, nrow = 3) +
  theme_minimal() +
  labs(x = "Hour of Day", y = "Density") +
  guides(fill = "none") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# BIN DATA BY HOUR AND CREATE CLASS FLAGS --------------------------------
SikundurBinned <- SikundurFilesDFAddWeather %>%
  mutate(
    HourBin = floor_date(DateTime, unit = "hour"),
    isGibbon = VerifiedClass == "LARGIBBON",
    isSiamang = VerifiedClass == "SIAMANG",
    isChainsaw = VerifiedClass == "CHAINSAW"
  )

# SUMMARIZE BY HOUR / UNIT / POINT ---------------------------------------
HourlySummary <- SikundurBinned %>%
  group_by(Unit_clean, point.name, Date, HourBin) %>%
  summarize(
    GibbonDetections = sum(isGibbon, na.rm = TRUE),
    SiamangDetections = sum(isSiamang, na.rm = TRUE),
    ChainsawEvents = sum(isChainsaw, na.rm = TRUE),
    precip_am = max(precip_am, na.rm = TRUE),
    TotalDetections = n(),
    .groups = "drop"
  ) %>%
  arrange(Unit_clean, HourBin) %>%
  group_by(Unit_clean) %>%
  mutate(
    ChainsawEvents_lag1 = lag(ChainsawEvents, n = 1, default = 0),
    Hour = hour(HourBin)
  ) %>%
  ungroup() %>%
  filter(Hour >= 6 & Hour <= 10) %>%
  mutate(
    GibbonDetections = ifelse(GibbonDetections > 0, 1, 0),
    SiamangDetections = ifelse(SiamangDetections > 0, 1, 0),
    ChainsawEvents = ifelse(ChainsawEvents > 0, 1, 0),
    Hour = factor(Hour)
  )

# OPTIONAL CHECKS ---------------------------------------------------------
nrow(HourlySummary)
head(HourlySummary)

# MIXED EFFECTS MODELS FOR GIBBONS ---------------------------------------
mod_mixed_gibbon_null <- glmer(
  GibbonDetections ~ Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_gibbon_siamang <- glmer(
  GibbonDetections ~ SiamangDetections + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_gibbon_chainsawlag <- glmer(
  GibbonDetections ~ ChainsawEvents_lag1 + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_gibbon_rain <- glmer(
  GibbonDetections ~ precip_am + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_gibbon_rain_siamang <- glmer(
  GibbonDetections ~ precip_am + SiamangDetections + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_gibbon_full <- glmer(
  GibbonDetections ~ precip_am + SiamangDetections + ChainsawEvents_lag1 + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

# MODEL COMPARISON --------------------------------------------------------
AICctab(
  mod_mixed_gibbon_null,
  mod_mixed_gibbon_siamang,
  mod_mixed_gibbon_chainsawlag,
  mod_mixed_gibbon_rain,
  mod_mixed_gibbon_rain_siamang,
  mod_mixed_gibbon_full
)

# MODEL SUMMARY AND DIAGNOSTICS ------------------------------------------
summary(mod_mixed_gibbon_rain)

simulationOutput_gibbon <- simulateResiduals(
  fittedModel = mod_mixed_gibbon_rain_siamang,
  plot = FALSE
)

plot(simulationOutput_gibbon)

# EFFECT PLOTS ------------------------------------------------------------
sjPlot::plot_model(
  mod_mixed_gibbon_rain,
  type = "eff",
  terms = "precip_am"
)

GibbonPlots <- sjPlot::plot_model(
  mod_mixed_gibbon_rain,
  type = "pred",
  title = NULL
)

GibbonPlots$precip_am + theme_bw() + xlab("Morning rainfall\n(mm)")
GibbonPlots$Hour + theme_bw() + xlab("Hour\n(local time)")

GibbonPlotsPred <- sjPlot::plot_model(
  mod_mixed_gibbon_rain,
  type = "est"
) + theme_bw()

GibbonPlotsPred

# MIXED EFFECTS MODELS FOR SIAMANGS --------------------------------------

mod_mixed_siamang_null <- glmer(
  SiamangDetections ~ Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_siamang_chainsaw <- glmer(
  SiamangDetections ~ ChainsawEvents + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_siamang_chainsawlag <- glmer(
  SiamangDetections ~ ChainsawEvents_lag1 + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_siamang_rain <- glmer(
  SiamangDetections ~ precip_am + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_siamang_gibbon <- glmer(
  SiamangDetections ~ GibbonDetections + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_siamang_rain_gibbon <- glmer(
  SiamangDetections ~ precip_am + GibbonDetections + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

mod_mixed_siamang_full <- glmer(
  SiamangDetections ~ precip_am + GibbonDetections + ChainsawEvents_lag1 + Hour + (1 | point.name),
  family = binomial(link = "logit"),
  data = HourlySummary
)

# MODEL COMPARISON --------------------------------------------------------
AICctab(
  mod_mixed_siamang_null,
  mod_mixed_siamang_gibbon,
  mod_mixed_siamang_chainsawlag,
  mod_mixed_siamang_rain,
  mod_mixed_siamang_rain_gibbon,
  mod_mixed_siamang_full
)

# MODEL SUMMARY AND DIAGNOSTICS ------------------------------------------
summary(mod_mixed_siamang_null)

simulationOutput_siamang <- simulateResiduals(
  fittedModel = mod_mixed_siamang_null,
  plot = FALSE
)

plot(simulationOutput_siamang)

# EFFECT PLOTS ------------------------------------------------------------
SiamangPlots <- sjPlot::plot_model(
  mod_mixed_siamang_gibbon,
  type = "pred"
)

SiamangPlots$GibbonDetections + theme_bw()
SiamangPlots$Hour + theme_bw()

sjPlot::plot_model(mod_mixed_siamang_gibbon, type = "est") + theme_bw()
sjPlot::plot_model(mod_mixed_siamang_gibbon, type = "re")

# FINAL SUMMARIES ---------------------------------------------------------
summary(mod_mixed_siamang)
summary(mod_mixed_gibbon_rain)

# CO-OCCURRENCE TABLE -----------------------------------------------------
table(HourlySummary$GibbonDetections, HourlySummary$SiamangDetections)
