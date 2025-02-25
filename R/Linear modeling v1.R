library(glmmTMB)
library(bbmle)
library(sjPlot)
library(DHARMa)

# We start by modeling by the day/24-hr period with
# 0 or 1 for the presence of absence of gibbons, siamangs, chainsaws
# Model diagnostics are not great but might change after manual verification

# Read in data ------------------------------------------------------------
# Read in processed data
SikundurFilesDFAddWeather_wide <-
  read.csv('data/SikundurFilesDFAddWeather_wide.csv')

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

# First lets try with logistic regression
SikundurFilesDFAddWeather_wide$Chainsaw <-
  as.factor(ifelse(SikundurFilesDFAddWeather_wide$Chainsaw==0,0,1))

SikundurFilesDFAddWeather_wide$Gibbon <-
  as.factor(ifelse(SikundurFilesDFAddWeather_wide$Gibbon==0,0,1))

SikundurFilesDFAddWeather_wide$Siamang <-
  as.factor(ifelse(SikundurFilesDFAddWeather_wide$Siamang==0,0,1))

# Remove NA vals
SikundurFilesDF_filtered <- na.omit(SikundurFilesDFAddWeather_wide)

table(SikundurFilesDF_filtered$Chainsaw)
table(SikundurFilesDF_filtered$Gibbon)
table(SikundurFilesDF_filtered$Siamang)


# Gibbon modeling ---------------------------------------------------------

# Fit null model
gibbon_model_null <- glmmTMB(
  Gibbon ~ (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

# Fit model with number of days as predictor
gibbon_model_chainsaw <- glmmTMB(
  Gibbon ~ Chainsaw + (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

summary(gibbon_model_chainsaw)

gibbon_model_rain <- glmmTMB(
  Gibbon ~ precip_am + (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

summary(gibbon_model_rain)

# Compare models using AICc,
bbmle::AICctab(gibbon_model_null,
               gibbon_model_chainsaw,
               gibbon_model_rain, weights=T)


# Siamang modeling --------------------------------------------------------

# Fit null model
Siamang_model_null <- glmmTMB(
  Siamang ~ (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

# Fit model with number of days as predictor
Siamang_model_chainsaw <- glmmTMB(
  Siamang ~ Chainsaw + (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

summary(Siamang_model_chainsaw)

Siamang_model_rain <- glmmTMB(
  Siamang ~ precip_am + (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

summary(Siamang_model_rain)

Siamang_model_rainpm <- glmmTMB(
  Siamang ~ precip_pm + (1 | point.name),  # Predictor: Season, random effect: Recorder
  data = SikundurFilesDF_filtered,
  family = binomial()
)

summary(Siamang_model_rainpm)

# Compare models using AICc,
bbmle::AICctab(Siamang_model_null,
               Siamang_model_chainsaw,
               Siamang_model_rain, Siamang_model_rainpm,weights=T)


# Evalute model diagnostics and lot results ------------------------------------------------------------
# Check model fit for the siamang model
res_Siamang_model_rainpm <- simulateResiduals(fittedModel = Siamang_model_rainpm)
plot(res_Siamang_model_rainpm)  # Residual diagnostics
testZeroInflation(res_Siamang_model_rainpm)

# Check model fit for the gibbon model
res_gibbon_model_rain <- simulateResiduals(fittedModel = gibbon_model_rain)
plot(res_gibbon_model_rain)
testZeroInflation(res_gibbon_model_rain)

## Plot results
sjPlot::plot_model(Siamang_model_rainpm,
                   type=c("pred"))

sjPlot::plot_model(gibbon_model_rain,
                   type=c("pred"))
