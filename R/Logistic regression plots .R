library(stringr)
library(ggpubr)
library(bbmle)
library(ggplot2)

# Chainsaws ----------------------------------------------------
# Read in the true and false positives for chainsaws
ChainsawPositive <- read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/evaluationfiles/CHAINSAW_Positive_detections.csv')
ChainsawNegative <- read.csv('/Users/denaclink/Desktop/RStudioProjects/Siamang-acoustics/data/evaluationfiles/CHAINSAW_Negative_detections.csv')

# Combine into a single dataframe
CombinedDFChainsaw <- rbind.data.frame(ChainsawPositive,ChainsawNegative)

# Convert to a binary outcome
CombinedDFChainsaw$Eval <-  ifelse(CombinedDFChainsaw$Positive.or.Negative=='Positive',1,0)

CombinedDFChainsaw$Confidence <- CombinedDFChainsaw$Confidence -0.0001

CombinedDFChainsaw$logit=log(CombinedDFChainsaw$Confidence /(1-CombinedDFChainsaw$Confidence ))

# Fit three models
null.model=glm(Eval~1, CombinedDFChainsaw, family = 'binomial')
conf.model=glm(Eval~Confidence, CombinedDFChainsaw, family = 'binomial')
logit.model=glm(Eval~logit, CombinedDFChainsaw, family = 'binomial')
AICctab(null.model, conf.model,weights=T)
summary(conf.model)

prediction.range.conf=seq(0,1,.001)
prediction.range.logit=seq(-3,7,.1) # this is the approximate range of the logit scores

predictions.conf=predict(conf.model, list(Confidence=prediction.range.conf), type='r')
predictions.logit=predict(logit.model, list(logit=prediction.range.logit), type='r')

prediction_df <- cbind.data.frame(prediction.range.conf,predictions.conf)

# thresholds for pr(Positive)= .85, 0.95, and 0.99
cutoff85.c=(log(.85/(1-.85))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff95.c=(log(.95/(1-.95))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff99.c=(log(.99/(1-.99))-conf.model$coefficients[1])/conf.model$coefficients[2]

cutoff85.l=(log(.85/(1-.85))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff95.l=(log(.95/(1-.95))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff99.l=(log(.99/(1-.99))-logit.model$coefficients[1])/logit.model$coefficients[2]

plot(Eval~Confidence, CombinedDFChainsaw, main = 'Confidence scores',
     ylab = 'pr(BirdNET prediction is correct)', xlab = 'confidence score',
     xlim=range(prediction.range.conf), pch=16, cex=1.5, col=rgb(0,0,0,.2))
lines(predictions.conf~prediction.range.conf, lwd=4, col=rgb(0,.75,1,.5))
abline(v=cutoff85.c, col='orange', lwd=4)
abline(v=cutoff95.c, col='red', lwd=4)
abline(v=cutoff99.c, col='magenta', lwd=4)

# Convert your data into a ggplot object
Chainsawregression <- ggplot(CombinedDFChainsaw, aes(x = Confidence, y = Eval)) +
  geom_point(color = rgb(0,0,0,0.2), size = 3) +
  geom_line(data = prediction_df,aes(x = prediction.range.conf, y = predictions.conf), color = 'grey', size = 1.5) +
  geom_vline(xintercept = cutoff85.c, color = "orange", size = 1.5) +
  annotate("text", x = cutoff85.c - 0.02, y = 0.02, label = "85", color = "orange") +
  # geom_vline(xintercept = cutoff95.c, color = "red", size = 1.5) +
  # annotate("text", x = cutoff95.c - 0.02, y = 0.02, label = "95", color = "red") +
  # geom_vline(xintercept = cutoff99.c, color = "magenta", size = 1.5) +
 # annotate("text", x = cutoff99.c - 0.02, y = 0.02, label = "99", color = "magenta") +
  labs(title = "Chainsaw",
       x = "Confidence score",
       y = "pr(BirdNET prediction \n is correct)") +
  theme_minimal()

Chainsawregression

# Lar gibbon --------------------------------------------------------------
FullFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/segments_center/segments_binary_Kutapanjang/lar gibbon/',full.names = TRUE)
TruePositives <- list.files(FullFolders[str_detect(FullFolders,'Positive')])
FalsePositives <- list.files(FullFolders[str_detect(FullFolders,'Negative')])

PositiveDF <- cbind.data.frame(rep('Positive', length(TruePositives)),TruePositives)
colnames(PositiveDF) <- c('Category','File')
NegativeDF <- cbind.data.frame(rep('Negative', length(FalsePositives)),FalsePositives)
colnames(NegativeDF) <- c('Category','File')


CombinedDF <- rbind.data.frame(PositiveDF,NegativeDF)
CombinedDF$Confidence <- as.numeric(str_split_fixed(CombinedDF$File, pattern = '_', n=2)[,1])
CombinedDF$Class <- (str_split_fixed(CombinedDF$File, pattern = '_', n=3)[,2])

CombinedDF$Eval <-  ifelse(CombinedDF$Category=='Positive',1,0)
CombinedDF$Confidence <- CombinedDF$Confidence -0.0001

CombinedDF$logit=log(CombinedDF$Confidence /(1-CombinedDF$Confidence ))

# Fit three models
null.model=glm(Eval~1, CombinedDF, family = 'binomial')
conf.model=glm(Eval~Confidence, CombinedDF, family = 'binomial')
logit.model=glm(Eval~logit, CombinedDF, family = 'binomial')
AICctab(null.model, conf.model,weights=T)
summary(conf.model)

prediction.range.conf=seq(0,1,.001)
prediction.range.logit=seq(-3,7,.1) # this is the approximate range of the logit scores

predictions.conf=predict(conf.model, list(Confidence=prediction.range.conf), type='r')
predictions.logit=predict(logit.model, list(logit=prediction.range.logit), type='r')

prediction_df <- cbind.data.frame(prediction.range.conf,predictions.conf)

# thresholds for pr(Positive)= .85, 0.95, and 0.99
cutoff85.c=(log(.85/(1-.85))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff95.c=(log(.95/(1-.95))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff99.c=(log(.99/(1-.99))-conf.model$coefficients[1])/conf.model$coefficients[2]

cutoff85.l=(log(.85/(1-.85))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff95.l=(log(.95/(1-.95))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff99.l=(log(.99/(1-.99))-logit.model$coefficients[1])/logit.model$coefficients[2]

plot(Eval~Confidence, CombinedDF, main = 'Confidence scores',
     ylab = 'pr(BirdNET prediction is correct)', xlab = 'confidence score',
     xlim=range(prediction.range.conf), pch=16, cex=1.5, col=rgb(0,0,0,.2))
lines(predictions.conf~prediction.range.conf, lwd=4, col=rgb(0,.75,1,.5))
abline(v=cutoff85.c, col='orange', lwd=4)
abline(v=cutoff95.c, col='red', lwd=4)
abline(v=cutoff99.c, col='magenta', lwd=4)

library(ggplot2)

# Convert your data into a ggplot object
LarGibbonRegression <- ggplot(CombinedDF, aes(x = Confidence, y = Eval)) +
  geom_point(color = rgb(0,0,0,0.2), size = 3) +
  geom_line(data = prediction_df,aes(x = prediction.range.conf, y = predictions.conf), color = 'grey', size = 1.5) +
  geom_vline(xintercept = cutoff85.c, color = "orange", size = 1.5) +
  annotate("text", x = cutoff85.c - 0.02, y = 0.02, label = "85", color = "orange") +
  geom_vline(xintercept = cutoff95.c, color = "red", size = 1.5) +
  annotate("text", x = cutoff95.c - 0.02, y = 0.02, label = "95", color = "red") +
  # geom_vline(xintercept = cutoff99.c, color = "magenta", size = 1.5) +
  # annotate("text", x = cutoff99.c - 0.02, y = 0.02, label = "99", color = "magenta") +
   labs(title = "Lar gibbon (binary)",
       x = "Confidence score",
       y = "pr(BirdNET prediction \n is correct)") +
  theme_minimal()


# White bearded -----------------------------------------------------------
FullFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/segments_center/segments_binaryclean_tahawa/white bearded gibbon/',full.names = TRUE)
TruePositives <- list.files(FullFolders[str_detect(FullFolders,'Positive')])
FalsePositives <- list.files(FullFolders[str_detect(FullFolders,'Negative')])

PositiveDF <- cbind.data.frame(rep('Positive', length(TruePositives)),TruePositives)
colnames(PositiveDF) <- c('Category','File')
NegativeDF <- cbind.data.frame(rep('Negative', length(FalsePositives)),FalsePositives)
colnames(NegativeDF) <- c('Category','File')

CombinedDF <- rbind.data.frame(PositiveDF,NegativeDF)
CombinedDF$Confidence <- as.numeric(str_split_fixed(CombinedDF$File, pattern = '_', n=2)[,1])
CombinedDF$Class <- (str_split_fixed(CombinedDF$File, pattern = '_', n=3)[,2])

CombinedDF$Eval <-  ifelse(CombinedDF$Category=='Positive',1,0)
CombinedDF$Confidence <- CombinedDF$Confidence -0.0001

CombinedDF$logit=log(CombinedDF$Confidence /(1-CombinedDF$Confidence ))

# Fit three models
null.model=glm(Eval~1, CombinedDF, family = 'binomial')
conf.model=glm(Eval~Confidence, CombinedDF, family = 'binomial')
logit.model=glm(Eval~logit, CombinedDF, family = 'binomial')
AICctab(null.model, conf.model,weights=T)
summary(conf.model)

prediction.range.conf=seq(0,1,.001)
prediction.range.logit=seq(-3,7,.1) # this is the approximate range of the logit scores

predictions.conf=predict(conf.model, list(Confidence=prediction.range.conf), type='r')
predictions.logit=predict(logit.model, list(logit=prediction.range.logit), type='r')

prediction_df <- cbind.data.frame(prediction.range.conf,predictions.conf)

# thresholds for pr(Positive)= .85, 0.95, and 0.99
cutoff65.c=(log(.65/(1-.65))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff95.c=(log(.95/(1-.95))-conf.model$coefficients[1])/conf.model$coefficients[2]
cutoff99.c=(log(.99/(1-.99))-conf.model$coefficients[1])/conf.model$coefficients[2]

cutoff65.l=(log(.65/(1-.65))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff95.l=(log(.95/(1-.95))-logit.model$coefficients[1])/logit.model$coefficients[2]
cutoff99.l=(log(.99/(1-.99))-logit.model$coefficients[1])/logit.model$coefficients[2]

library(ggplot2)

# Convert your data into a ggplot object
WhiteBeardedGibbonRegression <- ggplot(CombinedDF, aes(x = Confidence, y = Eval)) +
  geom_point(color = rgb(0,0,0,0.2), size = 3) +
  geom_line(data = prediction_df,aes(x = prediction.range.conf, y = predictions.conf), color = 'grey', size = 1.5) +
  #geom_vline(xintercept = cutoff65.c, color = "orange", size = 1.5) +
  #annotate("text", x = cutoff65.c - 0.02, y = 0.02, label = "65", color = "orange") +
  # geom_vline(xintercept = cutoff95.c, color = "red", size = 1.5) +
  # annotate("text", x = cutoff95.c - 0.02, y = 0.02, label = "95", color = "red") +
  #geom_vline(xintercept = cutoff99.c, color = "magenta", size = 1.5) +
  #annotate("text", x = cutoff99.c - 0.02, y = 0.02, label = "99", color = "magenta") +
  labs(title = "White-bearded gibbon (binary)",
       x = "Confidence score",
       y = "pr(BirdNET prediction \n is correct)") +
  theme_minimal()

cowplot::plot_grid(WhiteBeardedGibbonRegression,
                   Northerngreyregression,
                   LarGibbonRegression,
                   labels = c('A)','B)','C)'),
                   label_x = .95, nrow = 3)
