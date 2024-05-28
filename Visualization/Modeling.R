# Intelligent Data Analytics/DSA 5103
# Fall 2020
# Final project

# Initial EDA of World Values Survey data---------------------------------------
library(ggplot2)
library(tidyverse)
library(mice)
library(VIM)
library(plyr)
library(rockchalk)
library(caret)

# Loading the data--------------------------------------------------------------
raw.data <- read.csv("finalData.csv")
str(raw.data)


raw.data$Country <- as.factor(raw.data$Country)
raw.data$Income <- as.factor(raw.data$Income)
raw.data$Education <- as.factor(raw.data$Education)
raw.data$Gender <- as.factor(raw.data$Gender)
raw.data$Marital.status <- as.factor(raw.data$Marital.status)
raw.data$Employment.status <- as.factor(raw.data$Employment.status)
raw.data$Social.class <- as.factor(raw.data$Social.class)
raw.data$Life.satisfaction <- as.factor(raw.data$Life.satisfaction)
raw.data$Political.view <- as.factor(raw.data$Political.view)
raw.data$AgeGroup <- as.factor(raw.data$AgeGroup)
raw.data$Age <- as.numeric(raw.data$Age)
raw.data$Survey <- as.factor(raw.data$Survey)
raw.data$Year <- as.numeric(raw.data$Year)

raw.data <- raw.data[,-c(12,13) ]
str(raw.data)

Models------------------------------------------------------------------------
#Principal Component Analysis
?prcomp

raw.data$Country <- as.numeric(raw.data$Country)
raw.data$Income <- as.numeric(raw.data$Income)
raw.data$Education <- as.numeric(raw.data$Education)
raw.data$Gender <- as.numeric(raw.data$Gender)
raw.data$Marital.status <- as.numeric(raw.data$Marital.status)
raw.data$Employment.status <- as.numeric(raw.data$Employment.status)
raw.data$Social.class <- as.numeric(raw.data$Social.class)
raw.data$Life.satisfaction <- as.numeric(raw.data$Life.satisfaction)
raw.data$Political.view <- as.numeric(raw.data$Political.view)
raw.data$AgeGroup <- as.numeric(raw.data$AgeGroup)
raw.data$Age <- as.numeric(raw.data$Age)
raw.data$Survey <- as.numeric(raw.data$Survey)
raw.data$Year <- as.numeric(raw.data$Year)

data.pca <- prcomp(raw.data, scale. = TRUE)
data.pca

# summary(data.pca)
# biplot(data.pca)

library("factoextra")
fviz_eig(data.pca)
fviz_pca_biplot(data.pca,
                col.var = "contrib", # Color by contributions to the PC
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE) # Avoid text overlapping

# Trainging data divided by country---------------------------------------------
US <- filter (raw.data, Country == "United States")
Turkey <- filter(raw.data, Country == "Turkey")
Spain <- filter (raw.data, Country == "Spain")
SouthAfrica <- filter(raw.data, Country == "South Africa")
Peru <- filter(raw.data, Country == "Peru")
India <- filter(raw.data, Country == "India")
Chile <- filter(raw.data, Country == "Chile")

# Random Forest Function--------------------------------------------------------
library(randomForest)
myRF <- function(Data) {
  set.seed(2020)
  model.RF <- randomForest(Religiosity ~ ., data = Data, 
                           ntree = 100,
                           keep.forest = FALSE, 
                           importance = TRUE)
  
  print(model.RF$importance)
  b <- barplot(model.RF$importance, horiz = TRUE)
  print(b)
  v <- varImpPlot(model.RF, type=2)
  print(v)
}

#myRF(raw.data)
myRF(US)
myRF(Turkey)
myRF(Spain)
myRF(SouthAfrica)
myRF(Peru)
myRF(India)
myRF(Chile)


# XGBoost Function--------------------------------------------------------------
library(xgboost)  # for fitting GBMs
library("Ckmeans.1d.dp")

myXGB <- function(Data) {
  set.seed(2020)
  model.XGB <- xgboost(
    data = data.matrix(subset(Data, select = -Religiosity)),
    label = Data$Religiosity, 
    objective = "reg:linear",
    nrounds = 100, 
    max_depth = 5, 
    eta = 0.3,
    verbose = 0  # suppress printing
  )
  print("Model Evaluation")
  report <- model.XGB$evaluation_log
  print(report)
  
  print("Model importance")
  print(xgb.importance(model = model.XGB))
  
  p <- xgb.ggplot.importance(xgb.importance(model = model.XGB))
  print(p)
 
}

myXGB(raw.data)
# myXGB(US)
# myXGB(Turkey)
# myXGB(Spain)
# myXGB(SouthAfrica)
# myXGB(Peru)
# myXGB(India)
# myXGB(Chile)

# Decision Tree Function--------------------------------------------------------
library(rpart)          # for decision tree modeling
library(party)          # for visualizing trees
library(partykit)       # for visualizing trees
library(rattle)      		# fancy tree plot
library(rpart.plot)			# enhanced tree plots
library(RColorBrewer)		# color selection for fancy tree plot

myDT <- function(Data){
  model.DT <- rpart(Religiosity~., data=Data, xval=10, control=rpart.control(cp=0.001),
                    parms=list(split="information"), method = "class")  
  #defaults to Gini, 10-fold CV
  
  # Examine the results
  print("The results")
  printcp(model.DT)  # display the results
  p <- plotcp(model.DT)   # visualize cross-validation results
  print(p)
  #summary(model.DT)  # detailed summary of splits
  
  print("Model CP")
  modelCpValue <- model.DT$cptable[which.min(model.DT$cptable[,"xerror"]),"CP"]
  print(modelCpValue)
  
  print("Variable importance")
  report <- model.DT$variable.importance
  print(report)
   
  # Plots
  DTplot <- as.party(model.DT)
  s <-plot(DTplot)
  print(s)
   
  f <- fancyRpartPlot(model.DT, caption = NULL)
  print(f)
}

myDT(raw.data)
myDT(US)
myDT(Turkey)
myDT(Spain)
myDT(SouthAfrica)
myDT(Peru)
myDT(India)
myDT(Chile)


# DT with pruning---------------------------------------------------------------

# myDT <- function(Data, pruningValue){
#   model.DT <- rpart(Religiosity~., data=Data, xval=10, control=rpart.control(cp=0.001),
#                     parms=list(split="information"), method = "class")  
#   #defaults to Gini, 10-fold CV
#   
#   # Examine the results
#   print("The results")
#   printcp(model.DT)  # display the results
#   p <- plotcp(model.DT)   # visualize cross-validation results
#   print(p)
#   #summary(model.DT)  # detailed summary of splits
#   
#   print("Model CP")
#   modelCpValue <- model.DT$cptable[which.min(model.DT$cptable[,"xerror"]),"CP"]
#   print(modelCpValue)
#   
#   print("Variable importance")
#   report <- model.DT$variable.importance
#   print(report)
#   
#   # Plots
#   DTplot <- as.party(model.DT)
#   s <-plot(DTplot)
#   print(s)
#   
#   f <- fancyRpartPlot(model.DT, caption = NULL)
#   print(f)
#   
#   # Prune
#   Prune.model.DT <- prune(model.DT,cp = pruningValue)  #and we can prune to this level
#   d <- fancyRpartPlot(Prune.model.DT, caption = NULL)
#   print(d)
#   
#   print("Pruned model CP")
#   pruningModelCp <- Prune.model.DT$cptable[which.min(Prune.model.DT$cptable[,"xerror"]),"CP"]
#   pruningModelCp
# }
# 
# myDT(US, 0.001)

#-------------------------------------------------------------------------------
library(ggplot2)
data <- read.csv("VI.csv")
data$Method <- as.character(data$ï..Method)
data <- data[ ,-c(1) ]
head(data)

# bar <- function(Data){
#   ggplot(data = Data, aes(x = Value, y = Variable , fill = Method)) + 
#   geom_bar(width = 1, stat = "identity",  position=position_dodge())
# }
# bar(data)

bar <- function(Data){
  ggplot(data = Data, aes(x = Value, y = Method , fill = Variable)) + 
    geom_bar(width = 1, stat = "identity",  position=position_dodge())
}
#bar(data)

PCA <- filter(data, Method == "PCA")
bar(PCA)

RF <- filter(data, Method == "Random Forest")
bar(RF)

BT <- filter(data, Method == "Boosted Tree")
bar(BT)

DT <- filter(data, Method == "Decision Tree")
bar(DT)

bar(data)

bar <- function(Data){
  ggplot(data = Data, aes(y = Value, x = Method , fill = Variable)) + 
    geom_bar(width = 1, stat = "identity",  position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90))
}

bar(data)

countryData <- read.csv("VI2.csv")
countryData$Country <- as.character(countryData$ï..Country)
countryData <- countryData[ ,-c(1) ]
head(countryData)

bar <- function(Data){
  ggplot(data = Data, aes(y = Value, x = Country , fill = Variable)) + 
    geom_bar(width = 1, stat = "identity",  position=position_dodge())+
    theme(axis.text.x = element_text(angle = 45))
    #+facet_wrap(~Country)
}

bar(countryData)
