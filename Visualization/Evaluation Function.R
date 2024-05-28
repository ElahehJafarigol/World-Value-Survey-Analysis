#HW5
#due nov 12

#Required Packages:
library(caret)
library(ROCR)
library(VIM)
library(car)
library(rpart)
library(rattle)
library(randomForest)
library(adabag)
library(MASS)
library(sandwich)
library(party)  
library(ipred)      
library(glmnet)
library(ggplot2)      
library(rpart.plot)

#problem 1:Create a user-defined R function that produces a series of model evaluations for a binary classifer.
Eval <- function(a, b, c){
  input <- data.frame(True.values=a, Pred.values=b, Pred.probab=c)
  
  # Confusion Matrix
  CM <- confusionMatrix(as.factor(input$Pred.values), as.factor(input$True.values))
  CM
  
  # ROC
  pred <- prediction(input$Pred.probab, input$True.values)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75));
  abline(0, 1, col="red")
  
  #Concordant Pairs and AUC
  Association=function(trueVal,predProb)
  {
    Con_Dis_Data = cbind(trueVal, predProb) 
    
    ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
    zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
    
    conc=matrix(0, dim(zeros)[1], dim(ones)[1])   #build a matrix of 0's 
    disc=matrix(0, dim(zeros)[1], dim(ones)[1])
    ties=matrix(0, dim(zeros)[1], dim(ones)[1])
    
    for (j in 1:dim(zeros)[1])
    {
      for (i in 1:dim(ones)[1])
      {
        if (ones[i,2]>zeros[j,2])
        {conc[j,i]=1}
        
        else if (ones[i,2]<zeros[j,2])
        {disc[j,i]=1}
        
        else if (ones[i,2]==zeros[j,2])
        {ties[j,i]=1}
      }
    }
    
    Pairs=dim(zeros)[1]*dim(ones)[1]              #total number of pairs
    PercentConcordance=(sum(conc)/Pairs)*100
    PercentDiscordance=(sum(disc)/Pairs)*100
    PercentTied=(sum(ties)/Pairs)*100
    AUC=PercentConcordance +(0.5 * PercentTied)
    
    return(list("Percent Concordance"=PercentConcordance,
                "Percent Discordance"=PercentDiscordance,
                "Percent Tied"=PercentTied,
                "Pairs"=Pairs,
                "AUC"=AUC))
  }
  #***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE ENDS***#
  Association(fit$y, fit$fitted.values)
  
  #D statistic (2009)
  #prob.1 <- input[input$True.values==1,]
  #prob.0 <- input[input$True.values==0,]
  #D.statistic <- mean(prob.1$Pred.probab) - mean(prob.0$predProbPred.probab)
  
  #log loss: not sure, just found on google
  rf <- randomForest(Species~., data = iris, importance=TRUE, proximity=TRUE)
  prediction<-predict(rf, iris, type="prob")
  apply(prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15))
  
  logLoss = function(pred, actual){
    -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
  }
  
  logLoss(prediction, iris$Species)
  
  # K-S chart  (Kolmogorov-Smirnov chart) 
  # measures the degree of separation 
  # between the positive (y=1) and negative (y=0) distributions
  input$group <- cut(input$Pred.probab, seq(1,0,-.1), include.lowest=T)
  xtab <- table(input$group, input$True.values)
  xtab
  
  #make empty dataframe
  
  KS<-data.frame(Group=numeric(10),
                 CumPct0=numeric(10),
                 CumPct1=numeric(10),
                 Dif=numeric(10))  
  #fill data frame with information: Group ID, 
  #Cumulative % of 0's, of 1's and Difference
  for (i in 1:10) {
    KS$Group[i] <- i
    KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
    KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
    KS$Dif[i] <- abs(KS$CumPct0[i] - KS$CumPct1[i])
  }
  
  KS
  KS.max <- KS[KS$Dif==max(KS$Dif),]
  maxGroup <- KS[KS$Dif==max(KS$Dif),][1,1]
  
  #and the K-S chart
  ggplot(data=KS)+
    geom_line(aes(Group,CumPct0),color="blue")+
    geom_line(aes(Group,CumPct1),color="red")+
    geom_segment(x=maxGroup,xend=maxGroup, y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
    labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")
  
  #Distribution of predicted probabilities values for the true positives and true negatives
  y1 <- 0
  y2 <- 0
  for (runi in 1:length(pred@predictions)) {
    xx <- density(pred@predictions[[runi]][pred@labels[[runi]]==1])
    t1 <- max(xx$y)
    if(t1 > y1){y1 <- t1}
    yy <- density(pred@predictions[[runi]][pred@labels[[runi]]==0])
    t2 <- max(yy$y)
    if(t2 > y2){y2 <- t2}
  }
  y <- max(y1, y2)
  
  plot(0,0,type="n", xlim=c(0,1), ylim=c(0,y+1), xlab="Prediction", ylab="Density", main="Distribution of predicted probabilities")
  for (runi in 1:length(pred@predictions)) {
    lines(density(pred@predictions[[runi]][pred@labels[[runi]]==1]), col= "red")
    lines(density(pred@predictions[[runi]][pred@labels[[runi]]==0]), col="green")
  }
  
  #Outputs
  return(list("Confusion Matrix"=CM,
              #"Percent Concordance"=PercentConcordance,
              "Percent Discordance"=PercentDiscordance,
              "Percent Tied"=PercentTied,
              "Pairs"=Pairs,
              "AUC"=AUC,
              "D statistic"=D.statistic,
              "KS.max"=KS.max,
              "Log loss"= logloss))
}

# Loading data
honors <- read.csv("C:\\Users\\Mantre\\Desktop\\Intelligent Data Anaytics\\Unit 8\\honors.csv")
#view(honors)

# Model 
fit <- glm(data=honors, hon ~ math + read + female , family="binomial")
summary(fit)

# Function inputs
trueVal = honors$hon
predClass = as.numeric(fit$fitted.values>0.5) 
predProb = fit$fitted.values

predVals <-  data.frame(trueVal= honors$hon, predClass = as.numeric(fit$fitted.values>0.5), predProb = fit$fitted.values) 
head(predVals, 20)


a = trueVal
b = predClass
c = predProb

Eval(a, b, c)


#*******************************************
# Loading data
honors <- read.csv("C:\\Users\\Mantre\\Desktop\\Intelligent Data Anaytics\\Assignments\\Homework 6\\honors.csv")
#view(honors)

# Model 
fit <- glm(data=honors, hon ~ math + read + female , family="binomial")
summary(fit)

# Function inputs
trueVal = honors$hon
head(trueVal)
predClass = as.numeric(fit$fitted.values>0.5) 
head(predClass)
predProb = fit$fitted.values
head(predProb)

predVals <-  data.frame(trueVal= honors$hon, predClass = fit$fitted.values>0.5, predProb = fit$fitted.values) 
head(predVals)

confusionMatrix(as.factor(predClass), as.factor(trueVal))

# Creating a user-defined function for binary probabilistic classifier 
Evaluation_function <- function(trueVal, predClass, predProb){
  input <- data.frame(trueVal=trueVal, predClass = predClass, predProb = predProb)
  
  # 1. Confucion Matrix
  CM <- confusionMatrix(as.factor(trueVal), as.factor(predClass))
  CM
  
  # 2. Logloss
  LL <- logLoss(predClass, trueVal)
  LL
  
  # 3. ROC curve
  library(ROCR) 
  pred <- prediction(predProb, trueVal)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  ROC <- plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
  abline(0, 1, col="red")  
  
  # 5. D statistic
  honors.1 <- predVals[predVals$trueVal==1,]
  honors.0 <- predVals[predVals$trueVal==0,]
  
  D <- mean(honors.1$predProb) - mean(honors.0$predProb)
  D
  
  # 6. Accuracy average vs. cutoff graph 
  perf <- performance(pred, "acc")
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
  
  return(list(
    "Confucion Matrix" =CM,
    "Logloss" = LL,
    "ROC curve",
    "D statistic" = D,
    "Accuracy average vs. cutoff graph",
  ))
}

Evaluation_function(trueVal, predClass, predProb)  

