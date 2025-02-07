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
raw.data <- read.csv("Sociology.csv")

# Rename variable
raw.data$Year <- as.character(raw.data$ï..Year)
raw.data <- raw.data[ ,-c(1) ]

# Exploratory Data Analysis: Missing value--------------------------------------

# Let's start looking at missing values
missing <- is.na(raw.data)
dim(missing)

# Visualization of missing variables
na_plot <- aggr(raw.data, col=c('purple','green'),
                numbers=TRUE, sortVars=TRUE,
                labels=names(raw.data), cex.axis= 1,
                gap=3, ylab=c("Missing data","Pattern"))

missing <- is.na(raw.data)
dim(missing)

# This removes all NAs from the entire data set
raw.data <- raw.data[complete.cases(raw.data),]

# Correlation matrix------------------------------------------------------------
raw.data$Country <- as.numeric(raw.data$Country)
raw.data$Education <- as.numeric(raw.data$Education)
raw.data$Gender <- as.numeric(raw.data$Gender)
raw.data$Marital.status <- as.numeric(raw.data$Marital.status)
raw.data$Employment.status <- as.numeric(raw.data$Employment.status)
raw.data$Social.class <- as.numeric(raw.data$Social.class)
raw.data$Political.view <- as.numeric(raw.data$Political.view)
raw.data$Religiosity <- as.numeric(raw.data$Religiosity)
raw.data$Year <- as.numeric(raw.data$Year)

corMar <- cor(raw.data, method = "pearson")
heatmap(corMar)

# Combining Factors and Feature Creation----------------------------------------

# Making a new factor for age called AgeGroup
labs <- c(paste(seq(0, 95, by = 10), seq(0 + 10 - 1, 100 - 1, by = 10),
                sep = "-"))
labs
raw.data$AgeGroup <- cut(raw.data$Age, breaks = c(seq(0, 90, by = 10), Inf),
                         labels = labs, right = FALSE)

#raw.data <- raw.data[ ,-c(3) ]

# Making a new factor for Political view----------------------------------------
raw.data$Political.view <- as.factor(raw.data$Political.view)
levels(raw.data$Political.view)
raw.data$Political.view <- combineLevels(raw.data$Political.view,
                                         levs = c("1", "2"), newLabel = c("Left") )
raw.data$Political.view <- combineLevels(raw.data$Political.view,
                                         levs = c("3", "4"), newLabel = c("Left Leaning") )
raw.data$Political.view <- combineLevels(raw.data$Political.view,
                                         levs = c("5","6"), newLabel = c("Moderate") )
raw.data$Political.view <- combineLevels(raw.data$Political.view,
                                         levs = c("7", "8"), newLabel = c("Right Leaning") )
raw.data$Political.view <- combineLevels(raw.data$Political.view,
                                         levs = c("9", "10"), newLabel = c("Right") )
raw.data$Political.view <- fct_explicit_na(raw.data$Political.view, "Unknown")


# Making a new factor for Life satisfaction-------------------------------------
raw.data$Life.satisfaction <- as.factor(raw.data$Life.satisfaction)

raw.data$Life.satisfaction <- combineLevels(raw.data$Life.satisfaction,levs = c("1", "2"), 
                                            newLabel = c("Very Unsatisfied") )
raw.data$Life.satisfaction <- combineLevels(raw.data$Life.satisfaction,levs = c("3", "4"), 
                                            newLabel = c("Unsatisfied") )
raw.data$Life.satisfaction <- combineLevels(raw.data$Life.satisfaction,levs = c("5", "6"), 
                                            newLabel = c("Neutral") )
raw.data$Life.satisfaction <- combineLevels(raw.data$Life.satisfaction,levs = c("7", "8"), 
                                            newLabel = c("Satisfied") )
raw.data$Life.satisfaction <- combineLevels(raw.data$Life.satisfaction,levs = c("9", "10"), 
                                            newLabel = c("Very Satisfied") )

#-------------------------------------------------------------------------------
raw.data$Survey <- as.factor(raw.data$Year)
unique(raw.data$Survey)

# Years dropped because of missing values: "1989", "1990", "1991",
raw.data$Survey <- combineLevels(raw.data$Survey,
                                 levs = c("1994"),
                                 newLabel = c("Round2") )
raw.data$Survey <- combineLevels(raw.data$Survey,
                                 levs = c("1995", "1996", "1997", "1998"),
                                 newLabel = c("Round3") )
raw.data$Survey <- combineLevels(raw.data$Survey,
                                 levs = c("1999", "2000", "2001", "2002", "2003", "2004"),
                                 newLabel = c("Round4") )
raw.data$Survey <- combineLevels(raw.data$Survey,
                                 levs = c("2005", "2006", "2007", "2008", "2009"),
                                 newLabel = c("Round5") )
raw.data$Survey <- combineLevels(raw.data$Survey,
                                 levs = c("2010","2011", "2012", "2013", "2014"),
                                 newLabel = c("Round6") )
#raw.data <- raw.data[ ,-c(12) ]

#-------------------------------------------------------------------------------
glimpse(raw.data)

raw.data$Marital.status <- as.factor(raw.data$Marital.status)
levels(raw.data$Marital.status)[levels(raw.data$Marital.status) == "1"] <- "Married"
levels(raw.data$Marital.status)[levels(raw.data$Marital.status) == "0"] <- "Single"

raw.data$Education <- as.factor(raw.data$Education)
levels(raw.data$Education)[levels(raw.data$Education) == "0"] <- "Elementary"
levels(raw.data$Education)[levels(raw.data$Education) == "1"] <- "Secondary"
levels(raw.data$Education)[levels(raw.data$Education) == "2"] <- "Tertiary"

raw.data$Social.class <- as.factor(raw.data$Social.class)
levels(raw.data$Social.class)[levels(raw.data$Social.class) == "1"] <- "Lower class"
levels(raw.data$Social.class)[levels(raw.data$Social.class) == "2"] <- "Working class"
levels(raw.data$Social.class)[levels(raw.data$Social.class) == "3"] <- "Middle class"
levels(raw.data$Social.class)[levels(raw.data$Social.class) == "4"] <- "Higher class"

raw.data$Employment.status <- as.factor(raw.data$Employment.status)
levels(raw.data$Employment.status)[levels(raw.data$Employment.status) == "1"] <- "Employed"
levels(raw.data$Employment.status)[levels(raw.data$Employment.status) == "0"] <- "Unemployed"

raw.data$Gender <- as.factor(raw.data$Gender)
levels(raw.data$Gender)[levels(raw.data$Gender) == "1"] <- "Male"
levels(raw.data$Gender)[levels(raw.data$Gender) == "0"] <- "Female"

str(raw.data)

# Separating the Countries------------------------------------------------------
countries <- read.csv("Country_Codes.csv")
str(raw.data)
countries$Country.code <- as.factor(countries$code)
raw.data$Country.code <- as.factor(raw.data$Country)

# Change country numbers to names
raw.data %>% 
  left_join(countries, by=c("Country.code")) %>% 
  select(-c( "Country.code", "code")) -> raw.data

raw.data$Country = raw.data$country
raw.data <- raw.data[ ,-c(15) ]
str(raw.data)

raw.data$Survey <- as.character(raw.data$Survey)

#Dropped rows that were surveyed in round 2
raw.data <- data.frame(raw.data)
raw.data <- raw.data[raw.data$Survey != "Round2",]

# List of countries that participated in all rounds of survey from 3-6
countriesSurvey <- with(raw.data, table(Country, Survey))
countriesSurvey

completeCases <- data.frame(unclass(countriesSurvey))
dim(completeCases)

completeCases <- completeCases[completeCases$Round3 != 0,]
completeCases <- completeCases[completeCases$Round4 != 0,]
completeCases <- completeCases[completeCases$Round5 != 0,]
completeCases <- completeCases[completeCases$Round6 != 0,]
dim(completeCases)
completeCases

# The complete training data consisting of the complete 7 countries
trainingData <- filter (raw.data, Country == "United States"| Country == "Turkey"| 
                          Country == "Spain"| Country == "South Africa"| 
                          Country == "Peru"| Country == "India"| Country == "Chile")

dim(trainingData)
unique(trainingData$Country)



#view(trainingData)
str(trainingData)
trainingData %>% write.csv("finalData.csv", row.names = FALSE)
