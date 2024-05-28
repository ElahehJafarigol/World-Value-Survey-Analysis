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
raw.data$Survey <- as.factor(raw.data$Survey)

raw.data$Year <- as.numeric(raw.data$Year)

# Training data divided by country---------------------------------------------
US <- filter (raw.data, Country == "United States")
Turkey <- filter(raw.data, Country == "Turkey")
Spain <- filter (raw.data, Country == "Spain")
SouthAfrica <- filter(raw.data, Country == "South Africa")
Peru <- filter(raw.data, Country == "Peru")
India <- filter(raw.data, Country == "India")
Chile <- filter(raw.data, Country == "Chile")

# Box plots vs. year------------------------------------------------------------
myBoxPlot <- function(Data){
  p <- ggplot(data = Data, aes(x = Year, y = Income)) + 
    geom_boxplot(aes(fill = Income))+
    labs(x = "Year", y = "Income")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Education)) + 
    geom_boxplot(aes(fill = Education))+
    labs(x = "Year", y = "Education")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Gender)) + 
    geom_boxplot(aes(fill = Gender))+
    labs(x = "Year", y = "Gender")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Marital.status)) + 
    geom_boxplot(aes(fill = Marital.status))+
    labs(x = "Year", y = "Marital Status")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Employment.status)) + 
    geom_boxplot(aes(fill = Employment.status))+
    labs(x = "Year", y = "Employment Status")
  print(p)
  
  
  p <- ggplot(data = Data, aes(x = Year, y = Social.class)) + 
    geom_boxplot(aes(fill = Social.class))+
    labs(x = "Year", y = "Social Class")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Life.satisfaction)) + 
    geom_boxplot(aes(fill = Life.satisfaction))+
    labs(x = "Year", y = "Life Satisfaction")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Political.view)) + 
    geom_boxplot(aes(fill = Political.view)) +
    labs(x = "Year", y = "Political View")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = AgeGroup)) + 
    geom_boxplot(aes(fill = AgeGroup))+
    labs(x = "Year", y = "Age Group")
  print(p)
  
  p <- ggplot(data = Data, aes(x = Year, y = Survey)) + 
    geom_boxplot(aes(fill = Survey))+
    labs(x = "Year", y = "Survey")
  print(p)
  
}

myBoxPlot(US)
myBoxPlot(Turkey)
myBoxPlot(Spain)
myBoxPlot(SouthAfrica)
myBoxPlot(Peru)
myBoxPlot(India)
myBoxPlot(Chile)
#-------------------------------------------------------------------------------
myHistrogram <- function(Data){
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Income, fill = Income)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Education, fill = Education)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Gender, fill = Gender)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Marital.status, fill = Marital.status)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Employment.status, fill = Employment.status)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Social.class, fill = Social.class)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Life.satisfaction, fill = Life.satisfaction)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Political.view, fill = Political.view)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Survey, fill = Survey)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = AgeGroup, fill = AgeGroup)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
  h <- ggplot(data = Data, aes(x = Religiosity))+
    geom_histogram(bins = 15, aes(color = Survey, fill = Survey)) +
    scale_color_brewer(palette = "Spectral")+
    scale_fill_brewer(palette = "Spectral")
  print(h)
  
}

myHistrogram(US)
myHistrogram(Turkey)
myHistrogram(Spain)
myHistrogram(SouthAfrica)
myHistrogram(Peru)
myHistrogram(India)
myHistrogram(Chile)

# Patterns plot-----------------------------------------------------------------

myPlot <- function(Data){
  ggplot(data = Data, aes(x = Year, y = Religiosity)) + 
  geom_smooth()+
  labs(x = "Year", y = "Religiosity") 
}

myPlot(Data = raw.data)

?geom_smooth
myplot2 <- function(Data){
  ggplot(data = raw.data, aes(x = as.numeric(Income), y = Religiosity)) +
  geom_smooth()+
  labs(x = "Income", y = "Religiosity")
}

myplot2(raw.data)
# Density plots for different variables-----------------------------------------
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

myDensity <- function(Data){
  d <- ggplot(Data, aes(x = Religiosity, y = Income, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Education, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Gender, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Marital.status, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Employment.status, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Social.class, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Life.satisfaction, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  
  d <- ggplot(Data, aes(x = Religiosity, y = Political.view, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = AgeGroup, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
  
  d <- ggplot(Data, aes(x = Religiosity, y = Survey, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Religious Score", option = "C") +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  print(d)
}

myDensity(raw.data)
myDensity(US)
myDensity(Turkey)
myDensity(Spain)
myDensity(SouthAfrica)
myDensity(Peru)
myDensity(India)
myDensity(Chile)

#Box plots for country v. religious score---------------------------------------
raw.data %>%
  mutate(HouseStyle = fct_reorder(Country, Religiosity, .fun='median')) %>%
  ggplot( aes(x=reorder(Country, Religiosity), y=Religiosity, fill=Country)) + 
  geom_boxplot() +
  xlab("Country") +
  theme(legend.position="none") +
  xlab("")

raw.data %>%
  mutate(HouseStyle = fct_reorder(Country, Religiosity, .fun='median')) %>%
  ggplot( aes(x=reorder(Country, Religiosity), y=Religiosity, fill=Country)) + 
  geom_violin() +
  xlab("Country") +
  theme(legend.position="none") +
  xlab("Country")

# Pie plot of data by Country
library(ggplot2)

myPie <- function(Data){}

Data = raw.data
bar <- ggplot(data = Data, aes(x = "", y = Religiosity , fill = Country)) + 
  geom_bar(width = 1, stat = "identity")

pie <- bar + coord_polar(theta = "y", start = 0)
pie

?coord_polar

#-------------------------------------------------------------------------------