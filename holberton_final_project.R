## Project:  STA 215, Fall 2024, Final Project
# Located:   sta 215
# File Name: holberton-final-project.R
# Date:      2024_11_24
# Who:       Liza Holberton

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

#load data
setwd("/Users/lizaholberton/Desktop/sta215")
raw_data <- read_csv("raw_data.csv")

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(raw_data$race)

table(raw_data$gender)

table(raw_data$party)

table(raw_data$age)
mean(raw_data$age)
sd(raw_data$age)

table(raw_data$education)

table(raw_data$term)
mean(raw_data$term)
sd(raw_data$term)

table(raw_data$prev_job)

table(raw_data$wealth)
mean(raw_data$wealth)
sd(raw_data$wealth)

table(raw_data$christian)

table(raw_data$abortion)

table(raw_data$gun_policy)

table(raw_data$voting_record)

table(raw_data$bills)
mean(raw_data$bills)
sd(raw_data$bills)

table(raw_data$region)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
table(raw_data$race)
raw_data$gender <- as.numeric(raw_data$gender)

raw_data$lngender <- log(raw_data$gender)
boxplot(lngender ~ race, data = raw_data)

anova <- aov(race ~ gender, data = raw_data)
summary(anova)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
raw_data$race <- as.numeric(raw_data$race)
linear_plot <- plot(raw_data$gender, raw_data$lnrace)
print(linear_plot)
meany <- mean(raw_data$lnrace, na.rm = TRUE)
meanx <- mean(raw_data$gender, na.rm = TRUE)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")
raw_data$lnwealth <- log(raw_data$race)
linear_relationship <- lm(race ~ gender, data = raw_data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals

plot(raw_data$gender, residuals(linear_relationship))
plot(raw_data$race, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(raw_data$race, raw_data$gender)

chisq.test(table(raw_data$race, raw_data$gender))