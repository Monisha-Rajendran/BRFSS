# ALY6119 Big Data EDA
# Group Members:
# Catherine Richard
# Monisha Rajendran
# Akshit Bajaj
# 
# 
#
# Data source: 
# Purpose: Exploratory Analysis of data
# April 20, 2020

# ---------------------------------------------------------------------------------------------------------------

library(skimr)
library(readr)
library(dataPreparation)
library(tidyverse)
library(dplyr)
library(corrplot)
library(caret)
library(ggplot2)
library(leaps)
library(rpart)
library(tree)
library(glmnet) 
library(car)
library(MASS)

# Import Data
library(readr)
brfss1 <- read_csv("data/analytic.csv", col_types = cols(SEX = col_integer(), 
                                                         X_AGE_G = col_integer(), X_BMI5CAT = col_integer()))
###############################################
#          Exploratory Data Analysis          #
###############################################

# Check dimension of data 
dim(brfss1)

# Summary of data
skim(brfss1)

# ---------------------------------------------------------------------------------------------------------------
# Replace/Remove NAs & missing values


#Replace NA with zero
# SMOKEDAY2 - field blank if respondent does not smoke?  Replace with zero
brfss1 <- brfss1 %>% 
  mutate(SMOKDAY2=ifelse(is.na(SMOKDAY2), 0, SMOKDAY2))

#Replace NA with mean value

brfss1 <- brfss1 %>% 
  mutate(X_BMI5CAT=ifelse(is.na(X_BMI5CAT), 3, X_BMI5CAT)) 

# Remove NAs from : EXERANY2v & X_MRACE1
brfss <-  na.omit(brfss1)

#check
dim(brfss)
any(is.na.data.frame(brfss))

hist(brfss$VETERAN3) 
#all respondants are or have served in military per study inclusion criteria.

#Remove VETERAN3 & REDUDANT SLEPTIM1
brfss <- subset(brfss, select = -c(VETERAN3, SLEPTIM1))

table(brfss$EDGROUP)
# Removed Non-Responders 
brfss <- brfss[brfss$EDGROUP != 9,]

table(brfss$SMOKGRP)

#only 290 Non-responders. Removed from dataset
brfss <- brfss[brfss$SMOKGRP != 9,]

#convert categorical data to factors to properly graph 
brfss3 <- brfss %>% 
  mutate(ALCGRP = factor(ALCGRP, levels = c(3, 2, 9, 1),labels = c('Weekly', 'Monthly', "Unknown", "None")),
         X_AGE_G = factor(X_AGE_G, levels = c(1, 2, 3, 4, 5, 6), labels = c('18-24', '25-34', '35-44', '45-54', '55-64', '65+')),
         ASTHMA4 = factor(ASTHMA4, levels = c(1,0), labels = c("Yes", "No")),
         RACEGRP = factor(RACEGRP, levels = c(1, 2, 3, 4, 5, 6, 9), labels = c('White', 'AA', 'Native Am', 'Asian', 'Pacific Islander', 'Other/Multiracial', 'Unknown')),
         MARITAL = factor(MARITAL, levels = c(1, 2, 3, 4, 5, 9), labels = c('Married', 'Divorced', 'Widowed', 'Never Married', 'Partner', 'Unknown')),
         GENHLTH2 = factor(GENHLTH2, levels = c(1, 2, 3, 4, 5, 9), labels = c('Excellent', 'Very Good', 'Good', 'Fair', 'Poor', 'Unknown')),
         HLTHPLN2 = factor(HLTHPLN2, levels = c(1, 2, 9), labels = c('Yes', 'No', 'Unknown')),
         EDGROUP = factor(EDGROUP, levels = c(1, 2, 3, 4, 9), labels = c('Some High School', 'High School', 'Some college', 'College Graduate', 'Unknown')),
         INCOME3 = factor(INCOME3, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c('< $10K', '10-15', '15-20', '20-25', '25-35', '35-50', '50-75', '$75K +', 'Unknown')),
         BMICAT = factor(BMICAT, levels = c(1, 2, 3, 4, 9), labels = c('Underweight', 'Normal', 'Overweight', 'Obese', 'Unknown')),
         SMOKEDAY2 = factor(SMOKDAY2, levels = c(1,2, 3, 9), labels = c('Every Day', 'Some Days', 'Not at all', 'Unknown')),
         EXERANY3 = factor(EXERANY3, levels = c(1, 2, 9), labels = c('Yes', 'No', 'Unknown')))

skim(brfss3)
# ---------------------------------------------------------------------------------------------------------------

#Plots
#Sleep Distribution
# Barplot of Sleep Distribution        
bar_sleep <- ggplot(data=brfss) +
  geom_bar(mapping=aes(x=SLEPTIM2, fill=ALCDAY5),
           show.legend = TRUE,
           width=0.6) +
  xlim(-1, 20) +
  theme_minimal() +
  labs(x = "Hours of sleep", y = "Count", title="Hours Slept Distribution") 
bar_sleep

boxplot(brfss$SLEPTIM2, main="Box Plot of SLEPTIM2", 
        xlab="All Respondants", ylab="Hours Slept")
# General Sleep distribution of all veteran respondants

boxplot(SLEPTIM2~ALCGRP, data=brfss3, main="Box Plot of SLEPTIM2 by ALCGRP", 
        xlab="Alcohol consumption in last 30 days", ylab="Hours Slept")
#SleepHrs distribution - Alcohol Group Comparison

boxplot(SLEPTIM2~BMICAT, data=brfss3, main="Box Plot of Sleep by BMI Group", 
        xlab="BMI Category", ylab="Hours Slept")
#SleepHrs distribution - BMI Group Comparison

boxplot(SLEPTIM2~EDGROUP, data=brfss3, main="Box Plot of Sleep by EDU Group", 
        xlab="Highest Education Completed", ylab="Hours Slept")
#SleepHrs distribution - Highest Education Group Comparison


boxplot(SLEPTIM2~SMOKGRP, data=brfss, main="Box Plot of Sleep by Smoker Freq Group", 
        xlab="Current Smoking Freq", ylab="Hours Slept")
#SleepHrs distribution - Smoker Group Comparison


boxplot(SLEPTIM2~SMOKDAY2, data=brfss, main="Box Plot of SLEPTIM2 by Smoker Freq Group", 
        xlab="Current Smoking Freq", ylab="Hours Slept")
#SleepHrs distribution - Smoker Group Comparison


#BMI Distribution
bar_bmi2 <- ggplot(data=brfss3) +
  
  geom_bar(mapping=aes(x=forcats::fct_rev(fct_infreq(BMICAT)), fill=BMICAT),
           show.legend = FALSE,
           width=0.6, alpha=0.8) +
  theme(aspect.ratio=1) +
  theme_minimal() +
  labs(x = "BMI", y = "Count", title="BMI Distribution")
bar_bmi2


#SleepHrs distribution - BMI Group Comparison
boxplot(SLEPTIM2~BMICAT, data=brfss3, main="Box Plot of Sleep by BMI Group", 
        xlab="Reported BMI", ylab="Hours Slept")

# Plot Correlation Matrix with Correlation Coefficients
numcor2 <- cor(brfss1)
corrplot(numcor2, method = "color",type = "upper",  tl.col = "black", tl.cex = 0.5)

###############################################################################################

#########################################
#            Regularization             #
#########################################
#use L2 for feature selection - Rework for final


#########################################
#          Linear Regression            #
#########################################


#split data into training and testing dataset

brfss2 <- subset(brfss3, select = -c(ALCGRP, X_AGE_G, ASTHMA4, RACEGRP, MARITAL, GENHLTH2, HLTHPLN2, EDGROUP, INCOME3, BMICAT, EXERANY3))

set.seed(123)
row.number <- sample(x=1:nrow(brfss2), size=0.8*nrow(brfss2))
train.data <- brfss2[row.number,]
test.data <- brfss2[-row.number,]

dim(train.data) 
dim(test.data)   

prop.table(table(train.data$ALCDAY5))

linearmodel <- lm(SLEPTIM2~., data=train.data)
lm_summary <- summary(linearmodel)
lm_summary

lm_summary$r.squared


# Plots for Residual Analysis
#and linear Regression assumptions check

par(mfrow=c(2,2))
plot(linearmodel)
qqnorm(linearmodel$residuals); qqline(linearmodel$residuals)



#Monisha's code based on her screenshots; replaced SLEPTIM1 with SLEPTIM2)

mlinearmodel <- glm(ALCDAY5 ~ SLEPTIM2 + X_AGE_G + 
                      SMOKDAY2 + SEX + X_MRACE1 + GENHLTH + INCOME2 + X_BMI5CAT + 
                      ALCGRP + DRKMONTHLY + AGE2 + AGE3 + AGE4 + AGE5 + SMOKGRP + HISPANIC +
                      RACEGRP + BLACK + ASIAN + OTHRACE + FORMERMAR + GENHLTH2 + FAIRHLTH + 
                      POORHLTH + LOWED + SOMECOLL + INCOME3 + INC1 + INC2 + INC3 + INC4 + 
                      INC5 + INC6 + OVWT + EXERANY3, data = brfss)
stepAICm <- stepAIC(mlinearmodel, direction = 'backward')
stepAICm$anova

# Final Model:
ALCDAY5 ~ SLEPTIM2 + X_AGE_G + SMOKDAY2 + SEX + X_MRACE1 + GENHLTH +
  INCOME2 + X_BMI5CAT + ALCGRP + DRKMONTHLY + AGE2 + AGE3 +
  AGE4 + AGE5 + SMOKGRP + HISPANIC + RACEGRP + BLACK + ASIAN +
  OTHRACE + FORMERMAR + GENHLTH2 + POORHLTH + LOWED + SOMECOLL +
  INCOME3 + INC1 + INC2 + INC3 + INC4 + INC5 + INC6 + OVWT

m2linearmodel <- glm(ALCDAY5 ~ SLEPTIM2 + X_AGE_G + SMOKDAY2 + SEX + X_MRACE1 + GENHLTH +
                       INCOME2 + X_BMI5CAT + ALCGRP + DRKMONTHLY + AGE2 + AGE3 +
                       AGE4 + AGE5 + SMOKGRP + HISPANIC + RACEGRP + BLACK + ASIAN +
                       OTHRACE + FORMERMAR + GENHLTH2 + POORHLTH + LOWED + SOMECOLL +
                       INCOME3 + INC1 + INC2 + INC3 + INC4 + INC5 + INC6 + OVWT, data = brfss)

lm_summary <- summary(m2linearmodel)
lm_summary

# Plots for Residual Analysis
par(mfrow=c(2,2))
plot(m2linearmodel)
qqnorm(m2linearmodel$residuals); qqline(m2linearmodel$residuals)

###############################################################################################
# ASTHMA
set.seed(456)
row.number <- sample(x=1:nrow(brfss3), size=0.8*nrow(brfss3))
trainfactors <- brfss3[row.number,]
testfactors <- brfss3[-row.number,]

glmmodel <- glm(ASTHMA4~., family = binomial(link = 'logit'), data = trainfactors)
summary(glmmodel)
anova(glmmodel, test="Chisq")

# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#    
# ALCDAY5     1     44.8     45865      28099  2.16e-11 ***
# ASTHMA3     1  28099.0     45864          0 < 2.2e-16 ***


#prediction
prefactors <-as.numeric(predict(glmmodel,newdata = testfactors,type = "response")>0.5)
obs_p_lr = data.frame(prob=prefactors,obs=testfactors$ASTHMA4)

#ROC curve
par(mfrow=c(1,1))
lr_roc <- roc(testfactors$ASTHMA4,prefactors)

plot(lr_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC Curve ')



#########################################
#            Decision Tree              #
#########################################
#will fix later!!
#use brfss4 

###############################################
# Create a subset to filter variables for Decision Tree

brfss4 <- subset(brfss2, select = -c(ASTHMA3, SMOKE100, SMOKDAY2, X_MRACE1, 
                              INCOME2, X_BMI5CAT, SMOKGRP, MARGRP, HLTHPLN1))


# Build the Decision Tree Model
dtree <- rpart(SLEPTIM2~., data=train.data, method = "anova", control=rpart.control(minsplit=5,cp=0.004))


###############################################################################################
###############################################
#          Random Forest                      #
###############################################
# Will tune for final 

#Fitting Random Forest model

set.seed(666)
traindata<-brfss3[sample(1:nrow(brfss3),round(0.8*nrow(brfss3))),]
testdata<-brfss3[-sample(1:nrow(brfss3),round(0.8*nrow(brfss3))),]

treeRF1 <- randomForest(traindata$SLEPTIM2 ~., traindata, ntree=500)
treeRF1

# Call:
#   randomForest(formula = traindata$SLEPTIM2 ~ ., data = traindata,      ntree = 500) 
# Type of random forest: regression
# Number of trees: 5
# No. of variables tried at each split: 13
# 
# Mean of squared residuals: 3.000807
# % Var explained: -39.59

varImp(treeRF2)


#Predict Output
predictedRF <- predict(treeRF1,testdata)

# Checking classification accuracy
acctest <- table(predictedRF, testdata$SLEPTIM2)


# AUC Curve

treeRF_roc<- multiclass.roc(testdata$SLEPTIM2, as.numeric(predictedRF))

auc(treeRF_roc)
# Multi-class area under the curve: 0.8827


##TUNING REQUIRED
####
### Tuning ### 

# Tuning parameters:
# number of trees 
# number of variables tried at each split ("mtry")
