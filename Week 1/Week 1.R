setwd("D:/Dropbox/Data Science/Coursera/Practical Time Series Analysis R - The State University of New York")


######################################
## Class 1
######################################
install.packages("faraway", lib="D:/Program Files/R/R-4.1.2/library")

# Package with several datasets
data(package='faraway')

# load a dataset
data(coagulation, package='faraway')

ls()

coagulation

plot(coag~diet, data=coagulation)


summary(coagulation)

#n the faraway package, we have a data set named "worldcup" giving various R: data on players from the 2010 World Cup.
#The variable "Time" tells us the time played in minutes by the various players. 
#After you have installed and loaded the faraway package, find the average time played by using the command
#mean(worldcup$Time)
#What is the average time played? You may round to 2 or three places after the decimal point. 

data(worldcup,package='faraway')

worldcup

mean(worldcup$Time)


######################################
## Class 2 - Concatenation, Five-number summary, Standard Deviation
######################################

data.1=c(35, 8, 10, 23, 42)
data.1
print(data.1)

summary(data.1)
mean(data.1)
sum(data.1)/5
sd(data.1)



######################################
## Class 3 - Histogram
######################################

small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)

#
hist(small.size.dataset)
hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset), col='red', lwd=5)


#
hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)
lines(density(small.size.dataset), col='red', lwd=5)




######################################
## Class 4 - Scatterplot
######################################



set.seed(2016)  # There is a typo in the video (set.seed=2016)
Test_1_scores=round(rnorm(50, 78, 10))
Test_2_scores=round(rnorm(50, 78, 14))

Test_1_scores # Data won't be the same with the data generated in the video lecture since there was a typo in set.seed. 
Test_2_scores # Data won't be the same with the data generated in the video lecture since there was a typo in set.seed. 

plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores', col='blue')






######################################
## Final Test
######################################

# 1- Obtain 5-number summary.
# 2 - Find the summary of the dataset given in the following code block. What is the 3rd quartile?
data=c(37, 86, 79, 95, 61, 93, 19, 98, 121, 26, 39, 11, 26, 75, 29,130, 42, 8) # Edit this line
summary(data)


# 3 - We look at the dataset titled 'cheddar' from 'faraway' package. Continue the code in the following code block to look at the description of the dataset 'cheddar' using help() routine. How many observations and variables are there in the dataset? 
library(faraway)
data(cheddar,package='faraway')
help(cheddar)

# 4 - We are still working on the dataset 'cheddar' from the package 'faraway'. Apply simple linear regression model for the bivariate data 'taste' (modeled as a random variable Y) vs 'H2S' (modeled as a random variable X) in the dataset 'cheddar' in using lm() routine in the following code block. What is the model?
library(faraway)
m=lm(taste~H2S, data=cheddar)
summary(m)


# 5- What is the sum of the residuals in the simple linear regression model of Question 4? 
library(faraway)
m=lm(taste~H2S, data=cheddar)
m_residuals = resid(m)
sum(m_residuals)


# 6 - What is the sum of the fitted values in the simple linear regression model of Question 4? We can get the fitted values by using lm()$fitted routine.
library(faraway)
m=lm(taste~H2S, data=cheddar)
sum(m$fitted)


# 8 - Suppose you are testing the null hypothesis that a population mean is 0 against the alternative that it is not zero at the alpha=0.05 level of significance.
# Given the following function call and printout, can you reject your null hypothesis?
data = c(7,  5,  1,  7,  2,  5,  2,  4, 10,  6);
t.test(data, alternative = "two.sided", paired=FALSE)













