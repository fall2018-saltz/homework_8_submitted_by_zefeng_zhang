
# Homework 8 – Submitted by Zefeng Zhang (Ben) on Oct 23, 2018

####################################
#Part A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveySherison), as a JSON file.
#Hint: Don’t forget to use setwd() to make sure that R is looking in the right folder for your text file.

setwd("~/Desktop/Classes/687/Project ")
library(RJSONIO)
library(rjson)
library(jsonlite) 

#reads json and passes list into hotel
hotel <- read_json("hotelSurveySherison.json", simplifyVector = TRUE)

#2.	Use the str command to make sure you can see the following attributes
hotelSurvey <- data.frame(hotel)
str(hotelSurvey)

#########################
#Part B: Explore the data  
#1.	Create bivariate plots for each of the attributes.
#Your code should produce nine separate plots. 
#Make sure the Y-axis and X-axis are labeled. 
#Keeping in mind that the overall customer satisfacation is the outcome (or dependent) variable, which axis should it go on in your plots?
#  Hint: use the jitter command, so you can see all the surveys (something such as) jitter(hotelSurvey$checkInSat)

hotelSurvey$hotelState<-as.numeric(hotelSurvey$hotelState)
hotelSurvey$gender<-as.numeric(hotelSurvey$gender)

#plots X-axis with hotelSize and Y-axis with overallCustSat
plot(jitter(hotelSurvey$hotelSize), hotelSurvey$overallCustSat)

#plots X-axis with checkInSat and Y-axis with overallCustSat
plot(jitter(hotelSurvey$checkInSat), hotelSurvey$overallCustSat)

#plots X-axis with hotelState and Y-axis with overallCustSat
plot(jitter(hotelSurvey$hotelState), hotelSurvey$overallCustSat)

#plots X-axis with hotelClean and Y-axis with overallCustSat
plot(jitter(hotelSurvey$hotelClean), hotelSurvey$overallCustSat)

#plots X-axis with hotelFriendly and Y-axis with overallCustSat
plot(jitter(hotelSurvey$hotelFriendly), hotelSurvey$overallCustSat)

#plots X-axis with gender and Y-axis with overallCustSat
plot(jitter(hotelSurvey$gender), hotelSurvey$overallCustSat)

#plots X-axis with guestAge and Y-axis with overallCustSat
plot(jitter(hotelSurvey$guestAge), hotelSurvey$overallCustSat)

#plots X-axis with lengthOfStay and Y-axis with overallCustSat
plot(jitter(hotelSurvey$lengthOfStay), hotelSurvey$overallCustSat)

#plots X-axis with whenBookedTrip and Y-axis with overallCustSat
plot(jitter(hotelSurvey$whenBookedTrip), hotelSurvey$overallCustSat)


#2.What do you observe from the plots? Note via a block comment
#the hotel cleaness, friendliness and checkin satisfaction seem to have liner relationship with the the overall customer satisfacation level

#Part B: Generate a linear model  
#3.	Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response). 
#Refer to page 202 in the text for syntax and explanations of lm( ). 
#Make sure to include all predictors in one model – NOT different models each with one predictor.


#remove the last columns created with the free text. 
hotelSurvey <- hotelSurvey[,-11]
model1<- lm(formula = overallCustSat ~., data=hotelSurvey)
summary(model1)

#4.	
#Report the R-Squared in a comment. 
#Which of the predictors are statistically significant in the model?
#In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant. 

#a).R-squared: 0.668
#b).checkInSat,hotelClean,hotelFriendly,guestAge, lengthOfStay and whenBookedTrip shows statistically significance
#c) 
#Coefficients of importance: 
#                                 Estimate 
#checkInSat                     -2.381e-01 
#hotelClean                      4.042e-02  
#hotelFriendly                   1.122e+00  
#guestAge                       -1.205e-01 
#lengthOfStay                   -3.284e-01 
#whenBookedTrip                  6.421e-03

#5.	Write a block comment that explains in a narrative your overall interpretation of the model. Make sure to refer to each variable (one dependent and three independent) by a descriptive name (i.e., not X1, X2, etc.).
#The value of R-squared (0.668) shows that the model is not bad.
#Overall, checkInSat, hotelClean, hotelFriendly, guestAge, lenghtOfStay, and whenBookedTrip shows statistical significance 

#Part C: Generate a different linear model  
#8.	Next, create a different regression model predicting the overall customer satisfaction from the one variable you think is best.  

model2 <- lm(formula = overallCustSat ~ hotelFriendly, data=hotelSurvey)
summary(model2)

#R-squared:  0.378
#9.	Write a block comment comparing the two lm models
#The R-Squared value of is  much lower than the full model (model 1), which shows the fitness of model with more variables seems better than model with only one variable

