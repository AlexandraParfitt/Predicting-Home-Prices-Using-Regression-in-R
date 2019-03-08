setwd("~/Desktop/Spring 2019/MAT8406/R codes")

library(readr)
library(ggplot2)
library(MASS)

#read data and make initial changes
houses <-read.csv("kc_house_data.csv", header = T)
houses$yr_renovated[houses$yr_renovated==0] <- NA #changed all zeros to "N/A" in the yr_renovated column
houses.subset<-houses[,c(3:21)] #removed id and date variables from the data set


lsr <- lm(price ~ ., data=houses.subset) #lm with all remaining variables 
summary(lsr) #initial R-squared is 0.7662, sqf_basement "not definined because of singularities" suggesting that it is strongly correlated with another variable
    #Turns out sqft_living = sqft_above + sqft_basement that is what is causing this issue.

houses.subset2<-houses[,c(3:12,15,16,18:21)] #removed sqft_basement and sqft_living also removed zipcode because it's explained by long and lat and is categorical w/ 70 factors
  #removing just sqft_basement has no effect on R as it is explained by other variables. Removing other two does not change it much 0.7543
  #also tried removing the last 2 columns (15 surrounding houeses) which brought R-squared to 0.7361 so I left them in
lsr2 <- lm(price ~ ., data=houses.subset2) #lm with all remaining variables 
summary(lsr2)


bothstep.model <- stepAIC(lsr2, direction = "both", trace = FALSE)  
summary(bothstep.model)
#step model further removes bathrooms, sqft_lot, view, and long. No significant decrease in R-squared (0.7543->0.7538)
houses.subset3<-houses[,c(3,4,6,8,9,11,12,15,16,18,20,21)] #removed columns to match bothstep model
lsr3 <- lm(price ~ ., data=houses.subset3) #lm with all remaining variables 
summary(lsr3)

source("Scripts:pairs.panels.r")
pairs.panels(houses.subset3[ ,1:6]) #looks like a trasformation on price may be helpful
pairs.panels(houses.subset3[ ,c(1,7:12)]) 

#transformations
houses.trans<-houses.subset3
houses.trans$price<-log(houses.subset3$price)
trans.lsr3<-lm(price ~ ., data=houses.trans) #lm3 with price transformed to log price
summary(trans.lsr3) #brings R-squared up to 0.7919!
  #Note tried transforing sqft. living did not improve R-squared much

pairs.panels(houses.trans[ ,1:6]) 
pairs.panels(houses.trans[ ,c(1,7:12)]) 


plot(houses.trans$price,houses.trans$sqft_living, cex.axis=1.5,     
     cex.lab=1.6,cex.main=1.8,cex=1.5)
plot(houses.trans$price,houses.trans$bedrooms, cex.axis=1.5,     
     cex.lab=1.6,cex.main=1.8,cex=1.5) #might want to remove 33 bedroom outlier

