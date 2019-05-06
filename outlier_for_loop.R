setwd("~/Desktop/Villanova University/Spring 2019/Regression Analysis/Project")

library(readr)
library(ggplot2)
library(tidyr)
library(MASS)
library(dplyr)

houses <- read_csv("houses_no_33.csv")

houses$yr_renovated[houses$yr_renovated>0] <- 1 #changed all zeros to "N/A" in the yr_renovated column

names(houses)

houses.subset2<-houses[,c(2:12,15,16,18:21)] #removed sqft_basement and sqft_living also removed zipcode because it's explained by long and lat and is categorical w/ 70 factors

houses.subset2$price<-log(houses.subset2$price) #log transform on price
houses.subset2$sqft_living<-log(houses.subset2$sqft_living) #log transform on sqftliving...she willll like that


#Creating a new column "Outlier?" in data frame
outlier.vector <- rep.int(0,length(houses$id))
houses["Outlier?"] <- outlier.vector


attach(houses.subset2)

#Cooks
reg_water6 <-lm(price ~ bathrooms + view + sqft_living + waterfront + grade + yr_built + lat + sqft_living15 + lat*waterfront)
cooks_houses <- cooks.distance(reg_water6)
cooks_houses > 1
sum(cooks_houses>1)

#Leverage
leverages <- hatvalues(reg_water6)
sum(leverages > 9/21597)

#Dffits
dffits_houses <- dffits(reg_water6)
dffits_houses_limit <-2*sqrt(9/21597)
dffits_houses_limit
dffits_influence_a <- sum(dffits_houses > dffits_houses_limit)
dffits_influence_b <-sum(dffits_houses < -dffits_houses_limit)
total_dffits <- dffits_influence_a + dffits_influence_b
total_dffits

#DfBetas
betas <-dfbetas(reg_water6)
betas_limit <-2/sqrt(21597)
betas_limit
betas_influence_a <-sum(betas > betas_limit)
betas_influence_b <-sum(betas < -betas_limit)
total_betas <- betas_influence_a + betas_influence_b
total_betas


#Indicating Outliers in Dataframe
print.vector <- c()

test_vector <- leverages
for(i in 1:length(leverages)){
  if(abs(dffits_houses[i])>dffits_houses_limit && abs(betas[i])>betas_limit && leverages[i] > 9/21597){
    test_vector[i] <- 1
    print.vector <- cbind(print.vector,as.numeric(names(leverages)[i]))
    #houses$`Outlier?`[i] <- 1
  } # end if
  else {
    test_vector[i] <- 0
  } # end else
} # end for
sum(test_vector)

for(i in 1:length(print.vector)){
  houses$`Outlier?`[print.vector[i]] <- 1
}






#Color-Coded Residual Plots to indicate Outliers
reg_alex <-lm(price ~ bathrooms + view + sqft_living + waterfront + grade + yr_built + lat + sqft_living15 + waterfront*lat)
summary(reg_alex)

model.resid <- resid(reg_alex)

plot(houses$sqft_living, model.resid, ylab="Residuals", xlab="Sqft_Living", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$waterfront, model.resid, ylab="Residuals", xlab="Waterfront", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$grade, model.resid, ylab="Residuals", xlab="Grade", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$yr_built, model.resid, ylab="Residuals", xlab="yr_built", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$lat, model.resid, ylab="Residuals", xlab="Lat", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$sqft_living15, model.resid, ylab="Residuals", xlab="sqft_living15", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$bathrooms, model.resid, ylab="Residuals", xlab="Bathrooms", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$view, model.resid, ylab="Residuals", xlab="View", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)
plot(houses$waterfront*houses$lat, model.resid, ylab="Residuals", xlab="Waterfront*Lat", col=ifelse(houses$`Outlier?`==1, "red", "black"))
abline(0, 0)

plot(reg_alex)
#plot(reg_alex, model.resid, ylab="Residuals", xlab="Waterfront*Lat", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)