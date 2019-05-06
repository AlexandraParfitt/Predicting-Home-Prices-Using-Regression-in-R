setwd("~/Desktop/Villanova University/Spring 2019/Regression Analysis/Project")

library(readr)
library(ggplot2)
library(tidyr)
library(MASS)
library(dplyr)

houses.subset2.zip <- read_csv("houses_no_33.csv")
income <- read_csv("zipcodeincomes.xlsx")


houses$yr_renovated[houses$yr_renovated>0] <- 1 #changed all zeros to "N/A" in the yr_renovated column

names(houses.subset2.zip)

houses.subset2.zip$price<-log(houses.subset2.zip$price) #log transform on price
houses.subset2.zip$sqft_living<-log(houses.subset2.zip$sqft_living) #log transform on sqftliving...she willll like that



houses.subset2.zip.income <-left_join(houses.subset2.zip, income, by = c("zipcode"="Zip"))


#Color-Coded Residual Plots to indicate Outliers
attach(houses.subset2.zip.income)
reg_alex <-lm(price ~ bathrooms + view + sqft_living + waterfront + grade + yr_built + lat + sqft_living15 + waterfront*lat + Median + Mean)
summary(reg_alex)

model.resid <- resid(reg_alex)

#plot(houses$sqft_living, model.resid, ylab="Residuals", xlab="Sqft_Living", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$waterfront, model.resid, ylab="Residuals", xlab="Waterfront", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$grade, model.resid, ylab="Residuals", xlab="Grade", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$yr_built, model.resid, ylab="Residuals", xlab="yr_built", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$lat, model.resid, ylab="Residuals", xlab="Lat", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$sqft_living15, model.resid, ylab="Residuals", xlab="sqft_living15", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$bathrooms, model.resid, ylab="Residuals", xlab="Bathrooms", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$view, model.resid, ylab="Residuals", xlab="View", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)
#plot(houses$waterfront*houses$lat, model.resid, ylab="Residuals", xlab="Waterfront*Lat", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)

#plot(reg_alex)
#plot(reg_alex, model.resid, ylab="Residuals", xlab="Waterfront*Lat", col=ifelse(houses$`Outlier?`==1, "red", "black"))
#abline(0, 0)