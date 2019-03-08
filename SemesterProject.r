library(readr)
library(ggplot2)
library(MASS)
library(faraway)
houses <- read.csv("houses.txt", header=T)
attach(houses)

lsr <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + sqft_above + sqft_basement + sqft_living15 + sqft_lot15 + grade + condition + yr_built + yr_renovated + zipcode + lat + long, data = houses)

vif(lsr)

bothstep.model <- stepAIC(lsr, direction = "both", trace = FALSE)
summary(bothstep.model)

vif(bothstep.model)
