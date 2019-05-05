library(readr)
library(ggplot2)
library(MASS)
library(leaps)
#read data and make initial changes

library(readxl)
houses <- read_excel("houses_no_33.xlsx")
reg_houses <-lm(price~ .,data=houses)
summary(reg_houses)
renovated<-c(1:length(houses$yr_renovated))
for(i in 1:length(houses$yr_renovated)){
  if(houses$yr_renovated[i] == 0){renovated[i]<-0}
  else{renovated[i]<-1}
  }
houses$yr_renovated<-renovated
houses$yr_renovated<-as.factor(houses$yr_renovated)
houses$date<-as.Date(houses$date)
houses$waterfront<-as.factor(houses$waterfront)

sapply(houses, class) #check classes


houses.subset2<-houses[,c(2:13,15:16,18:21)] #removed ID, removed sqft_basement because of multicollinarity and also removed zipcode because it's explained by long and lat and is categorical w/ 70 factors
attach(houses.subset2)

houses.subset2$price<-log(houses.subset2$price) #log transform on price
  colnames(houses.subset2)[colnames(houses.subset2)=="price"] <- "ln_price" #changing the column name

  
  plot(houses$sqft_living,houses$price)
      plot(sqft_living,ln_price) 
        houses.subset2$sqft_living<-log(houses.subset2$sqft_living) #log transform on sqftliving
        colnames(houses.subset2)[colnames(houses.subset2)=="sqft_living"] <- "ln_sqft_living" #changing the column name
        plot(ln_sqft_living,ln_price)

names(houses.subset2)
attch(houses.subset2)
subsets <- regsubsets(ln_price ~ ., data=houses.subset2)
plot(subsets, scale = "Cp")
plot(subsets, scale = "adjr2")
plot(subsets, scale = "bic")

reg_alex <-lm(ln_price ~ ln_sqft_living + bathrooms + waterfront + view + grade + yr_built + lat + sqft_living15 +lat*waterfront)
summary(reg_alex)





houses.subset3<-houses.subset2[,c(2,4,5,8,9,11,13,15,17)]
attach(houses.subset3)

library(glmnet)                      # Load package 'glmnet'
waterlat<-c(1:length(houses$lat))  #Create a new column for the interaction term values
for(i in 1:length(houses$lat)){
  if(houses$waterfront[i] == 0){waterlat[i]<-0}
  else{waterlat[i]<-houses$lat[i]}
}
x<-houses.subset3[2:9]         # Combine variables by column
x<-cbind(x,waterlat)
x<-as.matrix(x)
y<-houses.subset3[1]
y<-as.matrix(y)
grid<-10^seq(10,-2, length=100)        # Create a grid of lambda values
ridge.mod=cv.glmnet(x,y,lambda=grid,   # Build a CV ridge regression          
                    nfold=length(y),                       # nfold=sample size, leave-one-out CV
                    alpha=0)       # alpha=0, ridge reg is fit
plot(log10(ridge.mod$lambda), ridge.mod$cvm,      # Plot average CV error versus log(lambda)
     xlab="log10(Lambda)", ylab="CV Error")     
abline(v = log10(ridge.mod$lambda.min), lty = 3)

(lambda=ridge.mod$lambda.min)        # The lambda that minimizes CV error
predict(ridge.mod,s=lambda,          # Obtain ridge reg coefs
        type="coefficients")      
min(ridge.mod$cvm)


