#Dalton Anderson

library(readxl)
library(dplyr)
library(ggplot2)

rm(list=ls())

#preprocessing

#1.) Load in dataset
#Import data
pop_master <- read_excel("6304 Module 7 Assignment Data.xlsx")

names(pop_master)
colnames(pop_master)=tolower(make.names(colnames(pop_master)))
names(pop_master)


#2.) Create a new "index" variable in the data frame which will be an identifying sequential numbering of rows 
#from 1 to the number of rows in the data frame.
pop <- tibble::rowid_to_column(pop_master, "year_index")

#analysis

#1.) Show a plot of the data using the number of visitors as the "y" variable in the plot.
#part of code from class - not all my code

plot(pop$year_index,pop$china.visitors,pch=19,
     main="China Visitors -- Raw Data")
plot(pop$year_index,pop$china.visitors,type="l",pch=19,
     main="China Visitors -- Raw Data")
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="China Visitors -- Raw Data")
points(pop.out$fitted.values,type="l",lwd=3,col="red")
cor(pop$pop$year_index,pop.out$fitted.values)
plot(pop$item,rstandard(pop.out),pch=19,type="o",
     main="pop Model, Standardized Residuals")
abline(0,0,col="red",lwd=3)

#2.) Using all the data parameterize a base time series simple regression model using "index" as the independent variable.
#Show the summary of your regression output.

pop.out = lm(china.visitors ~ year_index, data = pop)
summary(pop.out)

#3.) Drawing on Analysis Part 1 above, show a properly titled plot of the time series data with the simple regression line 
#layered on the graph in a contrasting color.

plot(pop$year_index,pop$china.visitors,pch=19,
     main="China Visitors -- Raw Data vs Model Fit")
plot(pop$year_index,pop$china.visitors,type="l",pch=19,
     main="China Visitors -- Raw Data vs Model Fit")
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="China Visitors -- Raw Data vs Model Fit")
points(pop.out$fitted.values,type="l",lwd=3,col="red")
cor(pop$pop$year_index,pop.out$fitted.values)
plot(pop$item,rstandard(pop.out),pch=19,type="o",
     main="pop Model, Standardized Residuals")
abline(0,0,col="red",lwd=3)

#4.)	Execute and interpret a Durbin-Watson test on your model results.
#durbin-watson ranges from 0 to 4
#2 is no autocorrelation
#1.5 to 2.5 is a normal range
#serial 0 is closer to postive serial 4 is negative
durbin.out=car::durbinWatsonTest(pop.out)
durbin.out



#interpretation
#reject the null the hypothsis with a p-value of zero, there is a postive autocorrelation.
#we need to get a better understanding on these fluctuations then fit them into the model.


#5.)	Note the original data appears to have a pronounced cyclical pattern.  
#Assuming the complete cycles are four quarters long, construct a set of seasonal indices
#which describe the typical annual fluctuations in visitors.  Use these indices to deseasonalize the visitors data.  
#Store this deseasonalized data in a column in the original data frame.

#Making Seasonal Indices
#code from class
indices=data.frame(quarter=1:4,average=0,index=0)
for(i in 1:4) {
  count=0
  for(j in 1:nrow(pop)) {
    if(i==pop$quarter[j]) {
      indices$average[i]=indices$average[i]+pop$china.visitors[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=indices$average[i]/mean(pop$china.visitors)
}
#Deseasonalizing the original data
for(i in 1:4){
  for(j in 1:nrow(pop)){
    if(i==pop$quarter[j]){
      pop$deseason.china.visitors[j]=pop$china.visitors[j]/indices$index[i]
    }
  }
}

#desrasonlized plot
desreg.out=lm(deseason.china.visitors~year_index,data=pop)
summary(desreg.out)
points(pop$year_index,desreg.out$fitted.values,type="l",
       lwd=3,col="blue")
plot(pop$year_index,rstandard(desreg.out),pch=19,type="o",
     main="Deseasonalized Forecasts -- Standardized Errors")
abline(0,0,col="red",lwd=3)
cor(pop$china.visitors,desreg.out)

#Comparison of Seasonalized and Deseasonalized
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original and Deseasonalized Data")
points(pop$year_index,pop$deseason.china.visitors,type = "o",
       pch=19,col="red")
#errors
for(j in 1:nrow(pop)) {
  xx =pop$quarter[j]
  pop$reseason.y.hat[j]=desreg.out$fitted.values[j]*indices$index[xx]
  pop$reseason.error[j]=pop$china.visitors[j]-pop$resonason.y.hat[j]
}
#reseasonalizing forecasts
pop$deseason.forecast=desreg.out$fitted.values
for(i in 1:4){
  for(j in 1:nrow(pop)){
    if(i==pop$quarter[j]){
      pop$reseason.forecast[j]=pop$deseason.forecast[j]*
        indices$index[i]
    }
  }
}

plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original Data and Reseasonalized Forecasts")
points(pop$year_index,pop$reseason.forecast,
       type="o",pch=19,col="red")
#I am not sure if this worked or not.
#fixed









#6.)	Using the deseasonalized data parameterize four different regression models.
#A simple regression model will be the base case to be followed by second order, third order, and fourth order polynomial models 
#which attempt to describe the longer-term secular fluctuations in the deseasonalized data.  

#create models together
#base
pop.out = lm(china.visitors ~ year_index, data = pop)
#create degree columns in model dataset
pop$quarter2 = pop$quarter^2
pop$quarter3 = pop$quarter^3 
pop$quarter4 = pop$quarter^4 
#deseasonalized
pop.outds = lm(deseason.china.visitors ~ year_index, data = pop)
summary(pop.outds)
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original and Deseasonalized Data")
points(pop$year_index,pop.outds$fitted.values,type = "o",
       pch=19,col="red")
#deseasonalized ^2
pop.outds2 = lm(deseason.china.visitors ~ pop$year_index + I(pop$year_index^2), data = pop)
summary(pop.outds2)
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original and Deseasonalized Data ^2")
points(pop$year_index,pop.outds2$fitted.values,type = "o",
       pch=19,col="red")
#deseasonalized ^3
pop.outds3 = lm(deseason.china.visitors ~ year_index + I(pop$year_index^2) + I(pop$year_index^3), data = pop)
summary(pop.outds3)
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original and Deseasonalized Data ^3")
points(pop$year_index,pop.outds3$fitted.values,type = "o",
       pch=19,col="red")
#deseasonalized ^4
pop.outds4 = lm(deseason.china.visitors ~ year_index + I(pop$year_index^2) + I(pop$year_index^3) + I(pop$year_index^4), data = pop)
summary(pop.outds4)
plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original and Deseasonalized Data ^4")
points(pop$year_index,pop.outds4$fitted.values,type = "o",
       pch=19,col="red")
#durbinWatsonTest
#base
summary(pop.out)
durbin.out=car::durbinWatsonTest(pop.out)
durbin.out
#deseasonalized 
summary(pop.outds)
durbin.outds=car::durbinWatsonTest(pop.outds)
durbin.outds
#deseasonalized ^2
summary(pop.outds2)
durbin.outds2=car::durbinWatsonTest(pop.outds2)
durbin.outds2
#deseasonalized ^3
summary(pop.outds3)
durbin.outds3=car::durbinWatsonTest(pop.outds3)
durbin.outds3
#deseasonalized ^4
summary(pop.outds4)
durbin.outds4=car::durbinWatsonTest(pop.outds4)
durbin.outds4


#7.)	Reseasonalize the fitted values for each of the four models, 
#storing the reseasonalized values in separate columns in the original data frame.  
#Drawing on Analysis Part 3 above, construct a plot showing the original data and the
#fitted values for each of the four regression models.  
#Show the four sets of fitted values plots in contrasting colors and title the graph appropriately.

#base
#reseasonalizing forecasts
pop$deseason.forecast=desreg.out$fitted.values
for(i in 1:4){
  for(j in 1:nrow(pop)){
    if(i==pop$quarter[j]){
      pop$reseason.forecast[j]=pop$deseason.forecast[j]*
        indices$index[i]
    }
  }
}
#-------------------------------------------------
#deseasonalized ^2
#reseasonalizing forecasts
pop$deseason.forecast2=pop.outds2$fitted.values
for(i in 1:4){
  for(j in 1:nrow(pop)){
    if(i==pop$quarter[j]){
      pop$reseason.forecast[j]=pop$deseason.forecast2[j]*
        indices$index[i]
    }
  }
}
#-------------------------------------------------
#deseasonalized ^3
#reseasonalizing forecasts
pop$deseason.forecast3=pop.outds3$fitted.values
for(i in 1:4){
  for(j in 1:nrow(pop)){
    if(i==pop$quarter[j]){
      pop$reseason.forecast[j]=pop$deseason.forecast3[j]*
        indices$index[i]
    }
  }
}
#-------------------------------------------------
#deseasonalized ^4
#reseasonalizing forecasts
pop$deseason.forecast4=pop.outds4$fitted.values
for(i in 1:4){
  for(j in 1:nrow(pop)){
    if(i==pop$quarter[j]){
      pop$reseason.forecast[j]=pop$deseason.forecast4[j]*
        indices$index[i]
    }
  }
}

plot(pop$year_index,pop$china.visitors,type="o",pch=19,
     main="Original and Deseasonalized Data")
points(pop$year_index,pop.outds$fitted.values,type = "o",
       pch=19,col="red")
points(pop$year_index,pop.outds2$fitted.values,type = "o",
       pch=19,col="blue")
points(pop$year_index,pop.outds3$fitted.values,type = "o",
       pch=19,col="green")
points(pop$year_index,pop.outds4$fitted.values,type = "o",
       pch=19,col="yellow")




#8.)	Select the model which in your view is the best fit to the deseasonalized data.  
#Give a brief justification as to why you believe your selection is the best fit. 
sumary(pop.outds3)
#interpretation
#I would go with pop.out3 the 3rd degree model since there is starting to see diminishing returns at 4th degree
#the 4th degree model isn't much better than the 3rd with an additional computational cost 

