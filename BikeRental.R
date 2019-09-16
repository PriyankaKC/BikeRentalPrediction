##########################Setup the environment##########################

rm(list=ls())

setwd('D:/Study/R programs')
getwd()

##########################Load the data and check how the data is##########################

RentalData = read.csv('day.csv',header= TRUE, sep = ',', na.strings = c("", " ","NA"))
summary(RentalData)
head(RentalData)

##########################Load all the required libraries##########################

libraries = c("plyr","dplyr","ggplot2", "rpart","DMwR", "randomForest", "usdm", "corrgram", "DataCombine", "sp", "raster", "usdm", "ggplot2")
lapply(X = libraries, require, character.only = TRUE)
rm(libraries)

##########################Data exploration to see how the data is what its properties are##########################

#first few rows
head(RentalData)

#Dimensions of the data
dim(RentalData)

#name of columns
names(RentalData)

#Datatype of variables
str(RentalData)


##########################Feature engineering#########################

#Create columns

RentalData$actual_temp <- RentalData$temp*47 - 8
RentalData$actual_atemp <- RentalData$atemp*66 - 16
RentalData$actual_windspeed <- RentalData$windspeed*67
RentalData$actual_humidity <- RentalData$hum*100
RentalData$actual_season <- factor(x = RentalData$season, levels = c(1,2,3,4), labels = c('Spring', 'Summer', 'Fall', 'Winter'))
RentalData$actual_yr <- factor(x = RentalData$yr, levels = c(0,1), labels = c('2011','2012'))
RentalData$actual_holiday <- factor(x = RentalData$holiday, levels =c(0,1), labels = c('Working Day', 'Holiday'))
RentalData$actual_weathersit <- factor(x = RentalData$weathersit, levels = c(1,2,3,4), labels = c('Clear','Cloudy/Mist', 'Rain/Snow/Fog', 'HeavyRain/Snow/Fog'))
RentalData$actual_workingday <- factor(x = RentalData$workingday, levels = c(0,1), labels = c('Non Working', 'Holiday'))

RentalData$weathersit <- as.factor(RentalData$weathersit)
RentalData$season <- as.factor(RentalData$season)
RentalData$dteday <- as.character(RentalData$dteday)
RentalData$mnth <- as.factor(RentalData$mnth)
RentalData$weekday <- as.factor(RentalData$weekday)
RentalData$workingday <- as.factor(RentalData$workingday)
RentalData$yr <- as.factor(RentalData$yr)
RentalData$holiday <- as.factor(RentalData$holiday)

##########################Replace all the missing values with NA##########################
sapply(RentalData, function(x) {sum(is.na(x))})


##########################Data exploration using graphs##########################
#Check the distribution of categorical data using bar graph

library(ggplot2)
bar1 = ggplot(data = RentalData, aes(x = actual_season)) + geom_bar(fill = '#FF6666') + ggtitle("Count of Seasons")
bar2 = ggplot(data = RentalData, aes(x = actual_weathersit)) +geom_bar(fill = 'blue') +ggtitle("Weather Situation distribution")
bar3 = ggplot(data = RentalData, aes(x = actual_holiday)) +geom_bar(fill = 'orange') +ggtitle("Holiday distribution")
bar4 = ggplot(data = RentalData, aes(x = actual_workingday)) +geom_bar(fill = 'red') +ggtitle("Working Day Distribution")

#Plot all the graphs together
gridExtra::grid.arrange(bar1, bar2, bar3, bar4, ncol=2)

##########################Check the distribution of numerical data using histogram##########################
bar1 = ggplot(data = RentalData, aes(x = actual_temp)) + geom_histogram(fill = '#999999', bins = 25) + ggtitle("Temperature Distribution")
bar2 = ggplot(data = RentalData, aes(x =  actual_atemp)) +geom_histogram(fill = '#E69F00', bins=25) + ggtitle("Actual Temperature Distribution")
bar3 = ggplot(data = RentalData, aes(x = actual_humidity)) +geom_histogram(fill = '#56B4E9', bins =25) +ggtitle("Humidity Distribution")
bar4 = ggplot(data = RentalData, aes(x= actual_windspeed)) + geom_histogram(fill = '#009E73', bins =25) +ggtitle("WindSpeed Distribution")

#Plot all the histograms together
gridExtra::grid.arrange(bar1, bar2, bar3, bar4, ncol =2)


##########################Check distribution of data using scatter plots##########################
scat1 = ggplot(data = RentalData, aes(x = actual_temp, y = cnt)) +geom_point(colour = '#009E73')+ ggtitle("Scatter plot of Temperature") + xlab ('Temperature') + ylab ('Bike Count')
scat2 = ggplot(data = RentalData, aes(x = actual_atemp, y = cnt)) +geom_point(colour = "#0072B2") + ggtitle("Scatter plot of Actual Temperature") +xlab('Temperature') + ylab('Bike Count')
scat3 = ggplot(data = RentalData, aes(x = actual_windspeed, y = cnt)) +geom_point(colour = '#D55E00') + ggtitle("Scatter plot of WindSpeed") +xlab('Windspeed') + ylab('Bike Count')
scat4 = ggplot(data = RentalData, aes(x = actual_humidity, y = cnt)) +geom_point(colour ='#CC79A7') +ggtitle("Scatter plot for Humidity") + xlab("Humidity") + ylab("Bike Count")
gridExtra::grid.arrange(scat1, scat2, scat3, scat4, ncol = 2)


##########################Check for outliers in data using boxplot##########################

cnames = colnames(RentalData[,c("actual_temp","actual_atemp","actual_windspeed","actual_humidity")])

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = RentalData)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1, gn3, gn2, gn4, ncol = 2)
  
#Remove the outliers in WindSpeed
val = RentalData[,19][RentalData[,19] %in% boxplot.stats(RentalData[,19])$out]
RentalData = RentalData[which(!RentalData[,19] %in% val),]

##########################Check for Multicollinearity using VIF##########################
df = RentalData[,c("temp","atemp","hum","windspeed")]
vifcor(df)

##########################Check for collinearity using correlation graph##########################
corrgram(RentalData, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove correlated variables
RentalData <- subset(RentalData, select = -c(holiday,instant,dteday,atemp,casual,registered,actual_temp,actual_atemp,actual_windspeed,
                                             actual_humidity,actual_season,actual_yr,actual_holiday,actual_weathersit))

rmExcept(keepers = "RentalData")


########################################LINEAR REGRESSION########################################
#Train the data using linear regression

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(RentalData), 0.8*nrow(RentalData))
train = RentalData[train_index,]
test = RentalData[-train_index,]

#fit = rpart(cnt ~., data = train, method = "anova")

lr_model = lm(cnt~., data = train)

#Check the summary of the model
summary(lr_model)


#Predict the test cases
lr_predictions = predict(lr_model, test[,-10])

#Create dataframe for actual and predicted variables
df = data.frame("Actual" = test[,10], lr_predictions)
head(df)

#Calculate MAPE

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
install.packages(x)

lapply(X = x, require, character.only = TRUE)
regr.eval(trues = test[,10], preds = lr_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], lr_predictions)

#Plot a graph for actual vs predicted value
plot(test$cnt,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#Predict a sample data
prediction = predict(lr_model,test[6,])

#MAPE = 12.17%
#Accuracy = 87.83%
#MAE = 494
#RMSE = 673
#Adjusted R-squared = 0.8373
#F-Statistic = 110.2


########################################DECISION TREE########################################

#rpart for regression
dt_model = rpart(cnt ~., data = train , method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[-10])

#Create dataframe for actual and predicted data
df = data.frame("Actual" = test[10], "Predictions" = dt_predictions)

#Calculate MAPE

regr.eval(trues = test[,10], preds = dt_predictions, stats = c("mae", "mse", "rmse", "mape"))
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], dt_predictions)

prediction = predict(dt_model, test[30,])

#MAPE: 17.47%
#MAE: 684
#RMSE: 864.8
#Accuracy: 82.53%

########################################RANDOM FOREST########################################

#Train data using random forest
rf_model = randomForest(cnt ~., data = train, ntree = 500)

rf_predictions = predict(rf_model, test[, -10])

#Create dataframe for actual and predicted values
df = data.frame("Actual" = test[,10], "Predictions" = rf_predictions)

#Calculate MAPE
regr.eval(trues = test[,10], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], rf_predictions)

plot(test$cnt,type="l",lty=2,col="red")
lines(rf_predictions,col="green")


#MAPE = 10.58
#RMSE = 530
#MAE = 392
#Accuracy = 89.42
