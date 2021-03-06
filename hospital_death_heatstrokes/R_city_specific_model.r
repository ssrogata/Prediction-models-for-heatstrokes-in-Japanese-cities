# "City-specific GAM for Mid to higher severity"

# setting
```{r}



########################## Data setting
### Setting
rm(list=ls(all=TRUE))

library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(psych)
library(pROC)
library(epitools)
library(tableone)
library(caret)
library(tictoc)
library(Boruta)

library(doParallel)
detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(27)
registerDoParallel(cl)



setwd(".")
set.seed(1234)



######################### Input data
Data <- fread("Data_demo.csv", encoding = "UTF-8" )
Data$V1 <- NULL

colnames(Data)
dim(Data)



Data[, table(TrainTest), ]



Data_train <- Data[TrainTest == "Train", , ] #*eligible 
Data_train <- data.frame(Data_train)



Data_test <- Data[TrainTest == "Test", , ] #*eligible 





######################## Prediction settings
# CV methods
CV_method <- "cv"
CV_number <- 5
Repeated_number <- 1
Tune_number <- 1





# formula
Form_temp <- formula( 	
	SeverityHigherMid ~ 
	offset(LogPopulation100000) + 
	#Mean_income_city_2015 + 
	#Green_area_percent +           
	#Age_median_city + 
	#Population_over_65_100000 +
	#SexRatioF +
	
	RainySeason + 
	DifDateRain + 
	holiday_flag + 
	Hour + 
	Month + 
	
	precip1Hour + 
	windSpeed_ms + 
	DownwardSolarRadiation_kwm2 + 
	relativeHumidity + 
	temperature + 
	temperature_past1d_max_diff +
	temperature_past1d_mean_diff + 
	temperature_past1d_min_diff 
)







### set seed
Size_n <- 13

Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )

Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix
					 
					 


# construct rfeControl object
rfe_control = rfeControl(
	functions = caretFuncs, 
	allowParallel = TRUE,
	method = CV_method,
    number = CV_number,
	returnResamp = "final",
	seeds = Seeds_fix
	)


# construct trainControl object for train method 
fit_control = trainControl(
	allowParallel = TRUE,
	method = CV_method,
	number = CV_number,
	seeds = Seeds_fix
	)
	


modelLookup("gamSpline")



# All city model for selecting feature variables and hyperparameter
gamSpline_city <- rfe(
	form = Form_temp, 
	data = Data_train,
    method = "gamSpline",
	family = "poisson",

	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneLength = 10,

    sizes = 1:14, #ncol(Data),
    rfeControl = rfe_control
	)
summary(gamSpline_city)
gamSpline_city

gamSpline_city$optVariables
gamSpline_city$fit

saveRDS(gamSpline_city, paste("gamSpline_city_for_selecting_parameters_features.rds", sep = ""))




```







# city specific
```{r}



### City specific
Form_temp <- formula(
	SeverityHigherMid ~ 
	offset(LogPopulation100000) + 
	Hour +
	RainySeason +                  
	Month +
	relativeHumidity +             
	windSpeed_ms +
	temperature_past1d_min_diff +  
	temperature_past1d_max_diff +
	precip1Hour +                  
	temperature +
	temperature_past1d_mean_diff + 
	holiday_flag
	)	





tune_params = expand.grid(
 	df = as.numeric(gamSpline_city$fit$bestTune)
)

tune_params




### set seed
Size_n <- 10

Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )

Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix
					 
					 


# construct trainControl object for train method 
fit_control = trainControl(
	allowParallel = TRUE,
	method = CV_method,
	number = CV_number,
	seeds = Seeds_fix
	)
	



HogeResults <- c()
HogeData <- c()


tic()
for(iii in Data$ID %>% unique() ){

# data
Data_train <- data.table(Data_train)
Data_train_city <- Data_train[ID == iii, , ] %>% data.frame()


Data_test <- data.table(Data_test)
Data_test_city <- Data_test[ID == iii, , ] %>% data.frame()




# city specific models
gamSpline_city <- train(
	form = Form_temp, 
	data = Data_train_city,
    method = "gamSpline",
	family = "poisson",

	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneLength = 10
	)


# save results
saveRDS(gamSpline_city, paste("gamSpline_city_", iii, ".rds", sep = ""))
	
HogeResults <- list(HogeResults, Model = iii, Results = gamSpline_city)


# make predictions
Data_train_city$Pred <- predict(gamSpline_city, Data_train_city)

Data_test_city$Pred <- predict(gamSpline_city, Data_test_city)



# make obs and pred dataset
Data_train_city <- data.table(Data_train_city)
Data_test_city <- data.table(Data_test_city)


HogeData <- rbind(HogeData, Data_train_city[, list(TrainTest, ID, Date, AMPM, SeverityHigherMid, Pred), ])
HogeData <- rbind(HogeData, Data_test_city[, list(TrainTest, ID, Date, AMPM, SeverityHigherMid, Pred), ])

}


HogeResults
HogeData[, Pred := ifelse(Pred >= 10000, 10000, Pred), ]
HogeData %>% fwrite("Out_obs_pred_cityspeficit_model.csv")





toc()


```









# info
```{r}
sessionInfo()

```















