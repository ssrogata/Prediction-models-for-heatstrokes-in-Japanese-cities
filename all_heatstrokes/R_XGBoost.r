# "XGBoost for all severity"

# Temp
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
library(mgcv)
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





#################################################################
###
#################################################################
# formula
Form_temp <- formula( 	
	SeverityAll ~ 
	Population100000 + 
	Mean_income_city_2015 + 
	Green_area_percent +           
	Age_median_city + 
	Population_over_65_100000 +
	SexRatioF +
	
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





##################################################################
###   xgbTree
##################################################################

Size_n <- 20  ### set seed
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
	




# 
tune_params = expand.grid(
 	nrounds = c(seq(10, 90, 10), seq(100, 1000, 100)), 
	max_depth = seq(3, 9, 2),
	min_child_weight = seq(1, 5, 1),
		gamma = 0,
 			colsample_bytree = 0.8,
 			subsample = 0.8,
					eta = 0.1
 	) %>% as.data.frame






# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()
















Size_n <- 41  ### set seed
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



#
xgbTree_temp$bestTune
get_nrounds <- xgbTree_temp$bestTune$nrounds
get_max_depth <- xgbTree_temp$bestTune$max_depth
get_min_child_weight <- xgbTree_temp$bestTune$min_child_weight



tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = seq(0, 0.4, 0.01),
 			colsample_bytree = 0.8,
 			subsample = 0.8,
					eta = 0.1
 	) %>% as.data.frame






# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()















Size_n <- 25  ### set seed
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
	








#
xgbTree_temp$bestTune
get_gamma <- xgbTree_temp$bestTune$gamma



tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = get_gamma,
 			colsample_bytree = seq(0.6, 1, 0.1),
 			subsample = seq(0.6, 1, 0.1),
					eta = 0.1
 	) %>% as.data.frame



# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()




















Size_n <- 10  ### set seed
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
	
	





# 
xgbTree_temp$bestTune
get_colsample_bytree <- xgbTree_temp$bestTune$colsample_bytree
get_subsample <- xgbTree_temp$bestTune$subsample

tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = get_gamma,
 			colsample_bytree = get_colsample_bytree,
 			subsample = get_subsample,
					eta = seq(0.01, 0.1, 0.01)
 	) %>% as.data.frame


	

# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()













### set seed
Size_n <- 19
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
	




#
xgbTree_temp$bestTune
get_eta <- xgbTree_temp$bestTune$eta


tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = get_gamma,
 			colsample_bytree = get_colsample_bytree,
 			subsample = get_subsample,
					eta = get_eta
 	) %>% as.data.frame


	
# 
tic()
xgbTree_temp <- rfe(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params,

    sizes = 1:20, 
    rfeControl = rfe_control
	)
summary(xgbTree_temp)
xgbTree_temp
toc()



xgbTree_temp$optVariables



```
















# Temp selected variables
```{r}


# formula
Form_temp <- formula( 	
	SeverityAll ~ 
		temperature +                  
		Population100000 +             
		Population_over_65_100000 +    
		DownwardSolarRadiation_kwm2 +  
		Month +                        
		DifDateRain +                  
		relativeHumidity +             
		temperature_past1d_max_diff +  
		Green_area_percent +           
		precip1Hour +                  
		windSpeed_ms +                 
		Age_median_city +              
		temperature_past1d_mean_diff + 
		Mean_income_city_2015                      
)
    






##################################################################
###   xgbTree
##################################################################


Size_n <- 20  ### set seed
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
	




# 
tune_params = expand.grid(
 	nrounds = c(seq(10, 90, 10), seq(100, 1000, 100)), 
	max_depth = seq(3, 9, 2),
	min_child_weight = seq(1, 5, 1),
		gamma = 0,
 			colsample_bytree = 0.8,
 			subsample = 0.8,
					eta = 0.1
 	) %>% as.data.frame






# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()
















Size_n <- 41  ### set seed
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
	






# 
xgbTree_temp$bestTune
get_nrounds <- xgbTree_temp$bestTune$nrounds
get_max_depth <- xgbTree_temp$bestTune$max_depth
get_min_child_weight <- xgbTree_temp$bestTune$min_child_weight



tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = seq(0, 0.4, 0.01),
 			colsample_bytree = 0.8,
 			subsample = 0.8,
					eta = 0.1
 	) %>% as.data.frame






# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()















Size_n <- 25  ### set seed
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
	





#
xgbTree_temp$bestTune
get_gamma <- xgbTree_temp$bestTune$gamma



tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = get_gamma,
 			colsample_bytree = seq(0.6, 1, 0.1),
 			subsample = seq(0.6, 1, 0.1),
					eta = 0.1
 	) %>% as.data.frame



# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()




















Size_n <- 10  ### set seed
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
	
	


# 
xgbTree_temp$bestTune
get_colsample_bytree <- xgbTree_temp$bestTune$colsample_bytree
get_subsample <- xgbTree_temp$bestTune$subsample

tune_params = expand.grid(
 	nrounds = get_nrounds, 
	max_depth = get_max_depth,
	min_child_weight = get_min_child_weight,
		gamma = get_gamma,
 			colsample_bytree = get_colsample_bytree,
 			subsample = get_subsample,
					eta = seq(0.01, 0.1, 0.01)
 	) %>% as.data.frame


	

# xgbTree
tic()
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc()

xgbTree_temp$bestTune





saveRDS(xgbTree_temp, "xgbTree_temp.rds")




```





# info
```{r}
sessionInfo()

```


