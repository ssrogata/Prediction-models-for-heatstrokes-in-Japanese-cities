### "GLM, GAM, and RF for Mid to higher severity"


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




# formula
Form_temp <- formula( 	
	SeverityHigherMid ~ 
	offset(LogPopulation100000) + 
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



Model_vif <- glm(
	family = "poisson",
	Form_temp,
	data = Data_train
	)
summary(Model_vif)
vif(Model_vif)





```







# GLM with WBGT only
```{r}
glm_uni <- glm(
	family = "poisson",
	SeverityHigherMid ~ 
	offset(LogPopulation100000) +  
	WBGT, 
	data = Data_train
	)
summary(glm_uni)


saveRDS(glm_uni, "glm_uni.rds")

```








# GLM
```{r}


##################################################################
###   GLM
##################################################################

modelLookup("glm")



### caretが指定する
Size_n <- 18

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
	
	
# GLM
tic()
GLM_temp <- rfe(
	form = Form_temp,
	data = Data_train,
    method = "glm",
	family = "poisson",

	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneLength = 10,

    sizes = 1:19, #ncol(Data),
    rfeControl = rfe_control
	)
summary(GLM_temp)
GLM_temp
toc()


GLM_temp$optVariables


saveRDS(GLM_temp, "GLM_temp.rds")







```












# GAM
```{r}




##################################################################
###   GAM
##################################################################

modelLookup("gamSpline")





### caretが指定する
Size_n <- 18

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
	
	
	
	
	
# gamSpline
tic()
gamSpline_temp <- rfe(
	form = Form_temp,
	data = Data_train,
    method = "gamSpline",
	family = "poisson",

	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneLength = 10,

    sizes = 1:19, #ncol(Data),
    rfeControl = rfe_control
	)
summary(gamSpline_temp)
gamSpline_temp
toc()


gamSpline_temp$optVariables

saveRDS(gamSpline_temp, "gamSpline_temp.rds")






```














# RF
```{r}



#################################################################
###
#################################################################

# formula
Form_temp <- formula( 	
	SeverityHigherMid ~ 
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
###   Random Forest
##################################################################


### caretが指定する
Size_n <- 19
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix
					 
					 


# construct rfeControl object
rfe_control = rfeControl(
	functions = rfFuncs, 
	allowParallel = TRUE,
	method = CV_method,
    number = CV_number,
	returnResamp = "final",
	seeds = Seeds_fix,
	)


# construct trainControl object for train method 
fit_control = trainControl(
	allowParallel = TRUE,
	method = CV_method,
	number = CV_number,
	seeds = Seeds_fix
	)
	
		
	
	

# RF
tic()
# get results
RF_temp <- rfe(
	form = Form_temp,
	data = Data_train,

	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"),
	tuneLength = 10,

    sizes = 1:20, #ncol(Data),
    rfeControl = rfe_control
	)
summary(RF_temp)
RF_temp
toc()

saveRDS(RF_temp, "RF_temp.rds")






```







# info
```{r}
sessionInfo()

```


