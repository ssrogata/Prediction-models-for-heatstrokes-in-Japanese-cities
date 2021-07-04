### Under-sampling classifier

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
cl <- makePSOCKcluster(detectCores(all.tests = FALSE, logical = TRUE) - 1)
registerDoParallel(cl)





set.seed(1234)



######################### Input data
Data <- fread("Data_demo.csv", encoding = "UTF-8" )
Data$V1 <- NULL

colnames(Data)
dim(Data)



Data[, table(TrainTest), ]

Data_train <- Data[TrainTest == "Train", , ] #*eligible 


Data_test <- Data[TrainTest == "Test", , ] #*eligible 



Noise <- function(Var, Data){
		Var + rnorm(mean = 0, sd = sd(Var)/5, n = nrow(Data))
	}


```











# Under-sampling setting
```{r}

# CV methods
CV_method <- "cv"
CV_number <- 5
Repeated_number <- 1
Tune_number <- 1



# Under-sampling parameters
Data_train_original <- data.frame(Data_train)

Parameter_grid <- expand.grid(
 	iii_Top_x_percentile = seq(0.90, 0.98, 0.01)
	) %>% as.data.frame




Population_cut <- Data_train[AMPM == "PM" & Population100000 >= 5, ID, ] %>% unique()
Population_cut


```















# Select top X% and make classifier
```{r}
tic(1)

tic(2)
for(iii in 1:nrow(Parameter_grid)){

############################################# step 1 

# Select top X% training data 
Data_train <- data.table(Data_train_original)





Top_cut <- Data_train[ID %in% Population_cut, quantile(SeverityAll, p = Parameter_grid[iii, "iii_Top_x_percentile"]), ]
Data_train_top <- Data_train[(ID %in% Population_cut) & (SeverityAll >= Top_cut), , ]


# Select low percentile
Data_train_low <- sample_n(
	tbl = Data_train[(ID %in% Population_cut) & (SeverityAll == 0), , ],     # action
	size = round(nrow(Data_train[(ID %in% Population_cut) & SeverityAll == 0, , ]) * 0.1),
	replace = F
	)
     # Data_train_low[, list(City, Date, SeverityAll), ] 







############################################# step 2  make clusters
# clustres in data with top and lowest
# data with top + lowest, the other data
Data_train_top[, Cluster := 1, ] 
Data_train_top[, table(ID, Cluster), ]


Data_train_low$Cluster <- as.character(99) # action 
Data_train_low[, table(ID, Cluster), ]


Data_train_top_lowest <- rbind(Data_train_top, Data_train_low)  # action 




############################################# step 3 xgboost for prediction clusters
### set seed

Size_n <- 36  ### set seed   2 = 16, 3 = 36  5 = 100
Seeds_fix <- vector(mode = "list", length = CV_number * Repeated_number + 1)
for(i in 1:(CV_number * Repeated_number)) Seeds_fix[[i]] <- sample.int(n = 10000, size = Size_n )
Seeds_fix[[(CV_number * Repeated_number + 1)]] <- sample.int(10000, 1)
Seeds_fix
					 
					 


# construct trainControl object for train method 
fit_control = trainControl(
	allowParallel = TRUE,
	method = CV_method,
	number = CV_number,
	seeds = Seeds_fix#,
	#selectionFunction = "oneSE"
	)





# xgboost model for classifier
 tic(3)

 Classifier <- train(
    Cluster ~
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
		temperature_past1d_min_diff, 
		
    data = Data_train_top_lowest,
	method = "xgbTree",   # action
	trControl = fit_control,
	metric = "Accuracy",    # action
	preProcess = c("center", "scale"),
	tuneLength = 3   # action
      )
summary(Classifier)
Classifier



caret::confusionMatrix(data = predict(Classifier, Data_train_top_lowest) %>% as.factor(), reference = Data_train_top_lowest$Cluster %>% as.factor)



saveRDS(Classifier, 
	paste("Classifier_top_",
	Parameter_grid[iii, "iii_Top_x_percentile"],
	"_percent.rds", 
	sep = "")
	)

toc(3)

}

toc(2)


```






	

















