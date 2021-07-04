# Heat map data for severity Mid to higher severity"

# Setting
```{r}

# reset
rm(list=ls(all=TRUE))



# library
library(data.table)
library(dplyr)
library(psych)
library(caret)
library(gam)
library(RSNNS)
library(e1071)
library(ranger)
library(randomForest)
library(xgboost)



# Directory
setwd(".")




# Get data
Data <- fread("Data_demo.csv", encoding = "UTF-8" )
Data$V1 <- NULL



# Train Test
Data_train <- Data[TrainTest == "Train", , ] #*eligible 
Data_train <- data.frame(Data_train)


Data_test <- Data[TrainTest == "Test", , ] #*eligible 
Data_test <- data.frame(Data_test)



```








# define functions
```{r}

RMSE <- function(Obs, Pred){
	Dif <- Pred - Obs
	RMSE <- round(sqrt(mean(Dif**2)), 2)
	return(RMSE)
}



MAE <- function(Obs, Pred){
	Dif <- Pred - Obs
	MAE <- Dif %>% abs() %>% mean()	%>% round(., 2)
	return(MAE)
}



MAPE <- function(Obs, Pred){
	Dif <- Pred - Obs
	MAPE <- mean(abs(Dif/Obs)*100) %>% round(., 2)
	return(MAPE)
}


```








# data for heatmap
```{r}

# load models
Model <- readRDS("gamSpline_temp.rds")


# get predicted values
Data_train$Pred <- predict(Model, Data_train)
Data_train$Pred <- ifelse(Data_train$Pred < 0, 0, Data_train$Pred)



# get predicted values
Data_test$Pred <- predict(Model, Data_test)
Data_test$Pred <- ifelse(Data_test$Pred < 0, 0, Data_test$Pred)





########
Data_test %>%
group_by(ID) %>% 
summarise(Obs = sum(SeverityHigherMid), Pred = sum(Pred)) %>%
data.table()%>%
print() %>%
fwrite(., "Out_heatmap_sever.csv")




```

