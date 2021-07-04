# SHAP  under-sampling bagging all severity"

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
#library(mgcv)
library(epitools)
library(tableone)
library(caret)
library(tictoc)
library(Boruta)

library("DALEX")
library("iBreakDown")
library("r2d3")
library("data.table")
library("dplyr")
library("ggplot2")
library("gam")


library(doParallel)
detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(27)
registerDoParallel(cl)



set.seed(1234)





######################### Input data
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









# Performance All SeverityAll in Train and Test
```{r}

# load model
Model_gam <- readRDS("gamSpline_temp.rds")


# get predicted values
Data_train$Pred_gam <- predict(Model_gam, Data_train)
summary(Data_train$Pred_gam)
Data_train$Pred_gam <- ifelse(Data_train$Pred_gam < 0, 0, Data_train$Pred_gam)
summary(Data_train$Pred_gam)


# get predicted values
Data_test$Pred_gam <- predict(Model_gam, Data_test)
summary(Data_test$Pred_gam)
Data_test$Pred_gam <- ifelse(Data_test$Pred_gam < 0, 0, Data_test$Pred_gam)
summary(Data_test$Pred_gam)








# load augued model
Data_MAPE <- fread("Out_MAPE_under.csv")



Data_MAPE <- rename(Data_MAPE,
	"Models" = "V1",
	"MAPE_train" = "V2",
	"MAPE_test" = "V3",
	"Percenterror_train" = "V4",
	"Percenterror_test" = "V5"
	)



Data_MAPE <- Data_MAPE[order(MAPE_train), , ]

Model_name <- as.character(Data_MAPE[, first(Models), ])


Temp_train <- c()
Temp_test <- c()

for(iii in 1:10){
Model_aug <- readRDS(paste(Model_name, "_", iii, ".rds", sep = ""))

# get predicted values 
Temp_train <- cbind(Temp_train, predict(Model_aug, Data_train, pe = "raw"))

# get predicted values 
Temp_test <- cbind(Temp_test, predict(Model_aug, Data_test, pe = "raw"))


}


Temp_train <- data.table(Temp_train)
Temp_train[, Pred := rowMeans(Temp_train), ]
Temp_train[, Pred := ifelse(Pred < 0, 0, Pred), ]


Temp_test <- data.table(Temp_test)
Temp_test[, Pred := rowMeans(Temp_test), ]
Temp_test[, Pred := ifelse(Pred < 0, 0, Pred), ]



#
Data_train <- data.table(Data_train)
Data_train[, Pred_aug := Temp_train$Pred, ]


Data_test <- data.table(Data_test)
Data_test[, Pred_aug := Temp_test$Pred, ]







# cutoff
Data_train[, Pred_sum_date := sum(Pred_aug), by = Date]
Data_test[, Pred_sum_date := sum(Pred_aug), by = Date]

Data_train[, Pred := 
	ifelse(Pred_sum_date < 150, Pred_gam, 
	ifelse(Pred_sum_date < 300, (Pred_gam + Pred_aug)/2, 
	Pred_aug)), ]


Data_test[, Pred := 
	ifelse(Pred_sum_date < 150, Pred_gam, 
	ifelse(Pred_sum_date < 300, (Pred_gam + Pred_aug)/2, 
	Pred_aug)), ]










###########################
###########################
###########################
Data_train <- Data_train[order(ID, Date), , ]
Data_train[, ID_SHAP := 1:nrow(Data_train), ]
Data_train <- Data_train[Pred_sum_date >= 150, , ]
Data_train <- data.frame(Data_train)


Model <- readRDS("xgbTree_temp_model_0.93_percent_200_under_1.rds")
Select_predictor <- Model$coefnames
Select_predictor




### object for SHAP
library(xgboost)



		   
explain_Model <- explain(Model,
                           data = Data_train[, Select_predictor],
                           y = Data_train$SeverityAll,
                           label = "xgboost"
						   )

Obs <- Data_train$SeverityAll
Pred <- explain_Model$y_hat


RMSE <- function(Obs, Pred){
	Dif <- Pred - Obs
	RMSE <- round(sqrt(mean(Dif**2)), 6)
	return(RMSE)
}

RMSE(Obs, Pred)










### loop

stopCluster(cl)
cl <- makePSOCKcluster(detectCores(all.tests = FALSE, logical = TRUE) -1)
registerDoParallel(cl)



tic()

Hoge <- c()

Hoge <- foreach(iii = Data_train$ID_SHAP, .combine = "rbind") %do%{

SHAP_model <- shap(explain_Model, subset(Data_train, ID_SHAP == iii), B = 5)

Kari <- SHAP_model %>% data.table()
Kari[, ID := iii, ]
Kari

}

toc()
stopCluster(cl)


fwrite(Hoge, "SHAP_all_severity_xgboost.csv")





###
SHAP_data <- Hoge %>% 
	group_by(ID, variable_name) %>%
	summarize(
		Variable_value = first(variable_value),
		Contribution = mean(contribution)
		) %>%
	data.table() %>% 
	print()



SHAP_data[, Contribution := as.numeric(Contribution), ]
SHAP_data[, Variable_value := as.numeric(Variable_value), ]


SHAP_data[, Variable_value_scale := scale(Variable_value), by = variable_name]
	
	
SHAP_data %>% 
	group_by(variable_name) %>%
	summarize(
		mean(Variable_value_scale),
		sd(Variable_value_scale),
		) 
	
	
	
### plot SHAP
Plot_data <- SHAP_data[ , , ]

ggplot(data = SHAP_data) +
    coord_flip() +
	ggforce::geom_sina(
	    aes(x = variable_name, y = Contribution, color = Variable_value_scale),
	    method = "counts",
		#maxwidth = .7,
		alpha = 3) + 
	scale_color_gradient(low = "#FFCC33", high = "#6600CC") +
	theme_bw()



```



