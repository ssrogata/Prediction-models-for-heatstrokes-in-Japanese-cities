# "Performance for all severity"

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










# Performance All SeverityAll in Train and Test
```{r}

Models <- c(
	"glm_uni.rds"
	, "GLM_temp.rds"
	, "gamSpline_temp.rds"
	, "RF_temp.rds"        
	, "xgbTree_temp.rds"
)


# get model list
Model_list <- Models


# Save performance
Hoge <- c()




# loop
for(iii in Model_list){

# load models
Model <- readRDS(iii)

# get predicted values
Data_train$Pred <- if(iii == "glm_uni.rds"){Data_train$Pred <- predict(Model, Data_train, type = "response")}else{Data_train$Pred <- predict(Model, Data_train)}
Data_train$Pred <- ifelse(Data_train$Pred < 0, 0, Data_train$Pred)

# get performance
RMSE_train <- RMSE(Obs = Data_train$SeverityAll, Pred = Data_train$Pred)

MAE_train <- MAE(Obs = Data_train$SeverityAll, Pred = Data_train$Pred)

Cor_train <- paste(
	cor.test(Data_train$SeverityAll, Data_train$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_train$SeverityAll, Data_train$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_train$SeverityAll, Data_train$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	)





# get predicted values
Data_test$Pred <- if(iii == "glm_uni.rds"){Data_test$Pred <- predict(Model, Data_test, type = "response")}else{Data_test$Pred <- predict(Model, Data_test)}
Data_test$Pred <- ifelse(Data_test$Pred < 0, 0, Data_test$Pred)

# get performance
RMSE_test <- RMSE(Obs = Data_test$SeverityAll, Pred = Data_test$Pred)

MAE_test <- MAE(Obs = Data_test$SeverityAll, Pred = Data_test$Pred)

Cor_test <- paste(
	cor.test(Data_test$SeverityAll, Data_test$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_test$SeverityAll, Data_test$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_test$SeverityAll, Data_test$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	)
	





# Summarize
Performance <- c(
	iii,
	RMSE_train 
	, RMSE_test
	
	, Cor_train	
	, Cor_test	
	)

Hoge <- cbind(Hoge, Performance)	

}


Hoge <- data.table(Hoge)
Hoge

fwrite(Hoge, "Out_RMSE.csv")



```











# MAPE by 24
```{r}

# get model list
Model_list <- Models

# Save performance
Hoge <- c()




# Roop
for(iii in Model_list){

# load models
Model <- readRDS(iii)




# get predicted values
Data_train$Pred <- if(iii == "glm_uni.rds"){Data_train$Pred <- predict(Model, Data_train, type = "response")}else{Data_train$Pred <- predict(Model, Data_train)}
Data_train$Pred <- ifelse(Data_train$Pred < 0, 0, Data_train$Pred)

# Organized dataset
Data_train_MAPE <- Data_train %>% 
	group_by(Date) %>%
	summarise(
		Obs = sum(SeverityAll),
		Pred = sum(Pred)
		) %>% data.table()

Data_train_MAPE[, Year := year(Date), ]

Data_train_MAPE[, Spike :=
	ifelse(Year == 2015 & Obs >= Data_train_MAPE[Year == 2015, quantile(Obs, 0.8), ], 1, 
	ifelse(Year == 2016 & Obs >= Data_train_MAPE[Year == 2016, quantile(Obs, 0.8), ], 1,
	ifelse(Year == 2017 & Obs >= Data_train_MAPE[Year == 2017, quantile(Obs, 0.8), ], 1, 0)))
	, ]
Data_train_MAPE[, table(Spike), ]
Data_train_MAPE[, table(Spike), ] / nrow(Data_train_MAPE)



# get performance
MAPE_train <- MAPE(
	Obs = Data_train_MAPE[Spike == 1, Obs, ],
	Pred = Data_train_MAPE[Spike == 1, Pred, ]
	)

PE_train <- Data_train_MAPE[Spike == 1, round( (abs(sum(Pred) - sum(Obs)) / sum(Obs) ) * 100, 2), ]
	






# get predicted values
Data_test$Pred <- if(iii == "glm_uni.rds"){Data_test$Pred <- predict(Model, Data_test, type = "response")}else{Data_test$Pred <- predict(Model, Data_test)}
Data_test$Pred <- ifelse(Data_test$Pred < 0, 0, Data_test$Pred)


# Organized dataset
Data_test_MAPE <- Data_test %>% 
	group_by(Date) %>%
	summarise(
		Obs = sum(SeverityAll),
		Pred = sum(Pred)
		) %>% data.table()

Data_test_MAPE[, Year := year(Date), ]

Data_test_MAPE[, Spike :=
	ifelse(Year == 2018 & Obs >= Data_test_MAPE[Year == 2018, quantile(Obs, 0.8), ], 1, 0)
	, ]
Data_test_MAPE[, table(Spike), ]
Data_test_MAPE[, table(Spike), ] / nrow(Data_test_MAPE)



# get performance
MAPE_test <- MAPE(
	Obs = Data_test_MAPE[Spike == 1, Obs, ],
	Pred = Data_test_MAPE[Spike == 1, Pred, ]
	)


PE_est <- Data_test_MAPE[Spike == 1, round( (abs(sum(Pred) - sum(Obs)) / sum(Obs) ) * 100, 2), ]
	




# Summarize
Performance <- c(
	iii,
	MAPE_train 
	, MAPE_test
	
	, PE_train
	, PE_est
	)

Hoge <- cbind(Hoge, Performance)	

}


Hoge <- data.table(Hoge)
Hoge


fwrite(Hoge, "Out_MAPE_24.csv")




```




















# Figure time series by 24h
```{r}

# get model list
Model_list <- Models

# Save performance
Hoge <- c()




# Roop
for(iii in Model_list){

# load models
Model <- readRDS(iii)




# get predicted values
Data_train$Pred <- if(iii == "glm_uni.rds"){Data_train$Pred <- predict(Model, Data_train, type = "response")}else{Data_train$Pred <- predict(Model, Data_train)}
Data_train$Predicted <- ifelse(Data_train$Pred < 0, 0, Data_train$Pred)

Data_train$Obserbved <- Data_train$SeverityAll
Data_train$Dif <- Data_train$Predicted - Data_train$Obserbved


# get predicted values
Data_test$Pred <- if(iii == "glm_uni.rds"){Data_test$Pred <- predict(Model, Data_test, type = "response")}else{Data_test$Pred <- predict(Model, Data_test)}
Data_test$Predicted <- ifelse(Data_test$Pred < 0, 0, Data_test$Pred)

Data_test$Obserbved <- Data_test$SeverityAll
Data_test$Dif <- Data_test$Predicted - Data_test$Obserbved




### Make figures
ggplot(data = Data_train, aes(x = Predicted, y = Obserbved)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 
ggsave(paste("Out_plot_", iii, "_train.png", sep = ""), width = 4.2, height = 3.2) #*action





### Make figures taest
ggplot(data = Data_test, aes(x = Predicted, y = Obserbved)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 
ggsave(paste("Out_plot_", iii, "_test.png", sep = ""), width = 4.2, height = 3.2) #*action











### Make Fig train
DataSum_train <- Data_train %>%
	group_by(Date) %>%
	summarise(
		Obserbved = sum(Obserbved),
		Predicted = sum(Predicted)
		) %>%
	data.table() %>%
	print()


DataSum_test <- Data_test %>%
	group_by(Date) %>%
	summarise(
		Obserbved = sum(Obserbved),
		Predicted = sum(Predicted)
		) %>%
	data.table() %>%
	print()


DataSum <- rbind(DataSum_train, DataSum_test)



DataSum[, Date:=as.Date(Date), ]
DataSum[, YearUse:=year(Date), ]
DataSum <- DataSum[order(Date), , ]
DataSum[, Day:=1:length(Obserbved), by = YearUse]
DataSum


DataSum[Date == as.Date("2015-06-01"), , ]
DataSum[Date == as.Date("2015-07-01"), , ]
DataSum[Date == as.Date("2015-08-01"), , ]
DataSum[Date == as.Date("2015-09-01"), , ]






ggplot(data = DataSum, aes(x = Day), group=factor(YearUse)) +
	geom_line(aes(y = Obserbved, x=Day), colour = "Black", size = 0.4) + 
	geom_line(aes(y = Predicted, x=Day), colour = "Red", size = 0.4) + 
	xlab("") + 
	ylab("") +

	scale_y_continuous(
		limits = c(0, 450), 
		breaks = seq(0, 450, 50)
		) + 

	scale_x_continuous(
		label = c("Jun.", "Jul.", "Aug.", "Sep."), 
		breaks = c(1, 31, 62, 93)
		) + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
	facet_grid(~YearUse) +
	theme_classic()

ggsave(paste("Out_time_24_", iii, ".png", sep = ""), width = 6.7 * 1.5, height = 3.5 * 0.66) #*action



fwrite(DataSum, paste("Out_data_figure1_", iii, ".csv", sep = ""))

}


Hoge <- data.table(Hoge)
Hoge





```

