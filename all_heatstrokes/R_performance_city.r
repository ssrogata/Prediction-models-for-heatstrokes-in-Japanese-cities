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



Data_obs_pred_city_specific <- fread("Out_obs_pred_cityspeficit_model.csv")
Data_obs_pred_city_specific$Pred <- ifelse(Data_obs_pred_city_specific$Pred < 0, 0, Data_obs_pred_city_specific$Pred)
Data_obs_pred_city_specific$Pred <- ifelse(Data_obs_pred_city_specific$Pred > 104, 104, Data_obs_pred_city_specific$Pred)




# Train Test
Data_train <- Data[TrainTest == "Train", , ] #*eligible 
Data_train <- data.frame(Data_train)


Data_test <- Data[TrainTest == "Test", , ] #*eligible 
Data_test <- data.frame(Data_test)



Data_obs_pred_city_specific_train <- Data_obs_pred_city_specific[TrainTest == "Train", , ]
Data_obs_pred_city_specific_test <- Data_obs_pred_city_specific[TrainTest == "Test", , ]




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
	"gamSpline_temp.rds"
)



# get model list
Model_list <- Models

# get city list
City_list <- c(
"Kobe"
, "Ashiya"
, "Nishinomiya"
, "Amagasaki"
, "Akashi" 
, "Himeji"

, "Kyoto"
, "Uji"
, "Muko"
, "Nagaokakyo"
 
, "Osaka"
, "Toyonaka"
, "Mino"
, "Ikeda"
, "Suita"
, "Sakai"             
)
# Save performance
Hoge <- c()





# loop
for(iii_city in City_list){


Data_train_city <- subset(Data_train, ID == iii_city)
Data_test_city <- subset(Data_test, ID == iii_city)

# load models
Model <- readRDS(Model_list)

# get predicted values
Data_train_city$Pred <- predict(Model, Data_train_city)
Data_train_city$Pred <- ifelse(Data_train_city$Pred < 0, 0, Data_train_city$Pred)

# get performance
RMSE_train <- RMSE(Obs = Data_train_city$SeverityAll, Pred = Data_train_city$Pred)

MAE_train <- MAE(Obs = Data_train_city$SeverityAll, Pred = Data_train_city$Pred)

Cor_train <- paste(
	cor.test(Data_train_city$SeverityAll, Data_train_city$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_train_city$SeverityAll, Data_train_city$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_train_city$SeverityAll, Data_train_city$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	)





# get predicted values
Data_test_city$Pred <- predict(Model, Data_test_city)
Data_test_city$Pred <- ifelse(Data_test_city$Pred < 0, 0, Data_test_city$Pred)

# get performance
RMSE_test <- RMSE(Obs = Data_test_city$SeverityAll, Pred = Data_test_city$Pred)

MAE_test <- MAE(Obs = Data_test_city$SeverityAll, Pred = Data_test_city$Pred)

Cor_test <- paste(
	cor.test(Data_test_city$SeverityAll, Data_test_city$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_test_city$SeverityAll, Data_test_city$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_test_city$SeverityAll, Data_test_city$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	)
	
	


# city specific model
Data_obs_pred_city_specific_train_sel <- Data_obs_pred_city_specific_train[ID == iii_city, , ]
Data_obs_pred_city_specific_test_sel <- Data_obs_pred_city_specific_test[ID == iii_city, , ]

RMSE_train_city <- RMSE(Obs = Data_obs_pred_city_specific_train_sel$SeverityAll, Pred = Data_obs_pred_city_specific_train_sel$Pred)
RMSE_test_city <- RMSE(Obs = Data_obs_pred_city_specific_test_sel$SeverityAll, Pred = Data_obs_pred_city_specific_test_sel$Pred)

Cor_train_city <- paste(
	cor.test(Data_obs_pred_city_specific_train_sel$SeverityAll, Data_obs_pred_city_specific_train_sel$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_obs_pred_city_specific_train_sel$SeverityAll, Data_obs_pred_city_specific_train_sel$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_obs_pred_city_specific_train_sel$SeverityAll, Data_obs_pred_city_specific_train_sel$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	)

Cor_test_city <- paste(
	cor.test(Data_obs_pred_city_specific_test_sel$SeverityAll, Data_obs_pred_city_specific_test_sel$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_obs_pred_city_specific_test_sel$SeverityAll, Data_obs_pred_city_specific_test_sel$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_obs_pred_city_specific_test_sel$SeverityAll, Data_obs_pred_city_specific_test_sel$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	)




# Summarize
Performance <- c(
	iii_city,
	RMSE_train
	, RMSE_train_city	
	, RMSE_test
	, RMSE_test_city		

	, Cor_train	
	, Cor_train_city	
	, Cor_test	
	, Cor_test_city	
	)

Hoge <- cbind(Hoge, Performance)	

}


Hoge <- data.table(Hoge)
Hoge$Name <- c(
	  "City"
	, "RMSE_train"
	, "RMSE_train_city"	
	, "RMSE_test"
	, "RMSE_test_city"		
	  
	, "Cor_train"	
	, "Cor_train_city"	
	, "Cor_test"
	, "Cor_test_city"	
)

fwrite(Hoge, "Out_RMSE_city_commonmodel.csv") #

Hoge




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










# Performance 
```{r}

Data <- fread("Out_obs_pred_cityspeficit_model.csv")
Data$Pred <- ifelse(Data$Pred < 0, 0, Data$Pred)
Data$Pred <- ifelse(Data$Pred > 104, 104, Data$Pred)

Data_train <- Data[TrainTest == "Train", , ]
Data_test <- Data[TrainTest == "Test", , ]



# get performance
RMSE_train <- RMSE(Obs = Data_train$SeverityAll, Pred = Data_train$Pred) %>% print()

MAE_train <- MAE(Obs = Data_train$SeverityAll, Pred = Data_train$Pred) %>% print()

Cor_train <- paste(
	cor.test(Data_train$SeverityAll, Data_train$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_train$SeverityAll, Data_train$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_train$SeverityAll, Data_train$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	) %>% print()







# get performance
RMSE_test <- RMSE(Obs = Data_test$SeverityAll, Pred = Data_test$Pred) %>% print()

MAE_test <- MAE(Obs = Data_test$SeverityAll, Pred = Data_test$Pred) %>% print()

Cor_test <- paste(
	cor.test(Data_test$SeverityAll, Data_test$Pred)$estimate %>% round(., 2),
	" (",
	cor.test(Data_test$SeverityAll, Data_test$Pred)$conf.int[1] %>% round(., 2),
	" to ",
	cor.test(Data_test$SeverityAll, Data_test$Pred)$conf.int[2] %>% round(., 2),
	")",
	sep = ""
	) %>% print()
	





# Summarize
Performance <- c(
	RMSE_train 
	, RMSE_test

	, Cor_train	
	, Cor_test	
	)
Performance

write.csv(Performance, "Out_RMSE_city_specific_model.csv")


```













# MAPE by 24
```{r}


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
	) %>% print()

PE_train <- Data_train_MAPE[Spike == 1, round( (abs(sum(Pred) - sum(Obs)) / sum(Obs) ) * 100, 2), ] %>% print()
	







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
	) %>% print()


PE_test <- Data_test_MAPE[Spike == 1, round( (abs(sum(Pred) - sum(Obs)) / sum(Obs) ) * 100, 2), ] %>% print()
	




# Summarize
Performance <- c(
	MAPE_train 
	, MAPE_test
	, PE_train
	, PE_test
	) %>% print()


write.csv(Performance, "Out_MAPE_24_city_specific_model.csv")


```

















# Figure time series by 24h
```{r}



# get predicted values
Data_train$Predicted <- Data_train$Pred
Data_train$Obserbved <- Data_train$SeverityAll
Data_train$Dif <- Data_train$Predicted - Data_train$Obserbved


# get predicted values
Data_test$Predicted <- Data_test$Pred
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
ggsave(paste("Out_plot_city_specific_train.png", sep = ""), width = 4.2, height = 3.2) #*action





### Make figures test
ggplot(data = Data_test, aes(x = Predicted, y = Obserbved)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 
ggsave(paste("Out_plot_city_specific_test.png", sep = ""), width = 4.2, height = 3.2) #*action






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

ggsave(paste("Out_time_24_city_specific.png", sep = ""), width = 6.7 * 1.5, height = 3.5 * 0.66) #*action







```




