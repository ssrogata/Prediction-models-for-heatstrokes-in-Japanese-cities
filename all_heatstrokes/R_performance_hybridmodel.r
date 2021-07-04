# performance under-sampling

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
summary(Temp_train$Pred)


Temp_test <- data.table(Temp_test)
Temp_test[, Pred := rowMeans(Temp_test), ]
Temp_test[, Pred := ifelse(Pred < 0, 0, Pred), ]
summary(Temp_test$Pred)



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
	RMSE_train 
	, RMSE_test
	
	, Cor_train	
	, Cor_test	
	)



HogeRMSE <- data.table(Performance)
HogeRMSE

fwrite(HogeRMSE, "Out_RMSE_aug_2stage.csv")



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
	)

PE_train <- Data_train_MAPE[Spike == 1, round( (abs(sum(Pred) - sum(Obs)) / sum(Obs) ) * 100, 2), ]
	








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
	MAPE_train 
	, MAPE_test
	
	, PE_train
	, PE_est
	)


HogeMAPE <- data.table(Performance)
HogeMAPE






fwrite(HogeMAPE, "Out_MAPE_24_aug_2stage.csv")




```




















# Figure time series by 24h
```{r}
## Make figures
ggplot(data = Data_train, aes(x = Pred, y = SeverityAll)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 
ggsave(paste("Out_plot_", "2stage", "_train.png", sep = ""), width = 4.2, height = 3.2) #*action





### Make figures taest
ggplot(data = Data_test, aes(x = Pred, y = SeverityAll)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 
ggsave(paste("Out_plot_", "2stage", "_test.png", sep = ""), width = 4.2, height = 3.2) #*action











### Make Fig train
DataSum_train <- Data_train %>%
	group_by(Date) %>%
	summarise(
		SeverityAll = sum(SeverityAll),
		Pred = sum(Pred)
		) %>%
	data.table() %>%
	print()


DataSum_test <- Data_test %>%
	group_by(Date) %>%
	summarise(
		SeverityAll = sum(SeverityAll),
		Pred = sum(Pred)
		) %>%
	data.table() %>%
	print()


DataSum <- rbind(DataSum_train, DataSum_test)



DataSum[, Date:=as.Date(Date), ]
DataSum[, YearUse:=year(Date), ]
DataSum <- DataSum[order(Date), , ]
DataSum[, Day:=1:length(SeverityAll), by = YearUse]
DataSum


DataSum[Date == as.Date("2015-06-01"), , ]
DataSum[Date == as.Date("2015-07-01"), , ]
DataSum[Date == as.Date("2015-08-01"), , ]
DataSum[Date == as.Date("2015-09-01"), , ]






ggplot(data = DataSum, aes(x = Day), group=factor(YearUse)) +
	#geom_point(aes(y = SeverityAll), colour = "Black", alpha = 1.0) + 
	geom_line(aes(y = SeverityAll, x=Day), colour = "Black", size = 0.4) + 
	#geom_point(aes(y = Pred), colour = "Red", alpha = 1.0) +
	geom_line(aes(y = Pred, x=Day), colour = "Red", size = 0.4) + 
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

ggsave(paste("Out_time_24_", "2stage", ".png", sep = ""), width = 6.7 * 1.5, height = 3.5 * 0.66) #*action




HogeMAPE
HogeRMSE



Data_train[, RMSE(Obs = SeverityAll, Pred = Pred), by = ID] %>% print() %>% fwrite(., "Out_RMSE_city_hybrid_train.csv")


Data_test[, RMSE(Obs = SeverityAll, Pred = Pred), by = ID] %>% print() %>%  fwrite(., "Out_RMSE_city_hybrid_test.csv")


Data_test %>% group_by(ID) %>% summarise(Obs = sum(SeverityAll), Pred = sum(Pred)) %>% data.table() %>% print() %>%  fwrite(., "Out_heatmapdata_hybrid.csv")


fwrite(DataSum, paste("Out_data_figure2_", "hybrid", ".csv", sep = ""))


```

