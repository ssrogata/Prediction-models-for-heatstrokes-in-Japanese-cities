# under-sampling and bagging make prediction models

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














# 
```{r}

# CV methods
CV_method <- "cv"
CV_number <- 5
Repeated_number <- 1
Tune_number <- 1



# under-sampling parameters
Data_train_original <- data.frame(Data_train)

Parameter_grid <- expand.grid(
 	iii_Top_x_percentile = seq(0.90, 0.98, 0.01),
	iii_N_down = seq(100, 200, 100)
 	) %>% as.data.frame




Population_cut <- Data_train[AMPM == "PM" & Population100000 >= 5, ID, ] %>% unique()
Population_cut


```






# Under sampling setting 
```{r}


# using variables
Var_use <- c( 
	"Cluster_pred", 
	"ID",
	"Date",


	"Population100000", 
	"LogPopulation100000", 	
	"Mean_income_city_2015",
	"Green_area_percent",      
	"Age_median_city",
	"Population_over_65_100000",
	"SexRatioF",
	
	
	"SeverityAll",   
	

	"RainySeason", 
	"DifDateRain", 
	"holiday_flag", 
	"Hour", 
	"Month", 
	
	"precip1Hour", 
	"windSpeed_ms", 
	"DownwardSolarRadiation_kwm2", 
	"relativeHumidity", 
	
	"temperature", 
	"temperature_past1d_max_diff",
	"temperature_past1d_mean_diff", 
	"temperature_past1d_min_diff"
)






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
	
	





# set seed   
Size_n <- 36  
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








```














# Under sampling
```{r}

tic(4)

# Under sampling
HogeRMSE <- c()
HogeMAPE <- c()



for(iii in 1:nrow(Parameter_grid)){



# read classifiers
Classifier <- readRDS(
	paste("Classifier",
		"_top_",
		Parameter_grid[iii, "iii_Top_x_percentile"],
		"_percent.rds", 
		sep = ""
		)
	)



# predict classess
Data_train_cluster <- Data_train[ID %in% Population_cut, , ]
Data_train_cluster$Cluster_pred <- predict(Classifier, Data_train_cluster, pe = "raw")


Data_train_cluster_no <- Data_train[(ID %in% Population_cut) == FALSE, , ]
Data_train_cluster_no[, Cluster_pred := "99", ]


Data_train_cluster <- rbind(Data_train_cluster, Data_train_cluster_no)
Data_train_cluster <- Data_train_cluster[, Var_use, with = F]



	
	
	
	
# Under sampling b1-5
Temp_train <- c()
Temp_test <- c()


for(bbb in 1:10){
set.seed(bbb)

Data_train_cluster_auged <- sample_n(
		tbl = Data_train_cluster[Cluster_pred == "99", , ], 
		size = Parameter_grid[iii, "iii_N_down"],
		replace = F
		)
		

Data_train_cluster_auged_bag <- rbind(Data_train_cluster_auged, Data_train_cluster[Cluster_pred != "99", , ]) 
	
	### Save data
	fwrite(
		Data_train_cluster_auged_bag, 
		paste(
			"Data_train_top_", 
			Parameter_grid[iii, "iii_Top_x_percentile"], 
			"_percent_", 
			Parameter_grid[iii, "iii_N_down"],
			"_under_",
			bbb,
			".csv", sep = "")
		)	



# xgbTree
tic(6)
xgbTree_temp <- train(
	form = Form_temp,
	data = Data_train_cluster_auged_bag,
    method = "xgbTree",

	objective="reg:squarederror",
	trControl = fit_control,
	metric = "RMSE", 
	preProcess = c("center", "scale"), 
	tuneLength = 3
	#tuneGrid = tune_params
	)
summary(xgbTree_temp)
xgbTree_temp
toc(6)



# save models
saveRDS(xgbTree_temp, 
	paste(
		"xgbTree_temp_model_",
		Parameter_grid[iii, "iii_Top_x_percentile"], 
		"_percent_", 
		Parameter_grid[iii, "iii_N_down"],
		"_under_",
		bbb,
		".rds", sep = ""
		)
	)
	

	
# Store
Temp_train <- cbind(Temp_train, predict(xgbTree_temp, Data_train, pe = "raw"))
Temp_test <- cbind(Temp_test, predict(xgbTree_temp, Data_test, pe = "raw"))

		
}



Temp_train <- data.table(Temp_train)
Temp_train[, Pred := rowMeans(Temp_train), ]
Temp_train[, Pred := ifelse(Pred < 0, 0, Pred), ]




Temp_test <- data.table(Temp_test)
Temp_test[, Pred := rowMeans(Temp_test), ]
Temp_test[, Pred := ifelse(Pred < 0, 0, Pred), ]






# get performance
Data_train[, Pred := Temp_train$Pred, ]
Data_test[, Pred := Temp_test$Pred, ]



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
	paste(
	"xgbTree_temp_model_",
	Parameter_grid[iii, "iii_Top_x_percentile"], 
	"_percent_", 
	Parameter_grid[iii, "iii_N_down"],
	"_under", sep = ""
	),
	
	RMSE_train 
	, RMSE_test
	
	, Cor_train	
	, Cor_test	
	)

HogeRMSE <- rbind(HogeRMSE, Performance)	








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
	paste(
	"xgbTree_temp_model_",
	Parameter_grid[iii, "iii_Top_x_percentile"], 
	"_percent_", 
	Parameter_grid[iii, "iii_N_down"],
	"_under", sep = ""
	),
	
	MAPE_train 
	, MAPE_test
	
	, PE_train
	, PE_est
	)

HogeMAPE <- rbind(HogeMAPE, Performance)	






# get predicted values
Data_train$Predicted <- Data_train$Pred
Data_train$Obserbved <- Data_train$SeverityAll


# get predicted values
Data_test$Predicted <- Data_test$Pred
Data_test$Obserbved <- Data_test$SeverityAll




## Make figures
ggplot(data = Data_train, aes(x = Predicted, y = Obserbved)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 
	
ggsave(
	paste(
		"Out_plot_", 
		"xgbTree_temp_model_",
		Parameter_grid[iii, "iii_Top_x_percentile"], 
		"_percent_", 
		Parameter_grid[iii, "iii_N_down"],
		"_under",
		"_train.png", sep = ""), 
		width = 4.2, height = 3.2
		) #*action





### Make figures taest
ggplot(data = Data_test, aes(x = Predicted, y = Obserbved)) +
	geom_point() + 
	geom_smooth(method = lm) + 
	scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
	xlab("") + 
	ylab("") +
	theme_classic() 

ggsave(
	paste(
		"Out_plot_", 
		"xgbTree_temp_model_",
		Parameter_grid[iii, "iii_Top_x_percentile"], 
		"_percent_", 
		Parameter_grid[iii, "iii_N_down"],
		"_under",
		"_test.png", sep = ""), 
		width = 4.2, height = 3.2
		) #*action






### Make Fig train
DataSum_train <- Data_train %>%
	group_by(Date) %>%
	summarise(
		Obserbved = sum(Obserbved),
		Predicted = sum(Predicted)
		) %>%
	data.table()





DataSum_test <- Data_test %>%
	group_by(Date) %>%
	summarise(
		Obserbved = sum(Obserbved),
		Predicted = sum(Predicted)
		) %>%
	data.table()




DataSum <- rbind(DataSum_train, DataSum_test)



DataSum[, Date:=as.Date(Date), ]
DataSum[, YearUse:=year(Date), ]
DataSum <- DataSum[order(Date), , ]
DataSum[, Day:=1:length(Obserbved), by = YearUse]



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

ggsave(
	paste(
		"Out_time_24_", 
		"xgbTree_temp_model_",
		Parameter_grid[iii, "iii_Top_x_percentile"], 
		"_percent_", 
		Parameter_grid[iii, "iii_N_down"],
		"_under",
		".png", sep = ""), 
		width = 6.7 * 1.5, height = 3.5 * 0.66
		) #*action





}


fwrite(HogeRMSE, "Out_RMSE_under.csv")
fwrite(HogeMAPE, "Out_MAPE_under.csv")

toc(4)


toc(1)

```







