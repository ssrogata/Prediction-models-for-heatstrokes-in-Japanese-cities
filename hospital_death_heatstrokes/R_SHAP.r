# "SHAP  for Mid to higher severity"

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

library("DALEX")
library("iBreakDown")
library("r2d3")
library("data.table")
library("dplyr")
library("ggplot2")


library(doParallel)
detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(27)
registerDoParallel(cl)



set.seed(1234)





######################### Input data
Data <- fread("Data_demo.csv", encoding = "UTF-8" )
Data$V1 <- NULL



Data_train <- Data[TrainTest == "Train", , ] #*eligible 
Data_train <- data.frame(Data_train)



Data_test <- Data[TrainTest == "Test", , ] #*eligible 
#Data_test <- data.frame(Data_test)









###########################
###########################
###########################

Model <- readRDS("gamSpline_temp.rds")
Model[5]
Model[6]


Select_predictor <- Model[6]
Select_predictor <- c("LogPopulation100000", Select_predictor$optVariables)
Select_predictor




### object for SHAP
library(gam)
		   
explain_Model <- explain(Model,
                           data = Data_train[, Select_predictor],
                           y = Data_train$SeverityHigherMid,
                           label = "gam"
						   )

Obs <- Data_train$SeverityHigherMid
Pred <- explain_Model$y_hat


RMSE <- function(Obs, Pred){
	Dif <- Pred - Obs
	RMSE <- round(sqrt(mean(Dif**2)), 6)
	return(RMSE)
}

RMSE(Obs, Pred)










### loop

stopCluster(cl)
cl <- makePSOCKcluster(27)
registerDoParallel(cl)



tic()

Hoge <- c()

Select_row <- sample(1:nrow(Data_train), size = 3000)

Hoge <- foreach(iii = Select_row, .combine = "rbind") %do%{

SHAP_model <- shap(explain_Model, Data_train[iii, ], B = 5)

Kari <- SHAP_model %>% data.table()
Kari[, ID := iii, ]
Kari

}

toc()
stopCluster(cl)


fwrite(Hoge, "SHAP_all.csv")






###
Hoge <- fread("SHAP_all.csv")


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
Var <- c(
"temperature", "temperature_past1d_max_diff", "temperature_past1d_mean_diff", "temperature_past1d_min_diff", "DownwardSolarRadiation_kwm2", "relativeHumidity", "precip1Hour", "windSpeed_ms",  

"Population_over_65_100000", "Age_median_city", "SexRatioF", "Mean_income_city_2015", "Green_area_percent",

"RainySeason", "DifDateRain", "holiday_flag", "Hour", "Month"
)
                                         
#"LogPopulation100000"                                         
                           
SHAP_data[Variable_value_scale > 5, , ]

SHAP_data[Variable_value_scale < -5, , ]

SHAP_data[abs(Contribution) > 20, , ]

           


SHAP_data[, Variable_value_scale := ifelse(Variable_value_scale > 5, 5, Variable_value_scale), ]


ggplot(data = SHAP_data) +
    coord_flip() +
	ggforce::geom_sina(
	    aes(x = variable_name, y = Contribution, color = Variable_value_scale),
	    method = "counts", size = 3,
		#maxwidth = .7,
		alpha = 3) + 
	#scale_color_gradient(low = "lightblue", high = "red") +
	scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 5)) + 
	scale_x_discrete(limits = rev(Var)) + 
	scale_color_gradient(low = "#FFCC33", high = "#6600CC") +
	theme_bw()

ggsave("Out_SHAP_sever.png", width = 10, height = 15)






SHAP_data <- SHAP_data[abs(Contribution) <= 20, , ]

fwrite(SHAP_data, "Out_data_figure5_SHAP_gam.csv")





