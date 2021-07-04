# SHAP  for all severity hybrid

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





```









# Performance All SeverityAll in Train and Test
```{r}



###
Hoge1 <- fread("SHAP_all_severity_gam.csv")
Hoge2 <- fread("SHAP_all_severity_xgboost.csv")
Hoge <- rbind(Hoge1, Hoge2)



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

"Population100000", 
"Population_over_65_100000", "Age_median_city", "SexRatioF", "Mean_income_city_2015", "Green_area_percent",

"RainySeason", "DifDateRain", "holiday_flag", "Hour", "Month"
)
           
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

ggsave("Out_SHAP_all.png", width = 10, height = 15)




SHAP_data <- SHAP_data[abs(Contribution) <= 20, , ]
fwrite(SHAP_data, "Out_data_figure3_SHAP_hybrid.csv")


