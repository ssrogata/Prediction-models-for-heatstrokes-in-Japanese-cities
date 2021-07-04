# Prediction-models-for-heatstrokes-in-Japanese-cities
R codes (upload version) used to develop prediction models

System requirements
	Hardware requirements
		Our analyses require a standard computer with 256 GB for RAM and with Intel(R) Xeon(R) W-2175.
	Software requirements
		OS Requirements
		Microsoft Windows 10 Pro
		Our analyses are performed by R statistical software (version 4.0.3 for windows). 

Installation Guide
	Please install R (https://cran.r-project.org/bin/windows/base/old/4.0.3/) 
	Run a program named “install_pkg_R.r” on the R to install R packages.
	These may take about 5 to 10 minutes on a normal desktop computer

Demo
	Run "R_GLM_GAM_RF.r", "R_XGBoost.r", "R_performance.r" to make prediction models
	Run "R_city_specific_model.r", "R_performance_city.r" to make city-specific models
	Run "R_down_sampling_XGBoost_no_clusters_classfier.r", "R_down_sampling_XGBoost_no_clusters_bagging.r", "R_performance_hybridmodel.r" to make under-sampling and bagging models, and a hybrid model
	Run "R_SHAP_gam.r", "R_SHAP_undersamplingXGBoost.r", "R_SHAP_hybrid.r", "R_SHAP.r" to make SHAP values
	It may takes about 60 mins in for demo on a normal desktop computer. Note that Demo data for this demo are not identical to data that we actually used in the present study. 

Instructions for use
	Our analyses used standard software, R that can be freely download from the website. Thus, description in “Demo” is enough to run the software R.





