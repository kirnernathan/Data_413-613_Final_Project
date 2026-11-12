library(tidyverse)
df <- read.csv("Data_413/final_project/breast+cancer/breast-cancer.data")
view(df) #https://archive.ics.uci.edu/dataset/14/breast+cancer 

#https://www.bcsc-research.org/index.php/datasets/rf/submit/1575#form1575
risk_factors_2013_2017<-read.csv("Data_413/final_project/bcsc_risk_factors_summarized1_092020.csv")
risk_factors_2009_2013<-read.csv("Data_413/final_project/bcsc_risk_factors_summarized3_092020.csv")
risk_factors_2005_2009<-read.csv("Data_413/final_project/bcsc_risk_factors_summarized2_092020.csv")

combined_risk_factors <- rbind(risk_factors_2013_2017, risk_factors_2009_2013, risk_factors_2005_2009)
view(combined_risk_factors)
