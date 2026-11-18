library(tidyverse)

breast_cancer_data_1988 <- read.csv("Data_413/final_project/breast+cancer/breast-cancer.data")
head(breast_cancer_data_1988) #https://archive.ics.uci.edu/dataset/14/breast+cancer 

#https://www.bcsc-research.org/index.php/datasets/rf/submit/1575#form1575
risk_factors_2013_2017<-read.csv("Data_413/final_project/bcsc_risk_factors_summarized1_092020.csv")
risk_factors_2009_2013<-read.csv("Data_413/final_project/bcsc_risk_factors_summarized3_092020.csv")
risk_factors_2005_2009<-read.csv("Data_413/final_project/bcsc_risk_factors_summarized2_092020.csv")

combined_risk_factors <- rbind(risk_factors_2013_2017, risk_factors_2009_2013, risk_factors_2005_2009)
head(combined_risk_factors)

combined_risk_factors_categorical <- combined_risk_factors %>%
  mutate(age_group = case_when(
    age_group_5_years == "1" ~ "18-29",
    age_group_5_years == "2" ~ "30-34",
    age_group_5_years == "3" ~ "35-39",
    age_group_5_years == "4" ~ "40-44",
    age_group_5_years == "5" ~ "45-49",
    age_group_5_years == "6" ~ "50-54",
    age_group_5_years == "7" ~ "55-59",
    age_group_5_years == "8" ~ "60-64",
    age_group_5_years == "9" ~ "65-69",
    age_group_5_years == "10" ~ "70-74",
    age_group_5_years == "11" ~ "75-79",
    age_group_5_years == "12" ~ "80-84",
    age_group_5_years == "13" ~ "85+"
      ),
    race_ethnicity=case_when(
      race_eth == "1" ~ "Non-Hispanic white",
      race_eth == "2" ~ "Non-Hispanic black",
      race_eth == "3" ~ "Asian/Pacific Islander",
      race_eth == "4" ~ "Native American",
      race_eth == "5" ~ "Hispanic",
      race_eth == "6" ~ "Other/mixed",
      race_eth == "9" ~ "Unknown"
    ),
    first_degree_history=case_when(
      first_degree_hx == "0" ~ "No",
      first_degree_hx == "1" ~ "Yes",
      first_degree_hx == "9" ~ "Unknown"
    ),
    age_at_menarche=case_when(
      age_menarche == "0" ~ ">13",
      age_menarche == "1" ~ "12-13",
      age_menarche == "2" ~ "<12",
      age_menarche == "9" ~ "Unknown",
      
    ),
    age_at_first_birth=case_when(
      age_first_birth == "0" ~ "<20",
      age_first_birth == "1" ~ "20-24",
      age_first_birth == "2" ~ "25-29",
      age_first_birth == "3" ~ ">29",
      age_first_birth == "4" ~ "Nulliparous",
      age_first_birth == "9" ~ "Unknown",
      
    ),
    BI_RADS_breast_density=case_when(
      BIRADS_breast_density == "1" ~ "Almost entirely fat",
      BIRADS_breast_density == "2" ~ "Scattered fibroglandular densities",
      BIRADS_breast_density == "3" ~ "Heterogeneously dense",
      BIRADS_breast_density == "4" ~ "Extremely dense",
      BIRADS_breast_density == "9" ~ "Unknown or different measurement system"
      
    ),
    use_of_hormone_therapy=case_when(
      current_hrt == "0" ~ "No",
      current_hrt == "1" ~ "Yes",
      current_hrt == "9" ~ "Unknown"
    ),
    menopaus_status=case_when(
      menopaus == "1" ~ "Pre or peri-menopausal",
      menopaus == "2" ~ "Post-menopausal",
      menopaus == "3" ~ "Surgical menopause",
      menopaus == "9" ~ "Unknown"
    ), 
    bmi_range=case_when(
      bmi_group == "1" ~ "10-24.99",
      bmi_group == "2" ~ "25-29.99",
      bmi_group == "3" ~ "30-34.99",
      bmi_group == "4" ~ "35 or more",
      bmi_group == "9" ~ "Unknown"
    ),
    previous_biopsy_or_aspiration=case_when(
      biophx == "0" ~ "No",
      biophx == "1" ~ "Yes",
      biophx == "9" ~ "Unknown"
    ),
    history_of_breast_cancer=case_when(
      breast_cancer_history == "0" ~ "No",
      breast_cancer_history == "1" ~ "Yes",
      breast_cancer_history == "9" ~ "Unknown"
    )
    )%>%
  select(age_group,race_ethnicity,first_degree_history, age_at_menarche,
         age_at_first_birth,BI_RADS_breast_density,use_of_hormone_therapy,
         menopaus_status, bmi_range, previous_biopsy_or_aspiration, 
         history_of_breast_cancer
  )

head(combined_risk_factors_categorical)
