# File: 3_generate_descriptive_statistics.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script generates descriptive statistics for the study populations, 
#   comparing the anthropometric and metabolic characteristics between 
#   the Hispanic-American and Hispanic-European cohorts. 
#   It calculates obesity status according to Cole et al. (2002), 
#   computes BMI z-scores using WHO references, and creates tables of descriptive 
#   statistics for the whole population and 
#   subsets based on age groups (3-11 years and 12-18 years).

# Tasks performed in this script:
#   - **Load necessary packages** required for data manipulation and statistical analysis.
#   - **Define and load custom functions**, including `calculate_cole()` for classifying obesity status according to Cole et al. (2002).
#   - **Import the processed datasets** for the Hispanic-American and Hispanic-European cohorts.
#   - **Compute obesity status** for each cohort using the Cole criteria.
#   - **Prepare datasets** for BMI z-score calculation using WHO standards.
#   - **Compute BMI z-scores** using the `anthroplus` package.
#   - **Combine datasets** from both cohorts.
#   - **Generate tables** of descriptive statistics by cohort for:
#     - The entire population (ages 3-18 years).
#     - Early childhood subset (ages 3-11 years).
#     - Adolescence subset (ages 12-18 years).
#   - **Export the generated tables** for further use or inclusion in reports/manuscripts.

# Input files:
#   - `./outputs/2023_10_23_iberoamerican_cook.csv`: Processed dataset of the Hispanic-American cohort with MetS classification.
#   - `./outputs/2023_10_23_spanish_cook.csv`: Processed dataset of the Hispanic-European cohort with MetS classification.
#   - `./reference_tables/COLE_TABLE_CUTOFFS`: Reference table containing BMI cutoff values for obesity classification according to Cole et al. (2002).

# Output files:
#   - `./Table2.docx`: Table 2A - Descriptive statistics for the entire population (ages 3-18 years).
#   - `./Table3.docx`: Table 2B - Descriptive statistics for early childhood subset (ages 3-11 years).
#   - `./Table4.docx`: Table 2C - Descriptive statistics for adolescence subset (ages 12-18 years).




# Load packages -----
library(magrittr)
library(dplyr)
library(anthroplus)
library(tidyr)
library(gtsummary)
library(flextable)
library(officer)
library(gt)


# Load functions --------
# Load calculate_cole() function to classify in Obesity status according Cole et al. (2002)
calculate_cole = function(reference_table, data_user) {
  COLE_calculated <- rep(NA,nrow(data_user))
  for(i in which(data_user$sex==0)){
    if(is.na(data_user$BMI[i]) | is.na(data_user$decimal_age[i])){ 
      COLE_calculated[i] <- NA}
    else if (data_user$BMI[i] >= reference_table$Obese_Males[which.min(abs(reference_table$Edad-data_user$decimal_age[i]))]){
      COLE_calculated[i] <- 'Obese'} 
    else if(data_user$BMI[i] >= reference_table$Overweight_Males[which.min(abs(reference_table$Edad-data_user$decimal_age[i]))]){
      COLE_calculated[i] <- 'Overweight'}
    else { COLE_calculated[i] <- 'Normal weight' }
  }
  for(i in which(data_user$sex==1)){
    if(is.na(data_user$BMI[i]) | is.na(data_user$decimal_age[i])){ 
      COLE_calculated[i] <- NA} 
    else if(data_user$BMI[i] >= reference_table$Obese_Females[which.min(abs(reference_table$Edad-data_user$decimal_age[i]))]){
      COLE_calculated[i] <- 'Obese'}
    else if (data_user$BMI[i] >= reference_table$Overweight_Females[which.min(abs(reference_table$Edad-data_user$decimal_age[i]))] ){
      COLE_calculated[i] <- 'Overweight'} 
    else { COLE_calculated[i] <- 'Normal weight' }
  }
  
  data_user$Obesity_Cole <- COLE_calculated
  data_user = data_user %>% mutate(Obesity_Cole = factor(Obesity_Cole, 
                                                         levels = c("Normal weight", "Overweight", "Obese") ))
  return(data_user)
}
ref_cole = read.table('./reference_tables/COLE_TABLE_CUTOFFS',sep = '', header = TRUE)


# Import Hispanic-American and Hispanic-European datasets ------
cook = read.csv2("./outputs/2023_10_23_iberoamerican_cook.csv")
cook2 = read.csv2("./outputs/2023_10_23_spanish_cook.csv")

# Compute the Obesity status (normoweight, overweight and obesity) ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook)
cook2 = calculate_cole(reference_table = ref_cole, data_user = cook2)

# Prepare the datasets to compute the BMI z-score (WHO) ------
cook_calculation =  cook %>% 
  dplyr::select(decimal_age, sex, height_m, weight_kg) %>% 
  dplyr::mutate(sex = as.numeric(sex)) %>% 
  dplyr::mutate(age_months = decimal_age*12, 
         sex = if_else(sex == 0, 1, 2), 
         height_cm = height_m*100)

cook_calculation2 =  cook2 %>% 
  dplyr::select(decimal_age, sex, height_m, weight_kg) %>% 
  dplyr::mutate(sex = as.numeric(sex)) %>% 
  dplyr::mutate(age_months = decimal_age*12, 
         sex = if_else(sex == 0, 1, 2), 
         height_cm = height_m*100)

# Compute the BMI z-score (WHO) ------
cook_calculation = anthroplus::anthroplus_zscores(sex = cook_calculation$sex, 
                                      age_in_months = cook_calculation$age_months, 
                                      height_in_cm = cook_calculation$height_cm, 
                                      weight_in_kg = cook_calculation$weight_kg)

cook_calculation2 = anthroplus::anthroplus_zscores(sex = cook_calculation2$sex, 
                                       age_in_months = cook_calculation2$age_months, 
                                       height_in_cm = cook_calculation2$height_cm, 
                                       weight_in_kg = cook_calculation2$weight_kg)

# Bind both datasets ------
cook = cook %>% 
  dplyr::mutate(Height_zscore_who = cook_calculation$zhfa, 
        Weight_zscore_who = cook_calculation$zwfa, 
                       BMI_zscore_who = cook_calculation$zbfa)
cook2 = cook2 %>% 
  dplyr::mutate(Height_zscore_who = cook_calculation2$zhfa, 
                Weight_zscore_who = cook_calculation2$zwfa, 
                BMI_zscore_who = cook_calculation2$zbfa)

cook = cook %>% 
  dplyr::mutate(Cohort = "Hispanic-American") %>% 
  dplyr::mutate(Cohort = factor(Cohort))
cook2 = cook2 %>% 
  dplyr::mutate(Cohort = "Hispanic-European") %>% 
  dplyr::mutate(Cohort = factor(Cohort))

cook3 = rbind(cook, cook2)

# 1) Table of descriptive statistics (3-18 years, whole population) by cohort ------
tab_cook = cook3 %>% 
  tidyr::drop_na(wc_cm, #insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age <= 18) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age,  
         dbp_mmHg, sbp_mmHg, 
         tg_mg_dl, hdl_mg_dl, glucose_mg_dl, insulin_microU_ml, BMI_zscore_who, 
         HOMA_IR, Obesity_Cole,  
         Cohort) %>% 
  tbl_summary(by = Cohort,
              label = list(sex  ~ "Sex", 
                           decimal_age ~ "Age (years)",
                           dbp_mmHg ~ "DBP (mmHg)", 
                           sbp_mmHg ~ "SBP (mmHg)", 
                           tg_mg_dl ~ "TAG (mg/dL)", 
                           hdl_mg_dl ~ "HDL-C (mg/dL)", 
                           glucose_mg_dl ~ "Glucose (mg/dL)", 
                           insulin_microU_ml ~ "Insulin (µU/mL)",
                           BMI_zscore_who ~ "BMI z-score", 
                           HOMA_IR ~ "HOMA-IR", 
                           Obesity_Cole ~ "Obesity status"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values") %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 2A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook
# modify_header(label = "**Variable**", p.value = "**p-valor**") %>%
# modify_footnote(p.value ~ "Test estadístico no paramétrico: Kruskal-Wallis") %>%

## Export Table 2A -----
# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "portrait")
# tab_cook  %>% as_flex_table() %>% 
#   flextable::save_as_docx(path = "./Table2.docx", align = "center", 
#                           pr_section = officer::prop_section(page_size = page_size_A3))

# 2) Table of descriptive statistics (3-11 years, early chidhood subset) by cohort ------
tab_cook = cook3 %>% 
  tidyr::drop_na(wc_cm, #insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age,  
                dbp_mmHg, sbp_mmHg, 
                tg_mg_dl, hdl_mg_dl, glucose_mg_dl, insulin_microU_ml, BMI_zscore_who, 
                HOMA_IR, Obesity_Cole,  
                Cohort) %>% 
  tbl_summary(by = Cohort,
              label = list(sex  ~ "Sex", 
                           decimal_age ~ "Age (years)",
                           dbp_mmHg ~ "DBP (mmHg)", 
                           sbp_mmHg ~ "SBP (mmHg)", 
                           tg_mg_dl ~ "TAG (mg/dL)", 
                           hdl_mg_dl ~ "HDL-C (mg/dL)", 
                           glucose_mg_dl ~ "Glucose (mg/dL)", 
                           insulin_microU_ml ~ "Insulin (µU/mL)",
                           BMI_zscore_who ~ "BMI z-score", 
                           HOMA_IR ~ "HOMA-IR", 
                           Obesity_Cole ~ "Obesity status"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values") %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 2B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook
# modify_header(label = "**Variable**", p.value = "**p-valor**") %>%
# modify_footnote(p.value ~ "Test estadístico no paramétrico: Kruskal-Wallis") %>%

## Export Table 2B -----
# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "portrait")
# tab_cook  %>% as_flex_table() %>% 
#   flextable::save_as_docx(path = "./Table3.docx", align = "center", 
#                           pr_section = officer::prop_section(page_size = page_size_A3))


# 3) Table of descriptive statistics (12-18 years, adolescence subset) by cohort ------
tab_cook = cook3 %>% 
  tidyr::drop_na(wc_cm, #insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age,  
                dbp_mmHg, sbp_mmHg, 
                tg_mg_dl, hdl_mg_dl, glucose_mg_dl, insulin_microU_ml, BMI_zscore_who, 
                HOMA_IR, Obesity_Cole,  
                Cohort) %>% 
  tbl_summary(by = Cohort,
              label = list(sex  ~ "Sex", 
                           decimal_age ~ "Age (years)",
                           dbp_mmHg ~ "DBP (mmHg)", 
                           sbp_mmHg ~ "SBP (mmHg)", 
                           tg_mg_dl ~ "TAG (mg/dL)", 
                           hdl_mg_dl ~ "HDL-C (mg/dL)", 
                           glucose_mg_dl ~ "Glucose (mg/dL)", 
                           insulin_microU_ml ~ "Insulin (µU/mL)",
                           BMI_zscore_who ~ "BMI z-score", 
                           HOMA_IR ~ "HOMA-IR", 
                           Obesity_Cole ~ "Obesity status"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values") %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 2C: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook
# modify_header(label = "**Variable**", p.value = "**p-valor**") %>%
# modify_footnote(p.value ~ "Test estadístico no paramétrico: Kruskal-Wallis") %>%

## Export Table 2C -----
# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "portrait")
# tab_cook  %>% as_flex_table() %>% 
#   flextable::save_as_docx(path = "./Table4.docx", align = "center", 
#                           pr_section = officer::prop_section(page_size = page_size_A3))

