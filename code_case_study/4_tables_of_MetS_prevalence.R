# File: 4_tables_of_MetS_prevalence.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script generates tables of Metabolic Syndrome (MetS) prevalence 
#   for the Hispanic-European and Hispanic-American cohorts using different 
#   diagnostic criteria and age groups. It calculates obesity status 
#   according to Cole et al. (2002) and creates prevalence tables based 
#   on definitions by Cook et al. (2003), Zimmet et al. (2007), and 
#   Ahrens et al. (2014).

# Tasks performed in this script (including table names from comments):
#   - **Table 4A**: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-European cohort, ages 3 to 11 years.
#   - **Table 5A**: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-European cohort, ages 11 to 18 years.
#   - **Table 6A**: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-European cohort, ages 3 to 18 years.
#   - **Table 4A**: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-American cohort, ages 3 to 11 years.
#   - **Table 5A**: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-American cohort, ages 11 to 18 years.
#   - **Table 6A**: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-American cohort, ages 3 to 18 years.

#   - **Statistical Comparisons**: Calculate p-values to compare the prevalence of MetS between European and American cohorts within obesity status groups.

# Input files:
#   - `./outputs/2023_10_23_spanish_cook.csv`
#   - `./outputs/2023_10_23_spanish_idf.csv`
#   - `./outputs/2023_10_23_spanish_ahrens.csv`
#   - `./outputs/2023_10_23_iberoamerican_cook.csv`
#   - `./outputs/2023_10_23_iberoamerican_idf.csv`
#   - `./outputs/2023_10_23_iberoamerican_ahrens.csv`
#   - `./reference_tables/COLE_TABLE_CUTOFFS`

# Output files:
#   - Generated tables as specified above (Tables 4, 5, 6 for both cohorts).

# Load packages -----
library(magrittr)
library(dplyr)
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

# 1) Import Hispanic-European datasets -----
cook = read.csv2("./outputs/2023_10_23_spanish_cook.csv")
idf = read.csv2("./outputs/2023_10_23_spanish_idf.csv")
ahrens = read.csv2("./outputs/2023_10_23_spanish_ahrens.csv")

## Compute the Obesity status (normoweight, overweight and obesity) ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook)
idf = calculate_cole(reference_table = ref_cole, data_user = idf)
ahrens = calculate_cole(reference_table = ref_cole, data_user = ahrens)

# 1.1) Table of MetS prevalence (3-10 years, early chidhood subset) by definitions -----
## Cook -----
tab_cook = cook %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 4A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook

## IDF -----
tab_idf = idf %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 4B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_idf

## Ahrens -----
tab_ahrens = ahrens %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 4C: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_ahrens

## Bind tables and export Table 4A ------
tab_gen_3_10 = tbl_merge(tbls = list(tab_cook, tab_idf, tab_ahrens), 
                         tab_spanner = c("**Cook (NCEP)**", "**Zimmet (IDF)**", "**Ahrens (IDEFICS, monitoring level)**")) %>% 
  modify_caption("Table 4A: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-European cohort with a range of 3 to 11 years.")
tab_gen_3_10

# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "landscape")
# tab_gen_3_10  %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "./Table5.docx", align = "center",
#                           pr_section = officer::prop_section(page_size = page_size_A3))


# 1.2) Table of MetS prevalence (11-18 years, adolescence subset) by definitions -----
## Cook -----
tab_cook = cook %>% 
  tidyr::drop_na(wc_cm, 
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 5A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook

## IDF -----
tab_idf = idf %>% 
  tidyr::drop_na(wc_cm, 
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 5B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_idf



## Bind tables and export Table 5A ------
tab_gen_11 = tbl_merge(tbls = list(tab_cook, tab_idf), 
                         tab_spanner = c("**Cook (NCEP)**", "**Zimmet (IDF)**")) %>% 
  modify_caption("Table 5A: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-European cohort with a range of 11 to 18 years.")
tab_gen_11

# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "landscape")
# tab_gen_11  %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "./Table5.docx", align = "center",
#                           pr_section = officer::prop_section(page_size = page_size_A3))

# 1.3) Table of MetS prevalence (3-18 years, whole population) by definitions -----
## Cook -----
cook1 = cook %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
cook2 = cook %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
cook = rbind(cook1, cook2)

tab_cook = cook %>% 
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 6A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook

## IDF -----

idf1 = idf %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
idf2 = idf %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
idf = rbind(idf1, idf2)

tab_idf = idf %>% 
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 6B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_idf



## Bind tables and export Table 6A ------
tab_gen_11 = tbl_merge(tbls = list(tab_cook, tab_idf), 
                       tab_spanner = c("**Cook (NCEP)**", "**Zimmet (IDF)**")) %>% 
  modify_caption("Table 6A: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-European cohort with a range of 11 to 18 years.")
tab_gen_11

# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "landscape")
# tab_gen_11  %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "./Table6.docx", align = "center",
#                           pr_section = officer::prop_section(page_size = page_size_A3))


# 2) Import Hispanic-American datasets -----
cook = read.csv2("./outputs/2023_10_23_iberoamerican_cook.csv")
idf = read.csv2("./outputs/2023_10_23_iberoamerican_idf.csv")
ahrens = read.csv2("./outputs/2023_10_23_iberoamerican_ahrens.csv")

## Compute the Obesity status (normoweight, overweight and obesity) ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook)
idf = calculate_cole(reference_table = ref_cole, data_user = idf)
ahrens = calculate_cole(reference_table = ref_cole, data_user = ahrens)

# 2.1) Table of MetS prevalence (3-10 years, early chidhood subset) by definitions -----
## Cook -----
tab_cook = cook %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 4A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook

## IDF -----
tab_idf = idf %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 4B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_idf

## Ahrens -----
tab_ahrens = ahrens %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml,
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 4C: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_ahrens

## Bind tables and export Table 4B ------
tab_gen_3_10 = tbl_merge(tbls = list(tab_cook, tab_idf, tab_ahrens), 
                         tab_spanner = c("**Cook (NCEP)**", "**Zimmet (IDF)**", "**Ahrens (IDEFICS, monitoring level)**")) %>% 
  modify_caption("Table 4A: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-American cohort with a range of 3 to 11 years.")
tab_gen_3_10

# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "landscape")
# tab_gen_3_10  %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "./Table5.docx", align = "center",
#                           pr_section = officer::prop_section(page_size = page_size_A3))


# 2.2) Table of MetS prevalence (11-18 years, adolescence subset) by definitions -----
## Cook -----
tab_cook = cook %>% 
  tidyr::drop_na(wc_cm, 
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 5A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook

## IDF -----
tab_idf = idf %>% 
  tidyr::drop_na(wc_cm, 
                 dbp_mmHg, sbp_mmHg,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>%
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 5B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_idf



## Bind tables and export Table 5B ------
tab_gen_11 = tbl_merge(tbls = list(tab_cook, tab_idf), 
                       tab_spanner = c("**Cook (NCEP)**", "**Zimmet (IDF)**")) %>% 
  modify_caption("Table 5A: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-American cohort with a range of 11 to 18 years.")
tab_gen_11

# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "landscape")
# tab_gen_11  %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "./Table5.docx", align = "center",
#                           pr_section = officer::prop_section(page_size = page_size_A3))

# 2.3) Table of MetS prevalence (3-18 years, whole population) by definitions -----
## Cook -----
cook1 = cook %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
cook2 = cook %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
cook = rbind(cook1, cook2)

tab_cook = cook %>% 
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_n()  %>% add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 6A: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_cook

## IDF -----

idf1 = idf %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
idf2 = idf %>%
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18) %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl)
idf = rbind(idf1, idf2)

tab_idf = idf %>% 
  dplyr::mutate(sex = if_else(sex == 0, "Male", "Female")) %>% 
  dplyr::select(sex, decimal_age, Obesity_Cole, Metabolic_Syndrome) %>% 
  tbl_summary(by = Metabolic_Syndrome,
              label = list(decimal_age ~ "Age (years)",
                           sex ~ "Sex",
                           Obesity_Cole ~ "Obesity status"),
              percent = "row", 
              missing = "always",
              missing_text = "Missing values",
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  # add_n()  %>% 
  add_p() %>% modify_header(label = "**Variable**") %>% 
  bold_labels() %>% 
  modify_footnote(all_stat_cols() ~ "n (%); Median (IQR)") %>% 
  modify_caption("Table 6B: Anthropometric and metabolic characteristics of the study population, according to the cohort.")
tab_idf



## Bind tables and export Table 6B ------
tab_gen_11 = tbl_merge(tbls = list(tab_cook, tab_idf), 
                       tab_spanner = c("**Cook (NCEP)**", "**Zimmet (IDF)**")) %>% 
  modify_caption("Table 6A: Prevalence of Metabolic Syndrome with selected criteria in the Hispanic-American cohort with a range of 11 to 18 years.")
tab_gen_11

# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "landscape")
# tab_gen_11  %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "./Table6.docx", align = "center",
#                           pr_section = officer::prop_section(page_size = page_size_A3))

# 3) Get the p-values for the definitive table -----

## Import all datasets of both study populations -----
cook_eu = read.csv2("./outputs/2023_10_23_spanish_cook.csv") %>% 
  dplyr::mutate(Cohort = "European")
cook_ame = read.csv2("./outputs/2023_10_23_iberoamerican_cook.csv") %>% 
  dplyr::mutate(Cohort = "American")

idf_eu = read.csv2("./outputs/2023_10_23_spanish_idf.csv") %>% 
  dplyr::mutate(Cohort = "European")
idf_ame = read.csv2("./outputs/2023_10_23_iberoamerican_idf.csv")  %>% 
  dplyr::mutate(Cohort = "American")

ahrens_eu = read.csv2("./outputs/2023_10_23_spanish_ahrens.csv") %>% 
  dplyr::mutate(Cohort = "European")
ahrens_ame = read.csv2("./outputs/2023_10_23_iberoamerican_ahrens.csv")  %>% 
  dplyr::mutate(Cohort = "American")

## Compute the Obesity status (normoweight, overweight and obesity) ------

cook_eu = calculate_cole(reference_table = ref_cole, data_user = cook_eu)
idf_eu = calculate_cole(reference_table = ref_cole, data_user = idf_eu)
ahrens_eu = calculate_cole(reference_table = ref_cole, data_user = ahrens_eu)

cook_ame = calculate_cole(reference_table = ref_cole, data_user = cook_ame)
idf_ame = calculate_cole(reference_table = ref_cole, data_user = idf_ame)
ahrens_ame = calculate_cole(reference_table = ref_cole, data_user = ahrens_ame)


### 3.1) Cook 3 a 10 años ----

eu = cook_eu %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

ame = cook_ame %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

cook = rbind(eu, ame)                        

cook %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

cook %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

cook %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()


### 3.2) IDF 3 a 10 años ----

eu = idf_eu %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

ame = idf_ame %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

idf = rbind(eu, ame)                        

idf %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

idf %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

idf %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()


### 3.3) Ahrens 3 a 10 años ----

eu = ahrens_eu %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

ame = ahrens_ame %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

ahrens = rbind(eu, ame)                        

ahrens %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

ahrens %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

ahrens %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()


### 3.2) Cook >11 años ----

eu = cook_eu %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18)

ame = cook_ame %>% 
  dplyr::filter(decimal_age >= 11)

cook = rbind(eu, ame)                        

cook %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

cook %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

cook %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()


### 3.2) IDF >11 años ----

eu = idf_eu %>% 
  tidyr::drop_na(wc_cm,  dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18)

ame = idf_ame %>% 
  dplyr::filter(decimal_age >= 11)

idf = rbind(eu, ame)                        

idf %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

idf %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

idf %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()


### 3.3) Cook 3 a 18 años ----

eu1 = cook_eu %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)
eu2 = cook_eu %>% 
  tidyr::drop_na(wc_cm,  dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18)

eu = rbind(eu1, eu2)

ame1 = cook_ame %>%  
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)
ame2 = cook_ame %>% 
  dplyr::filter(decimal_age >= 11)

ame = rbind(ame1, ame2)

cook = rbind(eu, ame)                        

cook %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

cook %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

cook %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()


### 3.3) IDF 3 a 18 años ----

eu1 = idf_eu %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)
eu2 = idf_eu %>% 
  tidyr::drop_na(wc_cm,  dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18)

eu = rbind(eu1, eu2)

ame1 = idf_ame %>%  
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)
ame2 = idf_ame %>% 
  dplyr::filter(decimal_age >= 11)

ame = rbind(ame1, ame2)

idf = rbind(eu, ame)                        

idf %>% 
  dplyr::filter(Obesity_Cole == "Normal weight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

idf %>% 
  dplyr::filter(Obesity_Cole == "Overweight") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()

idf %>% 
  dplyr::filter(Obesity_Cole == "Obese") %>% 
  dplyr::select(Cohort, Metabolic_Syndrome) %>% 
  tbl_summary(by = Cohort, 
              digits = list(all_categorical() ~ c(0, 2))) %>% 
  add_p()  %>% modify_header(label = "**Variable**") %>% 
  bold_labels()




