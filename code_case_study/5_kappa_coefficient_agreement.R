# File: 5_kappa_coefficient_agreement.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script performs a Kappa coefficient analysis to assess 
#   the agreement between different definitions of Metabolic Syndrome (MetS) 
#   (Cook et al., Zimmet et al., and Ahrens et al.) in children with obesity 
#   and overweight from both Hispanic-American and Hispanic-European cohorts.
#   The Kappa coefficient provides a statistical measure of inter-rater 
#   agreement for categorical items, which in this case are the MetS 
#   classifications according to different criteria.

# Tasks performed in this script (including table names from the comments):
#   - **Load necessary packages** required for data manipulation and statistical analysis.
#   - **Define and load custom functions**, including `calculate_cole()` for classifying obesity status according to Cole et al. (2002).
#   - **Import datasets** for the Hispanic-American and Hispanic-European cohorts with MetS classifications based on different criteria.
#   - **Compute obesity status** for each dataset using the Cole criteria and filter for children with obesity.
#   - **Calculate Kappa coefficients** to assess agreement between MetS definitions in children with obesity:
#     - **Cook vs. Zimmet**
#     - **Cook vs. Ahrens**
#     - **Zimmet vs. Ahrens**
#   - **Create a descriptive table** of Kappa coefficients for children with obesity, combining results from both cohorts.
#   - **Repeat the analysis** for children with overweight, calculating Kappa coefficients and creating a descriptive table.
#   - **Generate the following tables**:
#     - **Table of Kappa coefficients in children with obesity**
#     - **Table of Kappa coefficients in children with overweight**

# Additional information:
#   - References for Kappa calculation methods and interpretations:
#     - https://rpubs.com/VINDELL2981/kappa
#     - https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/

# Input files:
#   - `./outputs/2023_10_23_iberoamerican_cook.csv`
#   - `./outputs/2023_10_23_iberoamerican_idf.csv`
#   - `./outputs/2023_10_23_iberoamerican_ahrens.csv`
#   - `./outputs/2023_10_23_spanish_cook.csv`
#   - `./outputs/2023_10_23_spanish_idf.csv`
#   - `./outputs/2023_10_23_spanish_ahrens.csv`
#   - `./reference_tables/COLE_TABLE_CUTOFFS`

# Output files:
#   - **Descriptive tables** displaying Kappa coefficients for:
#     - Children with obesity (Table of Kappa coefficients in children with obesity)
#     - Children with overweight (Table of Kappa coefficients in children with overweight)




# Load packages -----
library(magrittr)
library(dplyr)
library(vcd)
library(tibble)
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


# Additional information -----
# https://rpubs.com/VINDELL2981/kappa
# https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/

# 1) Children with obesity -----
## Hispanic-American ----
cook = read.csv2("./outputs/2023_10_23_iberoamerican_cook.csv")
idf = read.csv2("./outputs/2023_10_23_iberoamerican_idf.csv")
ahrens = read.csv2("./outputs/2023_10_23_iberoamerican_ahrens.csv")

### Compute the obesity status ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook) %>% 
  dplyr::filter(Obesity_Cole == "Obese")
idf = calculate_cole(reference_table = ref_cole, data_user = idf) %>% 
  dplyr::filter(Obesity_Cole == "Obese")
ahrens = calculate_cole(reference_table = ref_cole, data_user = ahrens) %>% 
  dplyr::filter(Obesity_Cole == "Obese")

american = cook %>% 
  dplyr::mutate(MetS_Zimmet = idf$Metabolic_Syndrome, 
                MetS_Ahrens = ahrens$Metabolic_Syndrome) %>% 
  dplyr::rename(MetS_Cook = Metabolic_Syndrome) 

### Cook - Zimmet  -----
tab = table(american$MetS_Cook, american$MetS_Zimmet)
k = Kappa(tab)
k
confint(k)
# Cook - Zimmet 
# value     ASE     z  Pr(>|z|)
# Unweighted 0.6659 0.04858 13.71 9.181e-43
# Weighted   0.6659 0.04858 13.71 9.181e-43
# Kappa             lwr       upr
# Unweighted 0.570687 0.7611175
# Weighted   0.570687 0.7611175
### Cook - Ahrens  -----
tab = table(american$MetS_Cook, american$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# value     ASE     z  Pr(>|z|)
# Unweighted 0.3077 0.05845 5.265 1.405e-07
# Weighted   0.3077 0.05845 5.265 1.405e-07
# Kappa              lwr       upr
# Unweighted 0.1931408 0.4222438
# Weighted   0.1931408 0.4222438
### Zimmet - Ahrens  -----
tab = table(american$MetS_Zimmet, american$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# Zimmet - Ahrens 
# value     ASE    z  Pr(>|z|)
# Unweighted 0.2208 0.04748 4.65 3.313e-06
# Weighted   0.2208 0.04748 4.65 3.313e-06
# Kappa              lwr       upr
# Unweighted 0.1277298 0.3138286
# Weighted   0.1277298 0.3138286

american_kappa = data.frame(Comparison = c("Cook - Zimmet", "Cook - Ahrens", "Zimmet - Ahrens"), 
                            Kappa = c(0.66, 0.31, 0.22), 
                            Confidence_interval = c("(0.57-0.76)", "(0.19-0.42)", "(0.13-0.31)"), 
                            p_value = rep("<0.001", 3))


## Hispanic-European -----

cook = read.csv2("./outputs/2023_10_23_spanish_cook.csv")
idf = read.csv2("./outputs/2023_10_23_spanish_idf.csv")
ahrens = read.csv2("./outputs/2023_10_23_spanish_ahrens.csv")

### Compute the obesity status ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook) %>% 
  dplyr::filter(Obesity_Cole == "Obese")
idf = calculate_cole(reference_table = ref_cole, data_user = idf) %>% 
  dplyr::filter(Obesity_Cole == "Obese")
ahrens = calculate_cole(reference_table = ref_cole, data_user = ahrens) %>% 
  dplyr::filter(Obesity_Cole == "Obese")


european = cook %>% 
  dplyr::mutate(MetS_Zimmet = idf$Metabolic_Syndrome, 
                MetS_Ahrens = ahrens$Metabolic_Syndrome) %>% 
  dplyr::rename(MetS_Cook = Metabolic_Syndrome)

### Cook - Zimmet  -----
tab = table(european$MetS_Cook, european$MetS_Zimmet)
k = Kappa(tab)
k
confint(k)
# Cook - IDF 
# value     ASE     z  Pr(>|z|)
# Unweighted 0.5059 0.02827 17.89 1.302e-71
# Weighted   0.5059 0.02827 17.89 1.302e-71
# Kappa              lwr       upr
# Unweighted 0.4504724 0.5612898
# Weighted   0.4504724 0.5612898
### Cook - Ahrens  -----
tab = table(european$MetS_Cook, european$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# value     ASE     z  Pr(>|z|)
# Unweighted 0.4006 0.02896 13.83 1.687e-43
# Weighted   0.4006 0.02896 13.83 1.687e-43
# Kappa             lwr       upr
# Unweighted 0.343801 0.4573397
# Weighted   0.343801 0.4573397
### Zimmet - Ahrens -----
tab = table(european$MetS_Zimmet, european$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# Zimmet - Ahrens 
# value     ASE     z  Pr(>|z|)
# Unweighted 0.1329 0.01955 6.798 1.058e-11
# Weighted   0.1329 0.01955 6.798 1.058e-11
# Kappa               lwr       upr
# Unweighted 0.09457348 0.1711929
# Weighted   0.09457348 0.1711929

european_kappa = data.frame(Comparison2 = c("Cook - Zimmet", "Cook - Ahrens", "Zimmet - Ahrens"), 
                            Kappa2 = c(0.51, 0.40, 0.13), 
                            Confidence_interval2 = c("(0.45-0.56)", "(0.34-0.46)", "(0.09-0.17)"), 
                            p_value2 = rep("<0.001", 3))


# Descriptive table of kappa coefficient in children with obesity -----
t1 = tibble(american_kappa) 
t2 = tibble(european_kappa[, 2:4])
table = bind_cols(t1, t2) %>% gt() %>%  
  tab_spanner(
    label = "Hispanic-American",
    columns = c("Kappa", "Confidence_interval", "p_value")
  )   %>% 
  tab_spanner(
    label = "Hispanic-European",
    columns = c("Kappa2", "Confidence_interval2", "p_value2")
  )   %>% 
  cols_label(
    Confidence_interval = "(95% CI)",
    Kappa2 = "Kappa",
    Confidence_interval2 = "(95% CI)",
    p_value = "p-value",
    p_value2 = "p-value"
  )
table


# 2) Children with overweight----
## Hispanic-American -----
cook = read.csv2("./outputs/2023_10_23_iberoamerican_cook.csv")
idf = read.csv2("./outputs/2023_10_23_iberoamerican_idf.csv")
ahrens = read.csv2("./outputs/2023_10_23_iberoamerican_ahrens.csv")

### Compute the obesity status ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook) %>% 
  dplyr::filter(Obesity_Cole == "Overweight")
idf = calculate_cole(reference_table = ref_cole, data_user = idf) %>% 
  dplyr::filter(Obesity_Cole == "Overweight")
ahrens = calculate_cole(reference_table = ref_cole, data_user = ahrens) %>% 
  dplyr::filter(Obesity_Cole == "Overweight")

american = cook %>% 
  dplyr::mutate(MetS_Zimmet = idf$Metabolic_Syndrome, 
                MetS_Ahrens = ahrens$Metabolic_Syndrome) %>% 
  dplyr::rename(MetS_Cook = Metabolic_Syndrome) 

### Cook - Zimmet  -----
tab = table(american$MetS_Cook, american$MetS_Zimmet)
k = Kappa(tab)
k
confint(k)
# Cook - Zimmet 
# value     ASE     z  Pr(>|z|)
# Unweighted 0.5019 0.09871 5.085 3.676e-07
# Weighted   0.5019 0.09871 5.085 3.676e-07
# Kappa              lwr       upr
# Unweighted 0.3084668 0.6953942
# Weighted   0.3084668 0.6953942
### Cook - Ahrens  -----
tab = table(american$MetS_Cook, american$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# value   ASE     z  Pr(>|z|)
# Unweighted 0.3115 0.068 4.581 4.622e-06
# Weighted   0.3115 0.068 4.581 4.622e-06
# Kappa              lwr      upr
# Unweighted 0.1782403 0.444786
# Weighted   0.1782403 0.444786
### Zimmet - Ahrens-----
tab = table(american$MetS_Zimmet, american$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# value     ASE     z Pr(>|z|)
# Unweighted 0.1184 0.04997 2.369  0.01785
# Weighted   0.1184 0.04997 2.369  0.01785
# Kappa               lwr       upr
# Unweighted 0.02043025 0.2163145
# Weighted   0.02043025 0.2163145

american_kappa = data.frame(Comparison = c("Cook - Zimmet", "Cook - Ahrens", "Zimmet - Ahrens"), 
                            Kappa = c(0.50, 0.31, 0.12), 
                            Confidence_interval = c("(0.30-0.70)", "(0.18-0.44)", "(0.02-0.22)"), 
                            p_value = c(rep("<0.001", 2), "0.017")
)


## Hispanic-Eropean -----

cook = read.csv2("./outputs/2023_10_23_spanish_cook.csv")
idf = read.csv2("./outputs/2023_10_23_spanish_idf.csv")
ahrens = read.csv2("./outputs/2023_10_23_spanish_ahrens.csv")

### Compute the obesity status ------
cook = calculate_cole(reference_table = ref_cole, data_user = cook) %>% 
  dplyr::filter(Obesity_Cole == "Overweight")
idf = calculate_cole(reference_table = ref_cole, data_user = idf) %>% 
  dplyr::filter(Obesity_Cole == "Overweight")
ahrens = calculate_cole(reference_table = ref_cole, data_user = ahrens) %>% 
  dplyr::filter(Obesity_Cole == "Overweight")


european = cook %>% 
  dplyr::mutate(MetS_Zimmet = idf$Metabolic_Syndrome, 
                MetS_Ahrens = ahrens$Metabolic_Syndrome) %>% 
  dplyr::rename(MetS_Cook = Metabolic_Syndrome)

### Cook - Zimmet  -----
tab = table(european$MetS_Cook, european$MetS_Zimmet)
k = Kappa(tab)
k
confint(k)

# value     ASE     z  Pr(>|z|)
# Unweighted 0.4863 0.07505 6.479 9.205e-11
# Weighted   0.4863 0.07505 6.479 9.205e-11
# Kappa              lwr       upr
# Unweighted 0.3391663 0.6333376
# Weighted   0.3391663 0.6333376

### Cook - Ahrens  -----
tab = table(european$MetS_Cook, european$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# value    ASE     z  Pr(>|z|)
# Unweighted 0.3351 0.0767 4.368 1.252e-05
# Weighted   0.3351 0.0767 4.368 1.252e-05
# Kappa              lwr       upr
# Unweighted 0.1847289 0.4853911
# Weighted   0.1847289 0.4853911

### Zimmet - Ahrens-----
tab = table(european$MetS_Zimmet, european$MetS_Ahrens)
k = Kappa(tab)
k
confint(k)
# value    ASE     z Pr(>|z|)
# Unweighted 0.2203 0.0704 3.129 0.001754
# Weighted   0.2203 0.0704 3.129 0.001754
# Kappa               lwr       upr
# Unweighted 0.08229868 0.3582541
# Weighted   0.08229868 0.3582541

european_kappa = data.frame(Comparison2 = c("Cook - Zimmet", "Cook - Ahrens", "Zimmet - Ahrens"), 
                            Kappa2 = c(0.49, 0.33, 0.22), 
                            Confidence_interval2 = c("(0.34-0.63)", "(0.18-0.49)", "(0.09-0.36)"), 
                            p_value2 = c(rep("<0.001", 2), "0.002"))


# Descriptive table of kappa coefficient in children with overweight -----
t1 = tibble(american_kappa) 
t2 = tibble(european_kappa[, 2:4])
table = bind_cols(t1, t2) %>% gt() %>%  
  tab_spanner(
    label = "Hispanic-American",
    columns = c("Kappa", "Confidence_interval", "p_value")
  )   %>% 
  tab_spanner(
    label = "Hispanic-European",
    columns = c("Kappa2", "Confidence_interval2", "p_value2")
  )   %>% 
  cols_label(
    Confidence_interval = "(95% CI)",
    Kappa2 = "Kappa",
    Confidence_interval2 = "(95% CI)", 
    p_value = "p-value", 
    p_value2 = "p-value"
  )
table
