# File: 6_barplots_MetS_components.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script prepares data to visualize the altered components of 
#   Metabolic Syndrome (MetS) in children with obesity across three 
#   different age groups using bar plots. It processes datasets 
#   from both Hispanic-American and Hispanic-European cohorts, 
#   calculates obesity status according to Cole et al. (2002), 
#   and generates bar plots showing the percentage of altered 
#   MetS components in children with obesity for the following age groups:
#     - Early childhood subset (ages 3-10 years)
#     - Adolescence subset (ages 11-18 years)
#     - Whole population (ages 3-18 years)

# Tasks performed in this script:
#   - **Load necessary packages** required for data manipulation and visualization.
#   - **Define custom functions**:
#     - `calculate_cole()`: Classifies obesity status according to Cole et al. (2002).
#     - `factor_to_numeric()`: Converts factor variables to numeric.
#   - **Set up color palettes** for plotting.
#   - **Import datasets** for the Hispanic-American and Hispanic-European cohorts with MetS classifications based on different criteria (Cook et al., Zimmet et al., and Ahrens et al.).
#   - **Compute obesity status** for each dataset using the Cole criteria and select only children with obesity.
#   - **Prepare data** for each age group and cohort by:
#     - Filtering datasets based on age.
#     - Converting MetS component variables to numeric.
#     - Calculating the percentage of altered MetS components.
#   - **Generate bar plots** to visualize the percentage of altered MetS components in children with obesity for each age group and cohort:
#     - **Figure 2A**: Early childhood subset (ages 3-10 years).
#     - **Figure 2B**: Adolescence subset (ages 11-18 years).
#     - **Figure 2C**: Whole population (ages 3-18 years).
#   - **Export figures** by saving the plots as PDF files with specified settings.

# Input files:
#   - `./outputs/2023_10_23_iberoamerican_cook.csv`
#   - `./outputs/2023_10_23_iberoamerican_idf.csv`
#   - `./outputs/2023_10_23_iberoamerican_ahrens.csv`
#   - `./outputs/2023_10_23_spanish_cook.csv`
#   - `./outputs/2023_10_23_spanish_idf.csv`
#   - `./outputs/2023_10_23_spanish_ahrens.csv`
#   - `./reference_tables/COLE_TABLE_CUTOFFS`

# Output:
#   - **Figure 2A**: Bar plot showing the percentage of altered MetS components in children with obesity (early childhood subset, ages 3-10 years) for both cohorts.
#   - **Figure 2B**: Bar plot showing the percentage of altered MetS components in adolescents with obesity (adolescence subset, ages 11-18 years) for both cohorts.
#   - **Figure 2C**: Bar plot showing the percentage of altered MetS components in children and adolescents with obesity (whole population, ages 3-18 years) for both cohorts.

# Note:
#   - The figures are exported using the Plot panel in RStudio by selecting Export > Save as PDF, setting the PDF size to A4 with landscape orientation, and choosing the `cairo_pdf` device in the options.



# Load packages -----
library(magrittr)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(ggplot2)


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

# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
factor_to_numeric = function(var){
  var2 = as.character(var)
  var2 = as.numeric(var2)
  return(var2)
}


# Palette colors -----
show_col(hue_pal()(16))

par(mar=c(3,4,2,2))
display.brewer.all()
brewer.pal(n = 3, "Set2")
# [1] "#66C2A5" "#FC8D62" "#8DA0CB"
display.brewer.pal(n = 3, name = "Set2")

# Import Hispanic-European and Hispanic-American datasets  -----

ame_cook = read.csv2("./outputs/2023_10_23_iberoamerican_cook.csv")
ame_idf = read.csv2("./outputs/2023_10_23_iberoamerican_idf.csv")
ame_ahrens = read.csv2("./outputs/2023_10_23_iberoamerican_ahrens.csv")

euro_cook = read.csv2("./outputs/2023_10_23_spanish_cook.csv")
euro_idf = read.csv2("./outputs/2023_10_23_spanish_idf.csv")
euro_ahrens = read.csv2("./outputs/2023_10_23_spanish_ahrens.csv")

# Compute obesity status and select only the children with obesity  -----

ame_cook = calculate_cole(reference_table = ref_cole, data_user = ame_cook)
ame_idf = calculate_cole(reference_table = ref_cole, data_user = ame_idf)
ame_ahrens = calculate_cole(reference_table = ref_cole, data_user = ame_ahrens)

euro_cook = calculate_cole(reference_table = ref_cole, data_user = euro_cook)
euro_idf = calculate_cole(reference_table = ref_cole, data_user = euro_idf)
euro_ahrens = calculate_cole(reference_table = ref_cole, data_user = euro_ahrens)

ame_cook = ame_cook %>% 
  dplyr::filter(Obesity_Cole == "Obese")
ame_idf = ame_idf %>% 
  dplyr::filter(Obesity_Cole == "Obese")
ame_ahrens = ame_ahrens %>% 
  dplyr::filter(Obesity_Cole == "Obese")

euro_cook = euro_cook %>% 
  dplyr::filter(Obesity_Cole == "Obese")
euro_idf = euro_idf %>% 
  dplyr::filter(Obesity_Cole == "Obese")
euro_ahrens = euro_ahrens %>% 
  dplyr::filter(Obesity_Cole == "Obese")

# Children with obesity (Cook, Zimmet and Ahrens, 3-10 years) -----
## 1) Preparing the data of Hispanic-American -----


ame_cook2 = ame_cook %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) 

ame_idf2 = ame_idf %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) 

ame_ahrens2 = ame_ahrens %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11) 

componentes = colnames(ame_cook2)[20:24]

ame_cook2[, componentes] = lapply(ame_cook2[, componentes], 
                                 FUN = factor, levels = c("Normal", "Altered"), 
                                 labels = c("0", "1"))

ame_idf2[, componentes] = lapply(ame_idf2[, componentes], 
                                FUN = factor, levels = c("Normal", "Altered"), 
                                labels = c("0", "1"))

componentes = colnames(ame_ahrens2)[20:25]

ame_ahrens2[, componentes] = lapply(ame_ahrens2[, componentes], 
                                   FUN = factor, levels = c("Normal", "Altered"), 
                                   labels = c("0", "1"))



componentes = colnames(ame_cook2)[20:24]

ame_cook2[, componentes] = lapply(ame_cook2[, componentes], 
                                 FUN = factor_to_numeric)

ame_idf2[, componentes] = lapply(ame_idf2[, componentes], 
                                FUN = factor_to_numeric)

componentes = colnames(ame_ahrens2)[20:25]

ame_ahrens2[, componentes] = lapply(ame_ahrens2[, componentes], 
                                   FUN = factor_to_numeric)

cook = data.frame(Abdominal_obesity = sum(ame_cook2$Obesity, na.rm = TRUE), 
                  Blood_pressure = sum(ame_cook2$Blood_pressure, na.rm = TRUE), 
                  Triacylglycerols = sum(ame_cook2$Tryglicerides, na.rm = TRUE), 
                  HDL_C = sum(ame_cook2$HDL, na.rm = TRUE), 
                  Glucose = sum(ame_cook2$Glucose_homeostasis, na.rm = TRUE), 
                  Insulin_resistance = NA, 
                  Criterion = "Cook (NCEP)"
)

idf = data.frame(Abdominal_obesity = sum(ame_idf2$Obesity, na.rm = TRUE), 
                 Blood_pressure = sum(ame_idf2$Blood_pressure, na.rm = TRUE), 
                 Triacylglycerols = sum(ame_idf2$Tryglicerides, na.rm = TRUE), 
                 HDL_C = sum(ame_idf2$HDL, na.rm = TRUE), 
                 Glucose = sum(ame_idf2$Glucose_homeostasis, na.rm = TRUE), 
                 Insulin_resistance = NA, 
                 Criterion = "Zimmet (IDF)"
)

ahrens  = data.frame(Abdominal_obesity = sum(ame_ahrens2 $Obesity, na.rm = TRUE), 
                     Blood_pressure = sum(ame_ahrens2 $Blood_pressure, na.rm = TRUE), 
                     Triacylglycerols = sum(ame_ahrens2 $Tryglicerides, na.rm = TRUE), 
                     HDL_C = sum(ame_ahrens2 $HDL, na.rm = TRUE), 
                     Glucose = sum(ame_ahrens2 $Glucose_homeostasis, na.rm = TRUE), 
                     Insulin_resistance = sum(ame_ahrens2 $Insulin_resistance, na.rm = TRUE), 
                     Criterion = "Ahrens (IDEFICS, monitoring level)"
)

n = nrow(ame_cook2)

cook = cook %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
         variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

idf = idf %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
         variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

ahrens = ahrens %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
         variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))


american = rbind(cook, idf, ahrens) %>% 
  dplyr::mutate(Cohort = "American (N = 163)")

## 2) Preparing the data of Hispanic-European -----

euro_cook2 = euro_cook %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m)%>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

euro_idf2 = euro_idf %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)

euro_ahrens2 = euro_ahrens %>% 
  tidyr::drop_na(wc_cm, insulin_microU_ml, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl, height_m) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age < 11)


componentes = colnames(euro_cook2)[20:24]

euro_cook2[, componentes] = lapply(euro_cook2[, componentes], 
                                  FUN = factor, levels = c("Normal", "Altered"), 
                                  labels = c("0", "1"))

euro_idf2[, componentes] = lapply(euro_idf2[, componentes], 
                                 FUN = factor, levels = c("Normal", "Altered"), 
                                 labels = c("0", "1"))

componentes = colnames(euro_ahrens2)[20:25]

euro_ahrens2[, componentes] = lapply(euro_ahrens2[, componentes], 
                                    FUN = factor, levels = c("Normal", "Altered"), 
                                    labels = c("0", "1"))



componentes = colnames(euro_cook2)[20:24]

euro_cook2[, componentes] = lapply(euro_cook2[, componentes], 
                                  FUN = factor_to_numeric)

euro_idf2[, componentes] = lapply(euro_idf2[, componentes], 
                                 FUN = factor_to_numeric)

componentes = colnames(euro_ahrens2)[20:25]

euro_ahrens2[, componentes] = lapply(euro_ahrens2[, componentes], 
                                    FUN = factor_to_numeric)

cook = data.frame(Abdominal_obesity = sum(euro_cook2$Obesity, na.rm = TRUE), 
                  Blood_pressure = sum(euro_cook2$Blood_pressure, na.rm = TRUE), 
                  Triacylglycerols = sum(euro_cook2$Tryglicerides, na.rm = TRUE), 
                  HDL_C = sum(euro_cook2$HDL, na.rm = TRUE), 
                  Glucose = sum(euro_cook2$Glucose_homeostasis, na.rm = TRUE), 
                  Insulin_resistance = NA, 
                  Criterion = "Cook (NCEP)"
)

idf = data.frame(Abdominal_obesity = sum(euro_idf2$Obesity, na.rm = TRUE), 
                 Blood_pressure = sum(euro_idf2$Blood_pressure, na.rm = TRUE), 
                 Triacylglycerols = sum(euro_idf2$Tryglicerides, na.rm = TRUE), 
                 HDL_C = sum(euro_idf2$HDL, na.rm = TRUE), 
                 Glucose = sum(euro_idf2$Glucose_homeostasis, na.rm = TRUE), 
                 Insulin_resistance = NA, 
                 Criterion = "Zimmet (IDF)"
)

ahrens  = data.frame(Abdominal_obesity = sum(euro_ahrens2$Obesity, na.rm = TRUE), 
                     Blood_pressure = sum(euro_ahrens2$Blood_pressure, na.rm = TRUE), 
                     Triacylglycerols = sum(euro_ahrens2$Tryglicerides, na.rm = TRUE), 
                     HDL_C = sum(euro_ahrens2$HDL, na.rm = TRUE), 
                     Glucose = sum(euro_ahrens2$Glucose_homeostasis, na.rm = TRUE), 
                     Insulin_resistance = sum(euro_ahrens2 $Insulin_resistance, na.rm = TRUE), 
                     Criterion = "Ahrens (IDEFICS, monitoring level)"
)

n = nrow(euro_cook2)

cook = cook %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
         variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

idf = idf %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
         variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

ahrens = ahrens %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
         variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))


european = rbind(cook, idf, ahrens) %>% 
  dplyr::mutate(Cohort = "European (N = 716)")

all = rbind(american, european)
all = all %>% 
  dplyr::mutate(Definition = factor(Criterion, levels = c("Cook (NCEP)", "Zimmet (IDF)", "Ahrens (IDEFICS, monitoring level)")))

## 3) Figure 2A -----

bar = ggplot(all, aes(x=variable, y = value, fill = Definition)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label = round(value, 1), 
                vjust = ifelse(value < -1, 1.5, -0.3)), 
            color = "black", 
            size = 3, 
            position = position_dodge(width = 0.9)) + 
  theme_light() + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB")) + 
  xlab("Components of metabolic syndrome in children with obesity (early childhood subset, ages 3-10)") + 
  ylab("Percentage (%)") +   
  facet_wrap(~Cohort) +   
  theme(legend.position = "top",
        strip.text.x = element_text(colour = "Black"), 
        text = element_text(size = 11, family='Times New Roman'),
        panel.spacing = unit(1, "lines")) + 
  ylim(0, 100)

bar

# The figures were exported using the Plot panel in RStudio by selecting 
# Export > Save as PDF, setting the PDF size to A4 
# with landscape orientation, and choosing the `cairo_pdf` device in the options

# Children with obesity (Cook and Zimmet, 11-18 years) -----

## 1) Preparing the data of Hispanic-American -----


ame_cook2 = ame_cook %>% 
  dplyr::filter(decimal_age >= 11)

ame_idf2 = ame_idf %>% 
  dplyr::filter(decimal_age >= 11) 

componentes = colnames(ame_cook2)[20:24]

ame_cook2[, componentes] = lapply(ame_cook2[, componentes], 
                                  FUN = factor, levels = c("Normal", "Altered"), 
                                  labels = c("0", "1"))

ame_idf2[, componentes] = lapply(ame_idf2[, componentes], 
                                 FUN = factor, levels = c("Normal", "Altered"), 
                                 labels = c("0", "1"))


componentes = colnames(ame_cook2)[20:24]

ame_cook2[, componentes] = lapply(ame_cook2[, componentes], 
                                  FUN = factor_to_numeric)

ame_idf2[, componentes] = lapply(ame_idf2[, componentes], 
                                 FUN = factor_to_numeric)



cook = data.frame(Abdominal_obesity = sum(ame_cook2$Obesity, na.rm = TRUE), 
                  Blood_pressure = sum(ame_cook2$Blood_pressure, na.rm = TRUE), 
                  Triacylglycerols = sum(ame_cook2$Tryglicerides, na.rm = TRUE), 
                  HDL_C = sum(ame_cook2$HDL, na.rm = TRUE), 
                  Glucose = sum(ame_cook2$Glucose_homeostasis, na.rm = TRUE), 
                  Insulin_resistance = NA, 
                  Criterion = "Cook (NCEP)"
)

idf = data.frame(Abdominal_obesity = sum(ame_idf2$Obesity, na.rm = TRUE), 
                 Blood_pressure = sum(ame_idf2$Blood_pressure, na.rm = TRUE), 
                 Triacylglycerols = sum(ame_idf2$Tryglicerides, na.rm = TRUE), 
                 HDL_C = sum(ame_idf2$HDL, na.rm = TRUE), 
                 Glucose = sum(ame_idf2$Glucose_homeostasis, na.rm = TRUE), 
                 Insulin_resistance = NA, 
                 Criterion = "Zimmet (IDF)"
)


n = nrow(ame_cook2)

cook = cook %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

idf = idf %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))


american = rbind(cook, idf) %>% 
  dplyr::mutate(Cohort = "American (N = 62)")

## 2) Preparing the data of Hispanic-European -----

euro_cook2 = euro_cook %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl) %>% 
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18)

euro_idf2 = euro_idf %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, 
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl) %>% 
  dplyr::filter(decimal_age >= 11 & decimal_age <= 18)


componentes = colnames(euro_cook2)[20:24]

euro_cook2[, componentes] = lapply(euro_cook2[, componentes], 
                                   FUN = factor, levels = c("Normal", "Altered"), 
                                   labels = c("0", "1"))

euro_idf2[, componentes] = lapply(euro_idf2[, componentes], 
                                  FUN = factor, levels = c("Normal", "Altered"), 
                                  labels = c("0", "1"))



componentes = colnames(euro_cook2)[20:24]

euro_cook2[, componentes] = lapply(euro_cook2[, componentes], 
                                   FUN = factor_to_numeric)

euro_idf2[, componentes] = lapply(euro_idf2[, componentes], 
                                  FUN = factor_to_numeric)


cook = data.frame(Abdominal_obesity = sum(euro_cook2$Obesity, na.rm = TRUE), 
                  Blood_pressure = sum(euro_cook2$Blood_pressure, na.rm = TRUE), 
                  Triacylglycerols = sum(euro_cook2$Tryglicerides, na.rm = TRUE), 
                  HDL_C = sum(euro_cook2$HDL, na.rm = TRUE), 
                  Glucose = sum(euro_cook2$Glucose_homeostasis, na.rm = TRUE), 
                  Insulin_resistance = NA, 
                  Criterion = "Cook (NCEP)"
)

idf = data.frame(Abdominal_obesity = sum(euro_idf2$Obesity, na.rm = TRUE), 
                 Blood_pressure = sum(euro_idf2$Blood_pressure, na.rm = TRUE), 
                 Triacylglycerols = sum(euro_idf2$Tryglicerides, na.rm = TRUE), 
                 HDL_C = sum(euro_idf2$HDL, na.rm = TRUE), 
                 Glucose = sum(euro_idf2$Glucose_homeostasis, na.rm = TRUE), 
                 Insulin_resistance = NA, 
                 Criterion = "Zimmet (IDF)"
)

n = nrow(euro_cook2)

cook = cook %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

idf = idf %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))



european = rbind(cook, idf) %>% 
  dplyr::mutate(Cohort = "European (N = 578)")

all = rbind(american, european)
all = all %>% 
  dplyr::mutate(Definition = factor(Criterion, levels = c("Cook (NCEP)", "Zimmet (IDF)")))

## 3) Figure 2B -----

bar = ggplot(all, aes(x=variable, y = value, fill = Definition)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label = round(value, 1), 
                vjust = ifelse(value < -1, 1.5, -0.3)), 
            color = "black", 
            size = 3.5, 
            position = position_dodge(width = 0.9)) + 
  theme_light() + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) + 
  xlab("Components of metabolic syndrome in adolescents with obesity (adolescence subset, ages 11-18)") + 
  ylab("Percentage (%)") +   
  facet_wrap(~Cohort) +   
  theme(legend.position = "top",
        strip.text.x = element_text(colour = "Black"), 
        text = element_text(size = 13, family='Times New Roman'),
        panel.spacing = unit(1, "lines")) + 
  ylim(0, 100)

bar

# The figures were exported using the Plot panel in RStudio by selecting 
# Export > Save as PDF, setting the PDF size to A4 
# with landscape orientation, and choosing the `cairo_pdf` device in the options

# Children with obesity (Cook and Zimmet , 3-18 years) -----


## 1) Preparing the data of Hispanic-American -----


ame_cook2 = ame_cook

ame_idf2 = ame_idf 

componentes = colnames(ame_cook2)[20:24]

ame_cook2[, componentes] = lapply(ame_cook2[, componentes], 
                                  FUN = factor, levels = c("Normal", "Altered"), 
                                  labels = c("0", "1"))

ame_idf2[, componentes] = lapply(ame_idf2[, componentes], 
                                 FUN = factor, levels = c("Normal", "Altered"), 
                                 labels = c("0", "1"))


componentes = colnames(ame_cook2)[20:24]

ame_cook2[, componentes] = lapply(ame_cook2[, componentes], 
                                  FUN = factor_to_numeric)

ame_idf2[, componentes] = lapply(ame_idf2[, componentes], 
                                 FUN = factor_to_numeric)



cook = data.frame(Abdominal_obesity = sum(ame_cook2$Obesity, na.rm = TRUE), 
                  Blood_pressure = sum(ame_cook2$Blood_pressure, na.rm = TRUE), 
                  Triacylglycerols = sum(ame_cook2$Tryglicerides, na.rm = TRUE), 
                  HDL_C = sum(ame_cook2$HDL, na.rm = TRUE), 
                  Glucose = sum(ame_cook2$Glucose_homeostasis, na.rm = TRUE), 
                  Insulin_resistance = NA, 
                  Criterion = "Cook (NCEP)"
)

idf = data.frame(Abdominal_obesity = sum(ame_idf2$Obesity, na.rm = TRUE), 
                 Blood_pressure = sum(ame_idf2$Blood_pressure, na.rm = TRUE), 
                 Triacylglycerols = sum(ame_idf2$Tryglicerides, na.rm = TRUE), 
                 HDL_C = sum(ame_idf2$HDL, na.rm = TRUE), 
                 Glucose = sum(ame_idf2$Glucose_homeostasis, na.rm = TRUE), 
                 Insulin_resistance = NA, 
                 Criterion = "Zimmet (IDF)"
)


n = nrow(ame_cook2)

cook = cook %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

idf = idf %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))


american = rbind(cook, idf) %>% 
  dplyr::mutate(Cohort = "American (N = 252)")

## 2) Preparing the data of Hispanic-European -----

euro_cook2 = euro_cook %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age <= 18)

euro_idf2 = euro_idf %>% 
  tidyr::drop_na(wc_cm, dbp_mmHg, sbp_mmHg, height_m,
                 tg_mg_dl, hdl_mg_dl, glucose_mg_dl) %>% 
  dplyr::filter(decimal_age >= 3 & decimal_age <= 18)


componentes = colnames(euro_cook2)[20:24]

euro_cook2[, componentes] = lapply(euro_cook2[, componentes], 
                                   FUN = factor, levels = c("Normal", "Altered"), 
                                   labels = c("0", "1"))

euro_idf2[, componentes] = lapply(euro_idf2[, componentes], 
                                  FUN = factor, levels = c("Normal", "Altered"), 
                                  labels = c("0", "1"))



componentes = colnames(euro_cook2)[20:24]

euro_cook2[, componentes] = lapply(euro_cook2[, componentes], 
                                   FUN = factor_to_numeric)

euro_idf2[, componentes] = lapply(euro_idf2[, componentes], 
                                  FUN = factor_to_numeric)


cook = data.frame(Abdominal_obesity = sum(euro_cook2$Obesity, na.rm = TRUE), 
                  Blood_pressure = sum(euro_cook2$Blood_pressure, na.rm = TRUE), 
                  Triacylglycerols = sum(euro_cook2$Tryglicerides, na.rm = TRUE), 
                  HDL_C = sum(euro_cook2$HDL, na.rm = TRUE), 
                  Glucose = sum(euro_cook2$Glucose_homeostasis, na.rm = TRUE), 
                  Insulin_resistance = NA, 
                  Criterion = "Cook (NCEP)"
)

idf = data.frame(Abdominal_obesity = sum(euro_idf2$Obesity, na.rm = TRUE), 
                 Blood_pressure = sum(euro_idf2$Blood_pressure, na.rm = TRUE), 
                 Triacylglycerols = sum(euro_idf2$Tryglicerides, na.rm = TRUE), 
                 HDL_C = sum(euro_idf2$HDL, na.rm = TRUE), 
                 Glucose = sum(euro_idf2$Glucose_homeostasis, na.rm = TRUE), 
                 Insulin_resistance = NA, 
                 Criterion = "Zimmet (IDF)"
)

n = nrow(euro_cook2)

cook = cook %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))

idf = idf %>% 
  tidyr::pivot_longer(!Criterion, names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(value = (value/n) * 100, 
                variable = factor(variable, levels = c("Abdominal_obesity", "Blood_pressure", "Triacylglycerols", "HDL_C", "Glucose", "Insulin_resistance")))



european = rbind(cook, idf) %>% 
  dplyr::mutate(Cohort = "European (N = 1330)")

all = rbind(american, european)
all = all %>% 
  dplyr::mutate(Definition = factor(Criterion, levels = c("Cook (NCEP)", "Zimmet (IDF)")))

## 3) Figure 2C -----

bar = ggplot(all, aes(x=variable, y = value, fill = Definition)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label = round(value, 1), 
                vjust = ifelse(value < -1, 1.5, -0.3)), 
            color = "black", 
            size = 3.5, 
            position = position_dodge(width = 0.9)) + 
  theme_light() + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) + 
  xlab("Components of metabolic syndrome in children and adolescents with obesity (whole population, ages 3-18)") + 
  ylab("Percentage (%)") +   
  facet_wrap(~Cohort) +   
  theme(legend.position = "top",
        strip.text.x = element_text(colour = "Black"), 
        text = element_text(size = 13, family='Times New Roman'),
        panel.spacing = unit(1, "lines")) + 
  ylim(0, 100)

bar

# The figures were exported using the Plot panel in RStudio by selecting 
# Export > Save as PDF, setting the PDF size to A4 
# with landscape orientation, and choosing the `cairo_pdf` device in the options


