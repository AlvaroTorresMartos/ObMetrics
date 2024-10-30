# File: 1_cleaning_preprocessing_data.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script aims to clean and preprocess data from various studies 
#   for subsequent analysis using the web application ObMetrics. 
#   ObMetrics calculates different outputs of Metabolic Syndrome (MetS) 
#   in pediatric populations, including classifications and z-scores, in a 
#   reproducible and automated manner.
# 
#   To evaluate the use of the Shiny app, a case study was conducted involving 
#   two populations: Hispanic-American and Hispanic-European. 
#   This script integrates data from multiple cohorts in both regions, 
#   ensuring data consistency and quality for analysis.

# Tasks performed in this script:
#   - **Loading necessary packages** required for the script execution.
#   - **Defining functions** to verify and correct erroneous values in blood pressure measurements:
#     - `check_bp()`: Checks if systolic blood pressure is less than diastolic blood pressure and flags incorrect records.
#     - `change_bp()`: Corrects values by swapping systolic and diastolic measurements when necessary.
#   - **Importing column names** from the ObMetrics template to standardize datasets.
#   - **Importing and processing data from Hispanic-European cohorts**:
#     - **GENOBOX Study**:
#       - Selection of relevant variables for MetS.
#       - Renaming variables to match the template.
#       - Filtering by age range (3 to 18 years) and sex.
#       - Conversion and recoding of variables where necessary.
#     - **IBEROMICS Study**:
#       - Similar processing to the GENOBOX study.
#     - **Verifying and correcting blood pressure values** in both cohorts.
#     - **Merging datasets** from GENOBOX and IBEROMICS into `cohorte_espanola`.
#     - **Exporting** the processed Hispanic-European cohort.
#   - **Importing and processing data from Hispanic-American cohorts**:
#     - **Montemorelos Cohort, Mexico (Dr. Acosta)**:
#       - Selection and renaming of variables.
#       - Adding missing variables with NA values.
#       - Verifying and correcting blood pressure values.
#     - **Mexico City Cohort, Mexico (Dr. Jenny)**:
#       - Similar processing, no need to correct blood pressure values.
#     - **Medellín Cohort, Colombia (Dr. Juan Carlos)**:
#       - Selection and transformation of variables.
#       - Unit conversion and sex recoding.
#     - **Pachuca Cohort, Mexico (Dr. Guadalupe)**:
#       - Processing and standardization of variables.
#     - **Monterrey Cohort, Mexico (Drs. Susana Romo, Marcos, and Jazmin)**:
#       - Separation of combined columns and data type conversion.
#       - Verifying and correcting blood pressure values.
#     - **Merging datasets** from all Hispanic-American cohorts into `iberoamerican_cohort`.
#     - **Exporting** the processed Hispanic-American cohort.

# Input files:
#   - **plantilla.xlsx**: File containing column names from the ObMetrics template.
#   - **Hispanic-European Studies**:
#     - **2023_03_01_GENOBOX_BASE_COMPLETA_INTEGRADA.RDS**: Dataset from the GENOBOX study.
#     - **2023_07_19_Base_integrada_OMICS_def.RDS**: Dataset from the IBEROMICS study.
#   - **Hispanic-American Cohorts**:
#     - **08112022-DATA OBMETRICS-ACOSTA.xls**: Data from the Montemorelos cohort.
#     - **BaseCiudadMexico_CMD_Jenny_Vilchiz.xlsx**: Data from the Mexico City cohort.
#     - **Datos Medellín para Obsmetrics 2023.xlsx**: Data from the Medellín cohort.
#     - **Guadalupe_Marcos_Base_SIME_Hidalgo.sav**: Data from the Pachuca cohort.
#     - **Susana Romo y Jazmin BD Programa Ob infantil FaSPyN.xlsx**: Data from the Monterrey cohort.

# Output files:
#   - **2023_10_23_spanish_cohort.csv**: Combined and processed dataset of the Hispanic-European cohorts.
#   - **2023_10_23_iberoamerican_cohort.csv**: Combined and processed dataset of the Hispanic-American cohorts.


# Load packages ------
library(magrittr)
library(dplyr)
library(readxl)
library(tibble)
library(haven)
library(tidyr)

# Load functions -------
# Systolic blood pressure is always higher than diastolic blood pressure due 
# to biological reasons.
# The check_bp() function is designed to verify this relationship.
check_bp = function(data){
  data = data %>%
    dplyr::mutate(wrong_bp = ifelse(sbp_mmHg < dbp_mmHg, TRUE, FALSE),
                  sbp_new = NA,
                  dbp_new = NA)
  for(i in 1:nrow(data)){
    if(isTRUE(data$wrong_bp[i])){
      data$sbp_new[i] = data$dbp_mmHg[i]
      data$dbp_new[i] = data$sbp_mmHg[i]
  }}
  return(data)
}


# change_bp() function is designed to change the BP values 
# once checked the wrong values using the check_bp() function
change_bp = function(data){
  data = data %>% 
    dplyr::mutate(wrong_bp = ifelse(sbp_mmHg < dbp_mmHg, TRUE, FALSE),
                  sbp_new = NA, 
                  dbp_new = NA
    )
  for(i in 1:nrow(data)){
    if(isTRUE(data$wrong_bp[i])){
      data$sbp_new[i] = data$dbp_mmHg[i]
      data$dbp_new[i] = data$sbp_mmHg[i]
      data$dbp_mmHg[i] = data$dbp_new[i]
      data$sbp_mmHg[i] = data$sbp_new[i]
    }
  }
  data = data %>%
    dplyr::select(-c(wrong_bp, sbp_new, dbp_new))
  return(data)
}

# Import the names of columns of the ObMetrics template -----
vars = readxl::read_xlsx("./plantilla.xlsx") %>% colnames()
# vars 
# [1] "id"                "decimal_age"       "sex"               "height_m"          "weight_kg"               
# [6] "wc_cm"             "dbp_mmHg"          "sbp_mmHg"          "tg_mg_dl"          "hdl_mg_dl"        
# [11] "glucose_mg_dl"     "insulin_microU_ml" "tanner_index"

# 1) Import the Hispanic-European datasets (GENOBOX and IBEROMICS studies) ------

genobox = readRDS("/home/usuario/Escritorio/CIBM/BASES_IBEROMICS/OMICS/OMICS_BD/genobox_pubmep/2023_03_01_GENOBOX_BASE_COMPLETA_INTEGRADA.RDS")

iberomics = readRDS("/home/usuario/Escritorio/CIBM/BASES_IBEROMICS/OMICS/OMICS_BD/RAW_INPUT_CENTROS_22_02_22/2023_07_19_Base_integrada_OMICS_def.RDS")

## 1.1) Select the MetS variables in GENOBOX study -----
genobox = genobox %>% 
  dplyr::select(4, Age, Sex, Height, Weight,
                WC, DBP, SBP, `TAG (mg/dl)`, `HDLc (mg/dl)`,
                `Glucose (mg/dl)`, `Insulin (mU/l)`, Tanner#,
                # Origen
  ) %>%
  dplyr::mutate(Tanner = ifelse(Tanner  == 0, 1, 2)) %>% 
  dplyr::filter(Age >= 3 & Age <= 18) %>% 
  dplyr::filter(Sex < 2) #%>%  
  # tidyr::drop_na(WC, DBP, SBP, `TAG (mg/dl)`, `HDLc (mg/dl)`,
  #                `Glucose (mg/dl)`,  Height)

# genobox$Origen %>% table()
# 0   1   2 
# 518 820 296

## 1.2) Change the names of the variables ----- 
colnames(genobox) = vars

## 2.1) Select the MetS variables in IBEROMICS study -----
iberomics = iberomics %>% 
  dplyr::select(Code, Age, Sex, Height, Peso_Kg, 
                Perimetro_de_cintura, TD, TS, TAG__mg_dl_, HDLc__mg_dl_, 
                Glucose__mg_dl_, Insulin__mU_ml_, Estadio_tanner#, 
                # Origen
                ) %>% 
  dplyr::mutate(Sex = ifelse(Sex == 1, 0, 1), 
                Height = Height/100) %>% 
  dplyr::filter(Age >= 3 & Age <= 18) #  %>% 
  # tidyr::drop_na(Perimetro_de_cintura, TD, TS, TAG__mg_dl_, HDLc__mg_dl_, 
  #                Glucose__mg_dl_, Height)


# iberomics$Origen %>% table()
# 1   2 
# 677 100 

## 2.2) Change the names of the variables ----- 
colnames(genobox) = vars
colnames(iberomics) = vars


## 3.1) Check blood pressure variables ----
# iberomics %>% check_bp() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# FALSE  TRUE 
# 832     3 
# genobox %>% check_bp() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# FALSE  TRUE 
# 1690    10 

## 3.2) Change the blood pressure values when the values were wrong ----
iberomics = iberomics %>% change_bp()
genobox = genobox %>% change_bp()

## 3.3) Bind the datasets of GENOBOX and IBEROMICS studies -----
cohorte_espanola = rbind(genobox, iberomics)

## 3.4) Export the Hispanic-European dataset ------
# write.csv2(cohorte_espanola, "./inputs/2023_10_23_spanish_cohort.csv")

# 2) Import the Hispanic-American datasets ------
# Monterelos, Mexico City, Medellin, Pachuca and Monterrey cohorts

## 2.1) Cohort Montemorelos, México (Dr Acosta) -----
acosta = readxl::read_xls("./inputs/BD_original/08112022-DATA OBMETRICS-ACOSTA.xls")
### Select the MetS variables -------
acosta = acosta %>% 
  tibble::rownames_to_column(var = "id") %>% 
  dplyr::select(1:11) %>% 
  dplyr::mutate(Insulin = NA, Tanner = NA )

### Changes the names of the variables -------
colnames(acosta) = vars



### Check and change blood pressure variables ----
# acosta %>% check_bp() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# TRUE 
# 274 
acosta = acosta %>% change_bp() 

## 2.2) Cohort Mexico City, México (Dr Jenny) -----
jenny = readxl::read_xlsx("./inputs/BD_original/BaseCiudadMexico_CMD_Jenny_Vilchiz.xlsx") %>% 
  dplyr::select(1:13)

### Check blood pressure variables ----
# jenny %>% check_bp() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# FALSE 
# 308 

## 2.3) Cohort Medellin, Colombia (Dr Juan Carlos) -----
medellin = readxl::read_xlsx("./inputs/BD_original/Datos Medellín para Obsmetrics 2023.xlsx")

### Select the MetS variables -------
medellin = medellin %>% 
  dplyr::select(CÓDIGO, `EDAD DECIMAL`, SEXO, `TALLA (cm)`, 
                `PESO (kg)`, `Cintura (cm)`, `Presión Diastólica`, 
                `Presión Sistólica`, `TRIGLICERIDOS (mg/dL)`, 
                `COLESTEROL HDL (mg/dL)`, `GLUCOSA (mg/dL)`, INSULINA) %>% 
  dplyr::mutate(SEXO = if_else(SEXO == "f", 1, 0), 
        `TALLA (cm)` = `TALLA (cm)`/100, 
        Tanner = NA)

### Changes the names of the variables -------
colnames(medellin) = vars


### Check blood pressure variables ----
# medellin %>% check_blood_pressure() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# FALSE 
# 211 

## 2.4) Cohort Pachuca, México (Dr Guadalupe) -----
guada = haven::read_sav("./inputs/BD_original/Guadalupe_Marcos_Base_SIME_Hidalgo.sav")

### Select the MetS variables -------
guada = guada %>% 
  dplyr::select(folio, edad_año, sexo, talla_cm, peso_kg, circintu, 
                 pres_dia, pres_sis, triglice, HDL, glucosa, INSULINA) %>% 
  dplyr::mutate(sexo = if_else(sexo == 1, 0, 1), 
         talla_cm = talla_cm/100, 
         Tanner = NA)

### Changes the names of the variables -------
colnames(guada) = vars

### Check blood pressure variables ----
# guada %>% check_bp() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# FALSE 
# 655 

## 2.5) Cohort Monterrey, México (Drs Susana Romo, Marcos y Jhazmin) -----
susa = readxl::read_xlsx("./inputs/BD_original/Susana Romo (del grupo de Dra Elisabeth) y a Jazmin BD Programa Ob infantil FaSPyN .xlsx")

### Select the MetS variables -------
susa = susa %>% 
  dplyr::select(Id...2, `Age (edad decimal)`, `Sex 0=male, 1=female`, 
                `height metros`, `weight kg`, `wc cm`, `dbp/sbp mmHg`, 
                `tg mg/dl`, `hdl mg/ dl`, `glucosa mg/dl`, 
                `insulin μU/ml`, `Tanner Mamario/ genital`) %>% 
  tidyr::separate_wider_delim( `dbp/sbp mmHg`, "/", names = c("dbp", "sbp")) %>% 
  dplyr::mutate(dbp = as.numeric(dbp),
         sbp = as.numeric(sbp))

### Changes the names of the variables -------
colnames(susa) = vars

### Check and change blood pressure variables ----
# susa %>% check_bp() %>% dplyr::select(wrong_bp) %>% table()
# wrong_bp
# TRUE 
# 318 
susa = susa %>% change_bp()



## 2.6) Export the Hispanic-American dataset ------
iberoamerican_cohort = rbind(acosta, guada, jenny, medellin, susa) %>% 
  dplyr::mutate(weight_kg = as.numeric(weight_kg))

# summary(iberoamerican_cohort)


## 2.7) Export the Hispanic-European dataset ------
# write.csv2(iberoamerican_cohort, "./inputs/2023_10_23_iberoamerican_cohort.csv")
