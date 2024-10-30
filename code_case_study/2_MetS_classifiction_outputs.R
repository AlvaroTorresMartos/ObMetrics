# File: 2_Mets_classification_outputs.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script calculates the Metabolic Syndrome (MetS) classification 
#   for both the Hispanic-European and Hispanic-American cohorts using different 
#   diagnostic criteria. The criteria applied include those proposed 
#   by Cook et al. (2003), Zimmet et al. (2007), and Ahrens et al. (2014). 
#   The script utilizes the ObMetrics functions to perform these calculations 
#   in an automated and reproducible manner.

# Tasks performed in this script:
#   - **Load necessary packages** required for data manipulation and function execution.
#   - **Load custom functions** from `functions.R` and `read_reference.R`, which contain the necessary algorithms and reference data for MetS classification.
#   - **Import datasets**:
#     - `spanish`: The processed dataset of the Hispanic-European cohorts.
#     - `iberoamerican`: The processed dataset of the Hispanic-American cohorts.
#   - **Calculate MetS classification** for each cohort using the specified criteria:
#     - **Hispanic-European Cohort**:
#       - Calculate MetS using Cook et al. (2003) criteria.
#       - Calculate MetS using Zimmet et al. (2007) criteria.
#       - Calculate MetS using Ahrens et al. (2014) criteria.
#     - **Hispanic-American Cohort**:
#       - Perform the same calculations as above.
#   - **Export the resulting datasets** with the MetS classifications for further analysis.

# Input files:
#   - **`./inputs/2023_10_23_spanish_cohort.csv`**: Combined and processed dataset of the Hispanic-European cohorts.
#   - **`./inputs/2023_10_23_iberoamerican_cohort.csv`**: Combined and processed dataset of the Hispanic-American cohorts.
#   - **`functions.R`**: Script containing custom functions required for MetS calculations.
#   - **`read_reference.R`**: Script containing reference data and additional functions.

# Output files:
#   - **For the Hispanic-European Cohort**:
#     - `./outputs/2023_10_23_spanish_cook.csv`: MetS classification using Cook et al. (2003) criteria.
#     - `./outputs/2023_10_23_spanish_idf.csv`: MetS classification using Zimmet et al. (2007) criteria.
#     - `./outputs/2023_10_23_spanish_ahrens.csv`: MetS classification using Ahrens et al. (2014) criteria.
#   - **For the Hispanic-American Cohort**:
#     - `./outputs/2023_10_23_iberoamerican_cook.csv`: MetS classification using Cook et al. (2003) criteria.
#     - `./outputs/2023_10_23_iberoamerican_idf.csv`: MetS classification using Zimmet et al. (2007) criteria.
#     - `./outputs/2023_10_23_iberoamerican_ahrens.csv`: MetS classification using Ahrens et al. (2014) criteria.


# Load packages ------
library(magrittr)
library(dplyr)

# Load ObMetrics functions ------
source('functions.R')
source("read_reference.R")

# Import datasets ----
spanish = read.csv2("./inputs/2023_10_23_spanish_cohort.csv", 
                    row.names = 1)
iberoamerican = read.csv2("./inputs/2023_10_23_iberoamerican_cohort.csv", 
                          row.names = 1)

# 1) Hispanic-European dataset ----
## Calculate the MetS classification (Cook et al (2003), Zimmet et al (2007) and Ahrens et al (2014) ----
spanish_cook = spanish %>% select_criterion(first_choice = 1, mets = 1, 
                                            obesity_comp = 1)

spanish_idf = spanish %>% select_criterion(first_choice = 1, mets = 5, 
                                           obesity_comp = 1)

spanish_ahrens = spanish %>% select_criterion(first_choice = 1, mets = 7)

## Export datasets -------
# write.csv2(spanish_cook, "./outputs/2023_10_23_spanish_cook.csv", row.names = FALSE)
# write.csv2(spanish_idf, "./outputs/2023_10_23_spanish_idf.csv", row.names = FALSE)
# write.csv2(spanish_ahrens, "./outputs/2023_10_23_spanish_ahrens.csv", row.names = FALSE)

# 2) Hispanic-European dataset ----
## Calculate the MetS classification (Cook, Zimmet and Ahrens) ----
iberoamerican_cook = iberoamerican %>% select_criterion(first_choice = 1, mets = 1, 
                                                        obesity_comp = 1)

iberoamerican_idf = iberoamerican %>% select_criterion(first_choice = 1, mets = 5, 
                                                       obesity_comp = 1)

iberoamerican_ahrens = iberoamerican %>% select_criterion(first_choice = 1, mets = 7)


## Export datasets -------
# write.csv2(iberoamerican_cook, "./outputs/2023_10_23_iberoamerican_cook.csv", row.names = FALSE)
# write.csv2(iberoamerican_idf, "./outputs/2023_10_23_iberoamerican_idf.csv", row.names = FALSE)
# write.csv2(iberoamerican_ahrens, "./outputs/2023_10_23_iberoamerican_ahrens.csv", row.names = FALSE)
