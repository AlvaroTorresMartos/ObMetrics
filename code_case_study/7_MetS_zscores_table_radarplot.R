# File: 7_MetS_zscores_table_radarplot.R
# Author: Álvaro Torres-Martos
# Date: October 30, 2024
# Purpose:
#   This script calculates Metabolic Syndrome (MetS) z-scores and develops a 
#   descriptive table for the z-scores. It also generates a radar plot displaying 
#   the median of each z-score, separated by obesity status. 
#   The analysis compares data between Hispanic-European and Hispanic-American 
#   cohorts to understand the distribution of MetS components across different
#   obesity statuses and populations.

# Tasks performed in this script:
#   - **Load necessary packages** required for data manipulation, statistical analysis, and visualization, including `dplyr`, `gtsummary`, and `fmsb`.
#   - **Load custom functions and reference data**:
#     - `zscores_functions.R`: Contains functions for calculating various z-scores related to MetS components.
#     - `reference_tables_obmetrics.RData`: Provides reference data required for accurate z-score calculations.
#   - **Define helper functions**:
#     - `create_beautiful_radarchart()`: Custom function to create aesthetically pleasing radar charts using the `fmsb` package.
#   - **Import datasets** for the Hispanic-European (`genobox`) and Hispanic-American (`american`) cohorts.
#   - **Calculate MetS z-scores** for each participant in both cohorts using the appropriate reference data:
#     - **Calculate BMI, HOMA-IR, height z-score, and classify obesity status** according to Cole et al. (2002).
#     - **Calculate waist circumference z-score** using the Stravnsbo method.
#     - **Calculate blood pressure z-scores** for systolic and diastolic blood pressure.
#     - **Calculate triglycerides, HDL cholesterol, glucose, and HOMA-IR z-scores** using the Stravnsbo method.
#   - **Develop a descriptive table** (Table 4) for MetS z-scores, stratified by obesity status (normal-weight, overweight, obesity) and cohort:
#     - **Perform statistical comparisons** and obtain p-values for each variable across obesity statuses and cohorts.
#     - **Create additional tables** to get p-values specific to each obesity status group.
#   - **Prepare data for the radar plot** by:
#     - **Calculating median z-scores** for each MetS component, separated by obesity status and cohort.
#     - **Defining maximum and minimum values** for the radar plot axes to standardize the scale.
#     - **Arranging the data** in the required format for the `fmsb` package.
#   - **Generate a radar plot** (Figure 2D) displaying the median z-scores of MetS components, comparing the Hispanic-European and Hispanic-American cohorts across obesity status groups:
#     - **Plotting separate radar charts** for normal-weight, overweight, and obesity groups.
#     - **Customizing the appearance** of the radar charts, including colors, labels, and axis scales.
#   - **Export the general table and radar plot** using appropriate settings for high-quality output suitable for publication:
#     - **Table export**: Configured for A3 page size in portrait orientation.
#     - **Radar plot export**: Saved as a PDF with custom dimensions using the `cairo_pdf` device.

# Input files:
#   - `./inputs/2023_10_23_spanish_cohort.csv`: Dataset containing the Hispanic-European cohort data.
#   - `./inputs/2023_10_23_iberoamerican_cohort.csv`: Dataset containing the Hispanic-American cohort data.
#   - `./reference_tables/COLE_TABLE_CUTOFFS`: Reference table for obesity classification according to Cole et al. (2002).
#   - `zscores_functions.R`: Script containing functions for calculating z-scores for MetS components.
#   - `/home/usuario/Escritorio/Shiny/ObMetrics/obmetrics_app/reference_tables_obmetrics.RData`: Reference data required for accurate z-score calculations.

# Output:
#   - **Table 4**: Descriptive table summarizing MetS z-scores across different obesity statuses and cohorts, including statistical comparisons.
#   - **Figure 2D**: Radar plot visualizing the median z-scores of MetS components for normal-weight, overweight, and obesity groups in both cohorts.

# Note:
#   - The radar plot is exported using the Plot panel in RStudio by selecting Export > Save as PDF, setting custom dimensions (width: 12, height: 3.2), and choosing the `cairo_pdf` device for better rendering of vector graphics.
#   - The table is exported as a Word document using the `flextable` package with specific page size settings to ensure readability and proper formatting.



# Load packages -----
library(magrittr)
library(dplyr)
library(gtsummary)
library(flextable)
library(officer)
library(gt)
library(tidyr)
library(tibble)
library(fmsb)

# Load functions and reference data from ObMetrics  -------
load("/home/usuario/Escritorio/Shiny/ObMetrics/obmetrics_app/reference_tables_obmetrics.RData")
source("./zscores_functions.R")

# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/
create_beautiful_radarchart = function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Import Hispanic-European and Hispanic-American datasets  -----

genobox = read.csv2("./inputs/2023_10_23_spanish_cohort.csv", row.names = 1)

american = read.csv2("./inputs/2023_10_23_iberoamerican_cohort.csv", row.names = 1)

# Calculate the bmi, homa, height z-score, Obesity status, MetS z-scores ----

genobox = genobox %>% 
  bmi() %>% homa() %>% 
  calculate_height_zscore() %>% 
  calculate_cole(reference_table = obesity_bmi_cole) %>% 
  wc_zscore_stravnsbo() %>% 
  calculate_bp_zscore() %>% 
  tag_zscore_stravnsbo() %>% 
  hdl_zscore_stravnsbo() %>% 
  glu_zscore_stravnsbo() %>% 
  homa_zscore_stravnsbo() 

american = american %>% 
  bmi() %>% homa() %>% 
  calculate_height_zscore() %>% 
  calculate_cole(reference_table = obesity_bmi_cole) %>% 
  wc_zscore_stravnsbo() %>% 
  calculate_bp_zscore() %>% 
  tag_zscore_stravnsbo() %>% 
  hdl_zscore_stravnsbo() %>% 
  glu_zscore_stravnsbo() %>% 
  homa_zscore_stravnsbo() 

# Descriptive table of MetS z-scores -----
## General table (Table 4) ------
t2 = genobox %>%
  dplyr::filter(decimal_age > 5.9999) %>%
  dplyr::select(decimal_age:sex, Height_zscore:HOMA_zscore) %>%
  dplyr::mutate(Obesity_Cole = factor(Obesity_Cole, 
                                      levels = c("Normal-weight", "Overweight", 
                                                 "Obesity")),
                sex = ifelse(sex == 0, "Male", "Female")
                ) %>%
  tbl_summary(by = Obesity_Cole,
              label = list(sex  ~ "Sex",
                           decimal_age ~ "Age (years)",
                           Height_zscore ~ "Height z-score",
                           WC_zscore ~ "WC z-score",
                           DBP_zscore ~ "DBP z-score",
                           SBP_zscore ~ "SBP z-score",
                           TAG_zscore ~ "TG z-score",
                           HDL_zscore ~ "HDL z-score",
                           Glucose_zscore ~ "Glucose z-score",
                           HOMA_zscore ~ "HOMA-IR z-score"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values"
              ) %>%
    add_p() %>% add_n() %>%
  bold_labels() %>%
    modify_header(label = "**Variable**")

t1 = american %>%
  dplyr::filter(decimal_age > 5.9999) %>%
  dplyr::select(decimal_age:sex, Height_zscore:HOMA_zscore) %>%
  dplyr::mutate(Obesity_Cole = factor(Obesity_Cole,
                                      levels = c("Normal-weight", "Overweight", "Obesity")),
                sex = ifelse(sex == 0, "Male", "Female")
  ) %>%
  tbl_summary(by = Obesity_Cole,
              label = list(sex  ~ "Sex",
                           decimal_age ~ "Age (years)",
                           Height_zscore ~ "Height z-score",
                           WC_zscore ~ "WC z-score",
                           DBP_zscore ~ "DBP z-score",
                           SBP_zscore ~ "SBP z-score",
                           TAG_zscore ~ "TG z-score",
                           HDL_zscore ~ "HDL z-score",
                           Glucose_zscore ~ "Glucose z-score",
                           HOMA_zscore ~ "HOMA-IR z-score"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values"
  ) %>%
  add_p() %>% add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variable**")

tab_gen = tbl_merge(tbls = list(t1, t2),
                         tab_spanner = c("**Hispanic-American**", "**Hispanic-European**"))
tab_gen

## Additional tables to get the p-values ------
genobox_2 = genobox %>% 
  dplyr::mutate(Cohort = "EUROPEAN")
american_2 = american %>% 
  dplyr::mutate(Cohort = "AMERICAN")

all = rbind(genobox_2, american_2)

global = all %>%
  dplyr::filter(decimal_age > 5.9999) %>%
  dplyr::select(decimal_age:sex, Height_zscore:Cohort) %>%
  dplyr::mutate(Obesity_Cole = factor(Obesity_Cole,
                                      levels = c("Normal-weight", "Overweight", "Obesity")),
                sex = ifelse(sex == 0, "Male", "Female")
  ) %>%
  tbl_summary(by = Cohort,
              label = list(
                           sex  ~ "Sex",
                           decimal_age ~ "Age (years)",
                           Height_zscore ~ "Height z-score",
                           Obesity_Cole ~ "Obesity status",
                           WC_zscore ~ "WC z-score",
                           DBP_zscore ~ "DBP z-score",
                           SBP_zscore ~ "SBP z-score",
                           TAG_zscore ~ "TG z-score",
                           HDL_zscore ~ "HDL z-score",
                           Glucose_zscore ~ "Glucose z-score",
                           HOMA_zscore ~ "HOMA-IR z-score"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values"
  ) %>%
  add_p()  %>% add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variable**")

global

normo = all %>%
  dplyr::filter(decimal_age > 5.9999) %>%
  dplyr::select(decimal_age:sex, Height_zscore:Cohort) %>%
  dplyr::mutate(Obesity_Cole = factor(Obesity_Cole,
                                      levels = c("Normal-weight", "Overweight", "Obesity")),
                sex = ifelse(sex == 0, "Male", "Female")
  ) %>%
  dplyr::filter(Obesity_Cole == "Normal-weight") %>%
  tbl_summary(by = Cohort,
              label = list(
                sex  ~ "Sex",
                decimal_age ~ "Age (years)",
                Height_zscore ~ "Height z-score",
                Obesity_Cole ~ "Obesity status",
                WC_zscore ~ "WC z-score",
                DBP_zscore ~ "DBP z-score",
                SBP_zscore ~ "SBP z-score",
                TAG_zscore ~ "TG z-score",
                HDL_zscore ~ "HDL z-score",
                Glucose_zscore ~ "Glucose z-score",
                HOMA_zscore ~ "HOMA-IR z-score"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values"
  ) %>%
  add_p()  %>% add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variable**")

normo

over = all %>%
  dplyr::filter(decimal_age > 5.9999) %>%
  dplyr::select(decimal_age:sex, Height_zscore:Cohort) %>%
  dplyr::mutate(Obesity_Cole = factor(Obesity_Cole,
                                      levels = c("Normal-weight", "Overweight", "Obesity")),
                sex = ifelse(sex == 0, "Male", "Female")
  ) %>%
  dplyr::filter(Obesity_Cole == "Overweight") %>%
  tbl_summary(by = Cohort,
              label = list(
                sex  ~ "Sex",
                decimal_age ~ "Age (years)",
                Height_zscore ~ "Height z-score",
                Obesity_Cole ~ "Obesity status",
                WC_zscore ~ "WC z-score",
                DBP_zscore ~ "DBP z-score",
                SBP_zscore ~ "SBP z-score",
                TAG_zscore ~ "TG z-score",
                HDL_zscore ~ "HDL z-score",
                Glucose_zscore ~ "Glucose z-score",
                HOMA_zscore ~ "HOMA-IR z-score"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values"
  ) %>%
  add_p()  %>% add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variable**")

over

obesity = all %>%
  dplyr::filter(decimal_age > 5.9999) %>%
  dplyr::select(decimal_age:sex, Height_zscore:Cohort) %>%
  dplyr::mutate(Obesity_Cole = factor(Obesity_Cole,
                                      levels = c("Normal-weight", "Overweight", "Obesity")),
                sex = ifelse(sex == 0, "Male", "Female")
  ) %>%
  dplyr::filter(Obesity_Cole == "Obesity") %>%
  tbl_summary(by = Cohort,
              label = list(
                sex  ~ "Sex",
                decimal_age ~ "Age (years)",
                Height_zscore ~ "Height z-score",
                Obesity_Cole ~ "Obesity status",
                WC_zscore ~ "WC z-score",
                DBP_zscore ~ "DBP z-score",
                SBP_zscore ~ "SBP z-score",
                TAG_zscore ~ "TG z-score",
                HDL_zscore ~ "HDL z-score",
                Glucose_zscore ~ "Glucose z-score",
                HOMA_zscore ~ "HOMA-IR z-score"),
              digits = list(all_categorical() ~ c(0, 2)),
              missing_text = "Missing values"
  ) %>%
  add_p()  %>% add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variable**")

obesity

## Export the general table -----
# 
# page_size_A3 = officer::page_size(width = 11.16,
#                                   height = 20, orient = "portrait")
# 
# tab_gen  %>% as_flex_table() %>%
#    flextable::save_as_docx(path = "./Table9.docx", align = "center",
#                 pr_section = officer::prop_section(page_size = page_size_A3))




# Preprocessing data for the radar plot -----
## Select only the Obesity Status MetS z-scores -----
genobox2 = genobox %>% 
  dplyr::select(WC_zscore:HOMA_zscore, Obesity_Cole) %>% 
  dplyr::group_by(Obesity_Cole) %>% 
  tidyr::drop_na(Obesity_Cole, WC_zscore, SBP_zscore, DBP_zscore, 
                 TAG_zscore, HDL_zscore, Glucose_zscore, HOMA_zscore) %>% 
  dplyr::summarise(Waist_circumference = median(WC_zscore), 
                   Systolic_blood_pressure = median(SBP_zscore),
                   Diastolic_blood_pressure = median(DBP_zscore),
                   Triacylglycerols = median(TAG_zscore),
                   HDL_C = median(HDL_zscore) * -1,
                   Glucose = median(Glucose_zscore),
                   HOMA_IR = median(HOMA_zscore)) %>% 
  tibble::column_to_rownames(var = "Obesity_Cole") 

american2 = american %>% 
  dplyr::select(WC_zscore:HOMA_zscore, Obesity_Cole) %>% 
  dplyr::group_by(Obesity_Cole) %>% 
  tidyr::drop_na(Obesity_Cole, WC_zscore, SBP_zscore, DBP_zscore, 
                 TAG_zscore, HDL_zscore, Glucose_zscore, HOMA_zscore) %>% 
  dplyr::summarise(Waist_circumference = median(WC_zscore), 
                   Systolic_blood_pressure = median(SBP_zscore),
                   Diastolic_blood_pressure = median(DBP_zscore),
                   Triacylglycerols = median(TAG_zscore),
                   HDL_C = median(HDL_zscore) * -1,
                   Glucose = median(Glucose_zscore),
                   HOMA_IR = median(HOMA_zscore)) %>% 
  tibble::column_to_rownames(var = "Obesity_Cole") 

genobox2 = genobox2[c(1,3,2), ]
american2 = american2[c(1,3,2), ]



## Max and min in each Mets components z-score ------
max_min = data.frame(
  Waist_circumference = c(3, -1), 
  Systolic_blood_pressure = c(3, -1), 
  Diastolic_blood_pressure = c(3, -1),
  Triacylglycerols = c(3, -1), 
  HDL_C = c(3, -1), 
  Glucose = c(3, -1),
  HOMA_IR = c(3, -1)
)

rownames(max_min) = c("Max", "Min")

## Preparing the data ------
genobox2 = rbind(max_min, genobox2)
df2 = rbind(genobox2, american2)



# Figure 2D: Radar plot -----
titles = c("Normal-weight", "Overweight", "Obesity")

op = par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))


# Create the radar chart
for(i in 1:3){
  create_beautiful_radarchart(
    data = df2[c(1, 2, i+2, i+5), ],
    # data = df2[c(1, 2, 3+2, 3+5), ], 
    caxislabels = c(-1, 0, 1, 2, 3),
    color = c("#5AAA46", "#E7872B"), title = titles[i]
  )
}
par(op)

# The figures were exported using the Plot panel in RStudio by selecting 
# Export > Save as PDF, setting the PDF size to Custom 12 and 3.2 
# with landscape orientation, and choosing the `cairo_pdf` device in the options


