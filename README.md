# "Inter-Rater Reliability in Assessing the Methodological Quality of Research Papers in Psychology"

This repository contains the full code and partial raw data for the paper "Inter-Rater Reliability in Assessing the Methodological Quality of Research Papers in Psychology" by Franka Etzel, Anna Seyffert-Müller, Felix D. Schönbrodt, Lucie Kreuzer, Anne Gärtner, Paula Knischewski & Daniel Leising.

## Data availability

In Study 1, many of our participants were early career researchers currently (or soon) on the job market. We asked them to nominate their three best papers. We promised not to share individual data in order to increase the response rate and to remove potential risks for their application processes. We have seen no way of properly anonymizing the data, as the doi always leads to the first author, and also data like the h-index (in combination with the JIF of a journal) could be used to identify participants. Therefore the results of study 1 are not reproducible, but we share the full script to understand the steps taken in the analysis. 

For Study 2, all primary data is available (see `raw_data/Study2`).

## Materials

You can find the full material in `material`, which includes the rating sheets and other questionnaires.
**Please note that the rating scheme has evolved since this study. We discourage to use the old schemes from this repository. Rather go to the [RESQUE website](https://nicebread.github.io/RESQUE/) and use the updated rating scheme.**

## Preregistration

Study 2 was [preregistered](preregistration/Preregistration%20Study%202.pdf).

## How to reproduce

We share the full analysis scripts, but only results for Study 2 are reproducible for reasons outlined above ("data availability").
Set the top folder as working directory. If you had all primary data, you could reproduce all analyses by running all `.R` files in the top folder in their numbered order. With only access to the data of Study 2, two scripts are reproducible: `04-Analysis_Study2.R` and the second part of `05-Exploratory_analysis.R` (see also comments below).

### Install all necessary packages

```R 
install.packages("remotes")
remotes::install_github("nicebread/OAmetrics")
install.packages(c("openalexR", "dplyr", "rio", "ggplot2", "stringr", "DescTools", "psych", "openxlsx", "tidyr", "tidyverse", "irr", "psy", "Rfast", "Hmisc", "flextable", "officer", "scipub", "robustbase"))
```

### Run the scripts

- 00-get_reference_set.R
  - This small scripts downloads a reference set (for field and age normalized citation score). This takes a few hours; the resulting download for the calculations that are presented in the paper is stored in `processed_data/c_counts_psy_2001_2023.RDS`.
  - You could rerun this file, but this will change the results. We recommend to use our intermediate data file.
- 01-get_h_index.R
  - This script loads DOIs from two studies, fetches corresponding papers from OpenAlex, calculates the Journal Impact Factor (JIF) and h-index for each paper, and computes Field Normalized Citation Ratio (FNCR) and Field Normalized Paper Ratio (FNPR) for each paper.
  - You could rerun the script for Study 2, but you will get different h-indexes. For reproduction of our results, rather run `04-Analysis_Study2.R` which accesses an intermediate data file.
- 02-Consolidate_files.R
  - This script combines multiple data sources: The h-indexes, FNCS etc. retrieved from OpenAlex, the manual h-indexes from Scopus, and more.
- 03-Analysis_Study1.R
  - This script computes Kappa and ICCs for all indicators and the overall relative rigor score (RRS) in Study 1
  - Without the data from Study 1, this script is NOT reproducible.
- 04-Analysis_Study2.R
  - This script computes ICCs for all indicators and the overall relative rigor score (RRS) in Study 2
  - This script IS reproducible.
- 05-Exploratory_analysis.R
  - This script computes correlations between the Relative Rigor Score (RRS) and traditional metrics (h-index, citation rate, FNCS, JIF, publication age, academic age of first author)
  - The analyses for Study 2 (lines 60-110) are reproducible.

To summarize, to reproduce our results for Study 2, run the script `04-Analysis_Study2.R` and the second half of script `05-Exploratory_analysis.R`.
