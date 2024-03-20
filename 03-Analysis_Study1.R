# This file contains all analyses from Study 1. Due to anonymity reasons, the
# scientific publications we received and the associated data sets cannot be published. 
# (Many of our participants were early career researchers currently (or soon) on the job market. 
# We promised not to share individual data in order to increase the response rate and to remove 
# potential risks for their application processes.)
# Hence, though not fully reproducible, this script serves the purpose to understand the steps 
# taken in the analysis.

# load packages
library(psych)
library(dplyr)
library(DescTools)
library(openxlsx)
library(tidyr)

# load data
load(file = "raw_data/Study1/Study1_res.Rdata") # final ratings of papers (agreed upon by consensus)
load(file = "raw_data/Study1/Study1_res2.Rdata") # ratings of papers from each of the 3 raters (for inter-rater reliability)
times <- read.xlsx("raw_data/Study1/Study1_papers_time.xlsx", "Times") #excel sheet with rating times

# check correlation between OpenAlex JIFs and Clarivate JIFs
JIFs <- read.csv("raw_data/Study1/JIF_Clarivate_OA.csv")
cor.test(JIFs$JIF_OA, JIFs$JIF_Clarivate)
ggplot(JIFs, aes(JIF_OA, JIF_Clarivate)) + geom_point() + geom_smooth(method=lm) + xlab("JIF from OpenAlex") + ylab("JIF from Clarivate") + ggtitle(label="", subtitle="n = 27 journals")

#-----------------------------------------------------------------------------------------------------------------------
# Descriptives: time for rating considering all contributions
times_all <- describe(c(times$t1F_all, times$t2F_all, times$t3F_all, times$t1M_all, times$t2M_all, times$t3M_all, times$t1G_all, times$t2G_all, times$t3G_all), na.rm = TRUE)
# Descriptives: time for rating considering only contributions that fit into scheme
times_r <- describe(c(times$t1F_r, times$t2F_r, times$t3F_r, times$t1M_r, times$t2M_r, times$t3M_r, times$t1G_r, times$t2G_r, times$t3G_r), na.rm = TRUE)

#-----------------------------------------------------------------------------------------------------------------------
# Check interrater reliability for each indicator

targets <- c("data_type", "has_open_data", "open_data_ZK", "OD_FAIR_doi", "OD_FAIR_codebook",
             "OD_FAIR_fileformat", "has_repro_check", "has_open_scripts", "OS_FAIR_repo",
             "OS_FAIR_git", "OS_FAIR_md", "OS_FAIR_env", "has_open_material", "has_prereg",
             "prereg_ssp", "prereg_hyp", "prereg_op", "prereg_ap", "formal_modeling", "prereg_replication")
kappa_results <- list()

for (target in targets) {
  mat <- res2 %>%
    filter(ID == target) %>%
    select(-filename) %>%
    pivot_wider(id_cols = c(author, doi, ID), names_from = rater, values_from = value)
  
  kappa_value <- KappaM(mat[, c("Franka", "Gracia", "Martin")], method = "Fleiss", conf.level = 0.95)
  
  kappa_results[[target]] <- kappa_value
}

#-----------------------------------------------------------------------------------------------------------------------
# Calculate variances for each indicator
targets <- c("data_type", "has_open_data", "open_data_ZK", "OD_FAIR_doi", "OD_FAIR_codebook",
             "OD_FAIR_fileformat", "has_repro_check", "has_open_scripts", "OS_FAIR_repo",
             "OS_FAIR_git", "OS_FAIR_md", "OS_FAIR_env", "has_open_material", "has_prereg",
             "prereg_ssp", "prereg_hyp", "prereg_op", "prereg_ap", "formal_modeling", "prereg_replication")
vars <- list()

for (target in targets) {
  var <- res %>%
    filter(ID == target) %>%
    select(-filename)
  
  vars_value <- var(res[res$ID == target, "points"], na.rm = TRUE)
  
  vars[[target]] <- vars_value
}

# Create table for IRR and variances of all indicators
table_complete <- data.frame(kappa_results) 
table_complete <- transpose(table_complete)
row.names(table_complete) <- targets
table_complete$V4 <- as.numeric(vars)

colnames(table_complete) <- c("kappa", "ci_low", "ci_high", "variance")

# Calculate correlation between fleiss kappa and variance
cor.test(table_complete$kappa, table_complete$variance)

# ICC for the 52 RRS scores
RRS_scores <- subset(res2, ID == 'rel_points')
RRS_scores$RRS <- as.numeric(RRS_scores$value)

RRS_wide_paper <- pivot_wider(RRS_scores %>% select(doi, rater, RRS), id_cols = "doi", names_from = "rater", values_from = "RRS")

psych::ICC(RRS_wide_paper[, -1])

# variance of the averaged RRS
RRS_avg <- subset(res, ID == 'rel_points')
var(RRS_avg$value, na.rm=TRUE) |> round(2)

# ICC for the 21 authors (i.e., aggregated across 3 papers)
RRS_wide_author <- RRS_scores %>% 
  group_by(author, rater) %>% 
  summarise(RRS_author = mean(RRS)) %>% 
  pivot_wider(id_cols = "author", names_from = "rater", values_from = "RRS_author")

psych::ICC(RRS_wide_author[, 2:4])

RRS_wide_author$RRS_avg <- rowMeans(RRS_wide_author[, 2:4])

var(RRS_wide_author$RRS_avg, na.rm=TRUE) |> round(2)
