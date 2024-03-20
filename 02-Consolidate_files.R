# Script by Felix Schönbrodt
# This script combines multiple data sources: The h-indexes, FNCS etc. retrieved from OpenAlex, the manual h-indexes from Scopus, and more.

# Note: Without the data from Study 1, this script is NOT reproducible.

library(rio)
library(dplyr)
library(stringr)
library(ggplot2)

# load all dois from both studies
load(file="raw_data/all_dois.RData")
load("raw_data/Study1/Study1_res.Rdata")

dois_Study2 <- import("raw_data/Study2/doi_paper_number_normalized.xlsx")

dat1 <- import("raw_data/traditional_metrics_R1.xlsx")
dat2 <- import("raw_data/traditional_metrics_R2.xlsx")

colnames(dat1)[12] <- "h_index_first_au_SCOPUS.R1"
dat1$Rater2 <- dat2$Rater
dat1$h_index_first_au_SCOPUS.R2 <- dat2$h_index_first_au_SCOPUS

# any missing values?
table(is.na(dat1$h_index_first_au_SCOPUS.R1))
table(is.na(dat1$h_index_first_au_SCOPUS.R2))

#-----------------------------------
# Interrater agreement
#-----------------------------------

library(psych)

# reduce to unique authors (i.e., remove double entries of the same person)
ICC_dat <- dat1 %>% 
    group_by(first_au_id) %>% 
    slice(1) %>% 
    select(contains("h_index_first_au"))

# How many ratings are identical?
table(ICC_dat$h_index_first_au_SCOPUS.R1 == ICC_dat$h_index_first_au_SCOPUS.R2)

# Which ratings differ? --> need to be consolidated in a discussion.
# The consensus ratings are stored in file `raw_data/traditional_metrics_R1_R2.xlsx` (not shared due to privacy reasons)
diff <- which(ICC_dat$h_index_first_au_SCOPUS.R1 != ICC_dat$h_index_first_au_SCOPUS.R2)
ICC_dat[diff, c("h_index_first_au_SCOPUS.R1", "h_index_first_au_SCOPUS.R2")]

# compute the ICC (before consensus rating)
ICC(ICC_dat[, c("h_index_first_au_SCOPUS.R1", "h_index_first_au_SCOPUS.R2")])


#-----------------------------------
# Define other variables
#-----------------------------------

# load the consolidated h-index
dat <- import("raw_data/traditional_metrics_R1_R2.xlsx")

# How good are the automatic OpenAlex h-indexes? r = .98
# First, reduce data set to unique authors (as some authors are in the data set multiple times)
dat_unique <- dat %>% 
    select(first_au_id, h_index_first_au, h_index_first_au_SCOPUS) %>% 
    distinct()

export(dat_unique, file="processed_data/h_index_comparison.csv")

c1 <- cor.test(dat_unique$h_index_first_au, dat_unique$h_index_first_au_SCOPUS, use="p")
c1

ggplot(dat_unique, aes(h_index_first_au, h_index_first_au_SCOPUS)) + geom_point() + geom_smooth(method=lm) + xlab("h-index OpenAlex")+ ylab("h-index Scopus") + ggtitle(paste0("Correlation: r = ", c1$estimate |> round(2)))


dat$year_of_phd_first <- as.numeric(dat$year_of_phd_first)

dat$is_MR_eligible <- dat$is_MR_eligible == 1
dat$is_MR_eligible[is.na(dat$is_MR_eligible)] <- FALSE

dat$type <- factor(dat$type == 1, levels=c(TRUE, FALSE), labels=c("empirical", "theoretical"))


# read indexes (h-index, FNCS etc.) from all papers
dat2 <- readRDS(file="processed_data/results.RDS")

indexes <- left_join(
    dat2 %>% select(1:10, 22, 23), 
    dat %>% select(doi, year_of_phd_first, is_MR_eligible, type, h_index_first_au_SCOPUS), by="doi")

indexes$is_S1 <- indexes$doi %in% unique(res$doi)
indexes$is_S2 <- indexes$doi %in% dois_Study2$doi

table(S1=indexes$is_S1, S2=indexes$is_S2, useNA="always")

table(eligible = indexes[indexes$is_S2, ]$is_MR_eligible, indexes[indexes$is_S2, ]$type, useNA="always")

# Plausibility check: Which papers are neither assigned to S1 or S2? None!
indexes[!indexes$is_S1 & !indexes$is_S2, "doi"]

#-----------------------------------
# get BIP citation metrics (not reported in paper)
#-----------------------------------

BIP <- get_BIP(indexes$doi)
indexes <- cbind(indexes, BIP[, c("attrank", "pagerank")])

#-----------------------------------
# Get Relative Rigor Score (RRS) from both studies
#-----------------------------------

# the res.RData from Study 1 already contains the consensus ratings (so it's only a single rater)
S1 <- subset(res, ID == 'rel_points')
S1$RRS <- as.numeric(S1$value)

# Study 2
load(file="raw_data/Study2/data.Rdata")
S2_long <- data

# average the three ratings per paper:
S2 <- S2_long %>% 
    select(paper, RRS) %>% 
    group_by(paper) %>% 
    summarise(RRS = mean(RRS)) %>% 
    arrange(paper)
hist(S2$RRS)

S2$doi <- dois_Study2$doi

S1 <- merge(S1 %>% select(doi, RRS), indexes, by="doi")
S2 <- merge(S2 %>% select(doi, RRS), indexes, by="doi")

export(indexes, file="processed_data/indexes.csv")
export(S1, file="processed_data/S1.csv")
export(S2, file="processed_data/S2.csv")

