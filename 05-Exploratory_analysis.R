# Script by Felix Schönbrodt
# This script computes correlations between the Relative Rigor Score (RRS) and traditional metrics (h-index, citation rate, FNCS, JIF, publication age, academic age of first author)

# Note: The analyses for Study 2 (lines 60-110) are reproducible.

library(rio)
library(officer)
library(flextable)
library(scipub)
library(robustbase)
library(dplyr)

#--------------------------------------------------------
# Study 1 (not reproducible as we could not share the data)
#--------------------------------------------------------

S1 <- import(file="processed_data/S1.csv")
S1$publication_age <- 2024-S1$publication_year
S1$academic_age_first_au <- 2024-S1$year_of_phd_first

# the "year of PhD of the first author" was coded as 9999 if they were still in the process of doing their PhD
# Their academic age is set to zero:
S1$academic_age_first_au[S1$year_of_phd_first == 9999] <- 0

S1.sel <- S1[, c("RRS", "cited_by_count", "FNCS", "JIF_OA", "publication_age", "h_index_first_au", "academic_age_first_au")] 
S1.tab <- correltable(
  S1.sel, method = c("spearman"), use = c("pairwise"),
  tri = "lower", colnum = TRUE, round_n = 2)

S1.tab2 <- cbind(
  rownames(S1.tab$table),
  round(apply(S1.sel, 2, mean, na.rm=TRUE), 2),
  round(apply(S1.sel, 2, sd, na.rm=TRUE), 2),
  round(apply(S1.sel, 2, min, na.rm=TRUE), 2),
  round(apply(S1.sel, 2, max, na.rm=TRUE), 2),
  S1.tab$table
)
rownames(S1.tab2) <- NULL
colnames(S1.tab2) <- c("", "Mean", "SD", "Min", "Max", as.character(1:7))
S1.tab2[, 1] <- c("1. RRS", "2. Citation count", "3. FNCS", "4. JIF", "5. Publication age", "6. h-index", "7. Academic age")
S1.tab2 <- S1.tab2[, 1:11]

# Export to docx (can be copied and pasted from the viewer pane)
S1.tab2.docx <- S1.tab2 %>% as.data.frame() %>% regulartable() %>% autofit()

# Exploratory analysis (not reported in the paper): Do a robust multiple regression

S1$RRS.z <- scale(S1$RRS)
S1$h_index_first_au.z <- scale(S1$h_index_first_au)
S1$academic_age_first_au.z <- scale(S1$academic_age_first_au)
S1$publication_age.z <- scale(S1$publication_age)
S1$cited_by_count.z <- scale(S1$cited_by_count)

l1a <- lmrob(RRS.z~h_index_first_au.z + academic_age_first_au.z, S1)
summary(l1a)

l1b <- lmrob(RRS.z~h_index_first_au.z + publication_age, S1)
summary(l1b)

#--------------------------------------------------------
# Study 2 (reproducible)
#--------------------------------------------------------
S2 <- import(file="processed_data/S2.csv")
S2$publication_age <- 2024-S2$publication_year
S2$academic_age_first_au <- 2024-S2$year_of_phd_first
S2$academic_age_first_au[S2$year_of_phd_first == 9999] <- 0

# Not all studys from S2 were eligible for the Methodological Rigor Score (e.g., because they were purely theoretical)
table(S2$is_MR_eligible)
S2.MR <- S2[S2$is_MR_eligible == TRUE, ]

# Study 2
S2.sel <- S2.MR[, c("RRS", "cited_by_count", "FNCS", "JIF_OA", "publication_age", "h_index_first_au", "academic_age_first_au")] 
S2.tab <- correltable(
  S2.sel, method = c("spearman"), use = c("pairwise"),
  tri = "lower", colnum = TRUE, round_n = 2)

S2.tab2 <- cbind(
  rownames(S2.tab$table),
  round(apply(S2.sel, 2, mean, na.rm=TRUE), 2),
  round(apply(S2.sel, 2, sd, na.rm=TRUE), 2),
  round(apply(S2.sel, 2, min, na.rm=TRUE), 2),
  round(apply(S2.sel, 2, max, na.rm=TRUE), 2),
  S2.tab$table
)
rownames(S2.tab2) <- NULL
colnames(S2.tab2) <- c("", "Mean", "SD", "Min", "Max", as.character(1:7))
S2.tab2[, 1] <- c("1. RRS", "2. Citation count", "3. FNCS", "4. JIF", "5. Publication age", "6. h-index", "7. Academic age")
S2.tab2 <- S2.tab2[, 1:11]

# Export to docx (can be copied and pasted from the viewer pane)
S2.tab2.docx <- S2.tab2 %>% as.data.frame() %>% regulartable() %>% autofit()
S2.tab2.docx


# Exploratory analysis (not reported in the paper): Do a robust multiple regression

S2.MR$RRS.z <- scale(S2.MR$RRS)
S2.MR$h_index_first_au.z <- scale(S2.MR$h_index_first_au)
S2.MR$academic_age_first_au.z <- scale(S2.MR$academic_age_first_au)
S2.MR$publication_age.z <- scale(S2.MR$publication_age)
S2.MR$cited_by_count.z <- scale(S2.MR$cited_by_count)

l2a <- lmrob(RRS.z~h_index_first_au.z + academic_age_first_au.z, S2.MR)
summary(l2a)

l2b <- lmrob(RRS.z~h_index_first_au.z + publication_age, S2.MR)
summary(l2b)

#--------------------------------------------------------
# descriptive stats (across both studies; not reproducible due to sensitive data)
# --------------------------------------------------------

indexes <- import(file="processed_data/indexes.csv")
table(indexes$year_of_phd_first == 9999)
table(is.na(indexes$year_of_phd_first))
