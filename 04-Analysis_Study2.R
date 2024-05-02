# Analysis script by Anna Seyffert
# This script computes ICCs for all indicators and the overall relative rigor score (RRS) in Study 2

library(rio)
library(tidyverse)
library(psych)
library(irr)
library(psy)
library(Rfast)
library(Hmisc)
library(psych)
library(dplyr)

#3 import data
data0 <- import("raw_data/Study2/Data_Study_2.xlsx")
# define NAs (-1 <- option 'unclear')
rating_vars <- c("R003_01", "R003_02", "R003_03", "R003_04", "R003_05", "R003_06", "R003_07", "R003_08", "R003_09", "R003_10", "R003_11", "R003_12", "R003_13", "R003_14", "R003_15", "R003_16", "R003_17", "R003_18", "R003_19", "R003_20", "R003_21", "R003_22", "R003_23", "R003_24", "R003_25","R003_26","R003_27","R003_28", "R003_29", "R003_30", "R003_31", "R003_32", "R003_33", "R003_34", "R003_35", "R003_36", "R003_37", "R003_38")

consensus_vars <- c("R003_01", "R003_02", "R003_03", "R003_04", "R003_05", "R003_06", "R003_07", "R003_08", "R003_09", "R003_10", "R003_11", "R003_12", "R003_35", "R003_36", "R003_37")

MR_vars <- c("R003_13", "R003_14", "R003_15", "R003_16", "R003_17", "R003_18", "R003_19", "R003_20", "R003_21", "R003_22", "R003_23", "R003_24", "R003_25", "R003_26", "R003_27", "R003_28", "R003_29", "R003_30", "R003_31", "R003_32", "R003_33", "R003_34")

data0[, rating_vars][data0[, rating_vars] == -1] <- NA
#4 select needed variables
data <- data0 %>% select(R001, R002, R006, starts_with("R003_"))

#5 data exclusion: 8 studies were rated more then 3 times. Additional ratings get excluded because of k=3 (Case 147, 173, 228, 230, 238, 246, 314, 340, 341) + labels get removed (row 1)
data <- data[-c(1, 119, 140, 184, 186, 194, 200, 248, 271, 272),]

#6 convert to numeric vectors
data <- as.data.frame(apply(data, 2, as.numeric))

#7 calculate sums and mean for consensus criteria for each rating
data$sum_consensus <- rowSums(data %>% select(all_of(consensus_vars)), na.rm=T)
data$mean_consensus <- rowMeans(data %>% select(all_of(consensus_vars)), na.rm=T)

#8a calculate sums and mean for criteria of methodological rigor
data$sum_methods <- rowSums(data %>% select(all_of(MR_vars)), na.rm=T)
data$mean_methods <- rowMeans(data %>% select(all_of(MR_vars)), na.rm=T)

#8b: Compute relative rigor score RRS

# compute the maximally attainable score: each indicator has a maximum of 3 points. If it's NA, it is removed from the max score.
data2 <- data %>% select(all_of(MR_vars))

data$max_methods_score <- apply(data2, 1, function(x) sum(!is.na(x))*3)

data$RRS <- data$sum_methods / data$max_methods_score
hist(data$RRS)

#10 compress R002 and R006 into one column showing the paper number
data <- data %>% mutate(R006b = R006 + case_when(R006 == -1 ~ 0, R006 > -1 ~ 58))
data$R002[data$R002 == -1] <- NA
data$R006b[data$R006b == -1] <- NA
data$paper <- coalesce(data$R002, data$R006b)

save(data, file="processed_data/S2_data.Rdata")


#9 bring into needed format for analyses
data_long <- data %>% pivot_longer(cols=c(-R001, -R002, -R006), names_to="criterion") %>% rename(rater = R001)
data_long$criterion <- recode(data_long$criterion,
                              R003_38 = "c_0",
                              R003_01 = "c_1a",
                              R003_02 = "c_1b",
                              R003_03 = "c_1c",
                              R003_04 = "c_2a",
                              R003_05 = "c_2b",
                              R003_06 = "c_2c",
                              R003_07 = "c_3a",
                              R003_08 = "c_3b",
                              R003_09 = "c_3c",
                              R003_10 = "c_4a",
                              R003_11 = "c_4b",
                              R003_12 = "c_4c",
                              R003_13 = "c_5a",
                              R003_14 = "c_5b",
                              R003_15 = "c_5c",
                              R003_16 = "c_6a",
                              R003_17 = "c_6b",
                              R003_18 = "c_6c",
                              R003_19 = "c_6d",
                              R003_20 = "c_6e",
                              R003_21 = "c_6f",
                              R003_22 = "c_6g",
                              R003_23 = "c_7a",
                              R003_24 = "c_7b",
                              R003_25 = "c_7c",
                              R003_26 = "c_8a",
                              R003_27 = "c_8b",
                              R003_28 = "c_8c",
                              R003_29 = "c_8d",
                              R003_30 = "c_9a",
                              R003_31 = "c_9b",
                              R003_32 = "c_9c",
                              R003_33 = "c_9d",
                              R003_34 = "c_9e",
                              R003_35 = "c_10a",
                              R003_36 = "c_10b",
                              R003_37 = "c_10c",
                              mean_methods = "Overall_Score_Methods_Mean",
                              sum_methods = "Overall_Score_Methods_Sum",
                              mean_consensus ="Overall_Score_Consensus_Mean",
                              sum_consensus ="Overall_Score_Consensus_Sum"
)

#10 compress R002 and R006 into one column showing the paper number
data_long <- data_long %>% mutate(R006 = R006 + case_when(R006 == -1 ~ 0, R006 > -1 ~ 58))
data_long$R002[data_long$R002 == -1] <- NA
data_long$R006[data_long$R006 == -1] <- NA
data_long$paper <- coalesce(data_long$R002, data_long$R006)
data_long <- data_long %>% select(-R002, -R006)

res1 <- data.frame()

#MAIN ANALYSIS
#11 compute ICC
for (i in unique(data_long$criterion)) {
  print(i)
  data_wide <- data_long[data_long$criterion == i, ] %>% 
    pivot_wider(id_cols=c(paper), names_from=rater, values_from = value) %>% 
    select(-paper) 
  
  icc <- psych::ICC(data_wide, missing=FALSE)
  
  kripp <- kripp.alpha(t(as.matrix(data_wide)), method="ordinal")
  
  res1 <- rbind(res1, data.frame(
    criterion = i,
    ICC_1_1 = icc$results$ICC[1],
    CI_Lower_1_1 = icc$results$`lower bound`[1],
    CI_Upper_1_1 = icc$results$`upper bound`[1],
    kripp.alpha = kripp$value
  ))
}

#12 calculate ICC (1,3) an 95% CI - ICC_1_3 = (3*ICC1_1)/(1+(3-1)*ICC_1_1)
k=3
res1$ICC_1_3 <- ((k*res1[,2])/(1+(k-1)*res1[,2]))
res1$ICC_1_3_CI_Lower <- ((3*res1[,3])/(1+(3-1)*res1[,3]))
res1$ICC_1_3_CI_Upper <- ((3*res1[,4])/(1+(3-1)*res1[,4]))
res1

#13 mean, SD, minimum, maximum of ICC for methodological criteria 
method_mean_1_1 <- mean(res1[14:35,2])
SD_1_1 <- sd(res1[14:35,2])
Min_1_1 <- min(res1[14:35,2])
Max_1_1 <- max(res1[14:35,2])
method_mean_1_3 <- mean(res1[14:35,6])
SD_1_3 <- sd(res1[14:35,6])
Min_1_3 <- min(res1[14:35,6])
Max_1_3 <- max(res1[14:35,6])

method_mean <- data.frame(method_mean_1_1, SD_1_1, Min_1_1, Max_1_1, method_mean_1_3, SD_1_3, Min_1_3, Max_1_3)
method_mean <- round(method_mean, 2)
method_mean

#14 mean, SD, minimum, maximum of ICC for consensus criteria 
consensus_mean_1_1 <- mean(res1[c(2:13, 36:38),2])
C_SD_1_1 <- sd(res1[c(2:13, 36:38),2])
C_Min_1_1 <- min(res1[c(2:13, 36:38),2])
C_Max_1_1 <- max(res1[c(2:13, 36:38),2])
consensus_mean_1_3 <- mean(res1[c(2:13, 36:38),6])
C_SD_1_3 <- sd(res1[c(2:13, 36:38),6])
C_Min_1_3 <- min(res1[c(2:13, 36:38),6])
C_Max_1_3 <- max(res1[c(2:13, 36:38),6])

consensus_mean <- data.frame(consensus_mean_1_1, C_SD_1_1, C_Min_1_1, C_Max_1_1, consensus_mean_1_3, C_SD_1_3, C_Min_1_3, C_Max_1_3)
consensus_mean <- round(consensus_mean, 2)
consensus_mean

#15 quantiles
data_long$value <- as.numeric(data_long$value)
quantiles <- data_long %>%
  group_by(criterion, paper) %>%
  summarise(value = mean(value, na.rm=TRUE)) %>%
  group_by(criterion) %>%
  summarise(
    q25 = quantile(value, prob=.25, na.rm=TRUE),
    q50 = quantile(value, prob=.50, na.rm=TRUE),
    q75 = quantile(value, prob=.75, na.rm=TRUE)
  )
quantiles[,2:4] <- round(quantiles[,2:4], 2)
quantiles

#EXPLORATORY ANALYSES


#17 correlation matrix for all criteria
data_corr <- data
data_corr <- data_corr %>% select(c(-R001, -R002, -R006, -sum_consensus, -sum_methods, -mean_consensus, -mean_methods))
data_corr <- rename(data_corr,
                    "0" = R003_38,
                    "1a" = R003_01,
                    "1b" = R003_02,
                    "1c" = R003_03,
                    "2a" = R003_04,
                    "2b" = R003_05,
                    "2c" = R003_06,
                    "3a" = R003_07,
                    "3b" = R003_08,
                    "3c" = R003_09,
                    "4a" = R003_10,
                    "4b" = R003_11,
                    "4c" = R003_12,
                    "5a" = R003_13,
                    "5b" = R003_14,
                    "5c" = R003_15,
                    "6a" = R003_16,
                    "6b" = R003_17,
                    "6c" = R003_18,
                    "6d" = R003_19,
                    "6e" = R003_20,
                    "6f" = R003_21,
                    "6g" = R003_22,
                    "7a" = R003_23,
                    "7b" = R003_24,
                    "7c" = R003_25,
                    "8a" = R003_26,
                    "8b" = R003_27,
                    "8c" = R003_28,
                    "8d" = R003_29,
                    "9a" = R003_30,
                    "9b" = R003_31,
                    "9c" = R003_32,
                    "9d" = R003_33,
                    "9e" = R003_34,
                    "10a" = R003_35,
                    "10b" = R003_36,
                    "10c" = R003_37
)
data_corr <- as.matrix(data_corr)
corr_matrix <- rcorr(data_corr, type = "pearson")
corr_matrix_r <- as.matrix(corr_matrix$r)[1:38, 1:38]
diag(corr_matrix_r) <- res1[1:38,5]

#18 preparation for exploratory analysis of ICC only for articles with no further selection
data1 <- import("raw_data/Study2/Data_Study_2_exploratory.xlsx")
data1[, rating_vars][data1[, rating_vars] == -1] <- NA
data1 <- data1[-1,]
data_a <- data1 %>% select(R001, R002, R006, starts_with("R003_"))
data_long_a <- data_a %>% pivot_longer(cols=c(-R001, -R002, -R006), names_to="criterion") %>% rename(rater = R001)

data_long_a$criterion <- recode(data_long_a$criterion,
                                R003_38 = "c_0",
                                R003_01 = "c_1a",
                                R003_02 = "c_1b",
                                R003_03 = "c_1c",
                                R003_04 = "c_2a",
                                R003_05 = "c_2b",
                                R003_06 = "c_2c",
                                R003_07 = "c_3a",
                                R003_08 = "c_3b",
                                R003_09 = "c_3c",
                                R003_10 = "c_4a",
                                R003_11 = "c_4b",
                                R003_12 = "c_4c",
                                R003_13 = "c_5a",
                                R003_14 = "c_5b",
                                R003_15 = "c_5c",
                                R003_16 = "c_6a",
                                R003_17 = "c_6b",
                                R003_18 = "c_6c",
                                R003_19 = "c_6d",
                                R003_20 = "c_6e",
                                R003_21 = "c_6f",
                                R003_22 = "c_6g",
                                R003_23 = "c_7a",
                                R003_24 = "c_7b",
                                R003_25 = "c_7c",
                                R003_26 = "c_8a",
                                R003_27 = "c_8b",
                                R003_28 = "c_8c",
                                R003_29 = "c_8d",
                                R003_30 = "c_9a",
                                R003_31 = "c_9b",
                                R003_32 = "c_9c",
                                R003_33 = "c_9d",
                                R003_34 = "c_9e",
                                R003_35 = "c_10a",
                                R003_36 = "c_10b",
                                R003_37 = "c_10c"
)

data_long_a$R002 <- as.numeric(data_long_a$R002)
data_long_a$R006 <- as.numeric(data_long_a$R006)
data_long_a <- data_long_a %>% mutate(R006 = R006 + case_when(R006 == -1 ~ 0, R006 > -1 ~ 58))
data_long_a$R002[data_long_a$R002 == -1] <- NA
data_long_a$R006[data_long_a$R006 == -1] <- NA
data_long_a$paper <- coalesce(data_long_a$R002, data_long_a$R006)
data_long_a <- data_long_a %>% select(-R002, -R006)

# merge the information of MR_eligible
S2 <- import(file="processed_data/S2.csv")
data_long_b <- merge(data_long_a, S2 %>% select(paper_number, is_MR_eligible), by.x="paper", by.y="paper_number")

#19 quantiles of part 2 of the sample
data_long_a$value <- as.numeric(data_long_a$value)
quantiles_a <- data_long_a %>%
  group_by(criterion, paper) %>%
  summarise(value = mean(value, na.rm=TRUE)) %>%
  group_by(criterion) %>%
  summarise(
    q25 = quantile(value, prob=.25, na.rm=TRUE),
    q50 = quantile(value, prob=.50, na.rm=TRUE),
    q75 = quantile(value, prob=.75, na.rm=TRUE)
  )
quantiles_a[,2:4] <- round(quantiles_a[,2:4], 2)
quantiles_a

#20 frequency of "3" for all criteria in random sample
frequency <- data_long_b %>% 
  arrange(criterion) %>% 
  group_by(criterion) %>% 
  summarise(
    three = sum(value == 3, na.rm=TRUE)/sum(!is.na(value)) * 100
  )
  
frequency$three <- round(frequency$three, 1)
print(frequency, n=100)

# without criterion 0 (“Published in peer reviewed journal”)
mean(frequency$three[-1])
sd(frequency$three[-1])
range(frequency$three[-1])

# How many of the 30 papers are MR_eligible? 22
 data_long_b %>% 
  filter(is_MR_eligible == TRUE) %>% 
  pull(paper) |> unique() |> length()

frequency_eligible <- data_long_b %>% 
  filter(is_MR_eligible == TRUE) %>% 
  arrange(criterion) %>% 
  group_by(criterion) %>% 
  summarise(
    three_eligible = sum(value == 3, na.rm=TRUE)/sum(!is.na(value)) * 100
  )
  
frequency_eligible$three_eligible <- round(frequency_eligible$three_eligible, 1)
print(frequency_eligible, n=100)

mean(frequency_eligible$three_eligible[-1])
sd(frequency_eligible$three_eligible[-1])
range(frequency_eligible$three_eligible[-1])

usage <- inner_join(frequency, frequency_eligible, by="criterion")

#21 compute ICC of part 2 of the sample
res2 <- data.frame()

for (i in unique(data_long_a$criterion)) {
  print(i)
  data_wide_a <- data_long_a[data_long_a$criterion == i, ] %>% 
    pivot_wider(id_cols=c(paper), names_from=rater, values_from = value) %>% 
    select(-paper) 
  data_wide_a <- as.data.frame(apply(data_wide_a, 2, as.numeric))
  
  icc_a <- psych::ICC(data_wide_a, missing=FALSE)
  
  res2 <- rbind(res2, data.frame(
    criterion = i,
    ICC_1_1 = icc_a$results$ICC[1],
    CI_Lower_1_1 = icc_a$results$`lower bound`[1],
    CI_Upper_1_1 = icc_a$results$`upper bound`[1]
  ))
}
res2

#22 ICC(1,3) for part 2
res2$ICC_1_3 <- ((k*res2[,2])/(1+(k-1)*res2[,2]))
res2

#23 mean, SD, minimum, maximum for methodological criteria in part 2
method_mean_1_1a <- mean(res2[14:35,2])
SD_1_1a <- sd(res2[14:35,2])
Min_1_1a <- min(res2[14:35,2])
Max_1_1a <- max(res2[14:35,2])
method_mean_1_3a <- mean(res2[14:35,5])
SD_1_3a <- sd(res2[14:35,5])
Min_1_3a <- min(res2[14:35,5])
Max_1_3a <- max(res2[14:35,5])

method_meana <- data.frame(method_mean_1_1a, SD_1_1a, Min_1_1a, Max_1_1a, method_mean_1_3a, SD_1_3a, Min_1_3a, Max_1_3a)
method_meana <- round(method_meana, 2)
method_meana

#24 mean, SD, minimum, maximum for consensus criteria in part 2
consensus_mean_1_1a <- mean(res2[c(2:13, 36:38),2])
C_SD_1_1a <- sd(res2[c(2:13, 36:38),2])
C_Min_1_1a <- min(res2[c(2:13, 36:38),2])
C_Max_1_1a <- max(res2[c(2:13, 36:38),2])
consensus_mean_1_3a <- mean(res2[c(2:13, 36:38),5])
C_SD_1_3a <- sd(res2[c(2:13, 36:38),5])
C_Min_1_3a <- min(res2[c(2:13, 36:38),5])
C_Max_1_3a <- max(res2[c(2:13, 36:38),5])

consensus_meana <- data.frame(consensus_mean_1_1a, C_SD_1_1a, C_Min_1_1a, C_Max_1_1a, consensus_mean_1_3a, C_SD_1_3a, C_Min_1_3a, C_Max_1_3a)
consensus_meana <- round(consensus_meana, 2)
consensus_meana


#26 mean time rating
data_time <- data0 %>% select(R001, R004_01)
data_time <- data_time[-c(1, 119, 140, 184, 186, 194, 200, 248, 271, 272),]
data_time <- as.data.frame(apply(data_time, 2, as.numeric))
mean(data_time$R004_01)
sd(data_time$R004_01)
min(data_time$R004_01)
max(data_time$R004_01)

# rating time results for table 5
aggregate(data_time$R004_01, list(data_time$R001), FUN=mean)
aggregate(data_time$R004_01, list(data_time$R001), FUN=sd)
data_time %>% count(R001)

#27 round ICC results for presentation
res1[,2:8] <- round(res1[, 2:8],2)
res1

# TODO: Compute ICC for "Overall Score of Methodology Criteria", but only for the 89 eligible papers.