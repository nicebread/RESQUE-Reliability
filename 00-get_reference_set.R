# This downloads a reference set for the field and age normalized citation score
# We use the concept ID for "Psychology"

library(OAmetrics)

# reference set: same year, same publication type, same field
# Observation: Due to the extremely skewed distribution, the mean citation gets 
# larger the larger the sample is (see also Schmoch, 2020)
c_counts_psy <- get_reference_set(years = 2023:2001, n_per_year=100000, type="article", concept.id=c("C15744967"), save_intermediate = "intermediate", seed=1234)

c_counts_psy <- get_reference_set_from_files("intermediate")

saveRDS(c_counts_psy, file="raw_data/c_counts_psy_2001_2023.RDS")
