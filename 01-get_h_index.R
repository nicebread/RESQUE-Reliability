# Script by Felix Schönbrodt
# This script loads DOIs from two studies, fetches corresponding papers from OpenAlex, calculates the Journal Impact Factor (JIF) and h-index for each paper, and computes Field Normalized Citation Ratio (FNCR) and Field Normalized Paper Ratio (FNPR) for each paper. The script then exports the results.

# Note: Without the data from Study 1, this script is NOT reproducible.

library(OAmetrics)
library(openalexR)
library(dplyr)
library(rio)
library(ggplot2)

# load all dois from both studies
load("raw_data/Study1/Study1_res.Rdata")
dois_Study2 <- import("raw_data/Study2/doi_paper_number_normalized.xlsx")

# 161 unique dois: 52 from Study 1, 110 from Study 2, but 1 is overlapping
dois <- unique(c(res$doi, dois_Study2$doi))
save(dois, file="raw_data/all_dois.RData")

# load the reference set for the FNCS computation
c_counts_psy_2001_2023 <- readRDS(file="raw_data/c_counts_psy_2001_2023.RDS")

# retrieve all papers from OpenAlex. Some will not be retrieved (e.g. preprints from OSF)
papers <- oa_fetch(entity = "works", doi = dois, verbose=TRUE)

# which are missing? setdiff = elements in the first vector which are not in the second vector
setdiff(papers$doi, dois)
setdiff(dois, papers$doi)

# manually fix a wrong author assignment from OpenAlex
for (i in 1:nrow(papers)) {
    papers$author[i][[1]][papers$author[i][[1]]$au_id == "https://openalex.org/A5052911565", "au_id"] <- "https://openalex.org/A5056609792"    
}


#---------------------------------------------
# Get JIFs for each paper (from publication year)
# Get h-Index for first and last author
#---------------------------------------------

papers$JIF_OA <- NA
papers$two_yr_mean_citedness <- NA

papers$first_au_id <- NA
papers$first_au_name <- NA
papers$h_index_first_au <- NA
papers$academic_age_first_au <- NA
papers$first_au_first_work <- NA
papers$first_au_first_work_so <- NA
papers$first_au_first_work_concept <- NA

papers$last_au_id <- NA
papers$last_au_name <- NA
papers$h_index_last_au <- NA
papers$academic_age_last_au <- NA
papers$last_au_first_work <- NA
papers$last_au_first_work_so <- NA
papers$last_au_first_work_concept <- NA


for (i in 1:nrow(papers)) {

    # compute JIF "manually"
    if (!is.na(papers$issn_l[i])) {
        print(paste0(i, "/",  nrow(papers), ": Retrieving JIF for ", papers$so[i], " (", papers$publication_year[i], ")"))
    
        JIF <- tryCatch({get_JIF(issn=papers$issn_l[i], year=papers$publication_year[i], verbose=FALSE)},
        error = function(e) {
            #what should be done in case of exeption?
            print("Could not retrieve JIF, returning NA")
            return(list(JIF=NA))
        })
        papers$JIF_OA[i] <- round(JIF$JIF, 1)
  }

    # get h-index first author
    au_id <- papers[i, ]$author[[1]][1, "au_id"]
    print(paste0(i, "/",  nrow(papers), ": Retrieving h-index for first author ", papers[i, ]$author[[1]][1, "au_display_name"], " (", papers$publication_year[i], ")"))
    
    first_author <- oa_fetch("authors", identifier = papers[i, ]$author[[1]][1, "au_id"])

    hi <- h_index(author.id = au_id)
    papers$first_au_id[i]  <- au_id
    papers$first_au_name[i]  <- papers[i, ]$author[[1]][1, "au_display_name"]
    papers$h_index_first_au[i] <- hi$h_index

    papers$first_au_first_work[i] <- hi$first_publication$display_name
    papers$first_au_first_work_so[i] <- hi$first_publication$so
    co <- hi$first_publication$concepts[[1]]
    papers$first_au_first_work_concept[i] <- co %>% filter(level==0) %>% slice(1) %>% pull("display_name")

}


#---------------------------------------------
# Get FNCR and FNPR for each paper (from publication year)
#---------------------------------------------

# Get paper level metrics
paper_metrics <- FNCS(papers=papers, dois=NULL, ref_set = c_counts_psy_2001_2023, upper_trim=0)

papers2 <- inner_join(papers, paper_metrics %>% select(doi, FNCS, FNPR), by="doi")

indexes <- papers2 %>% 
    select(doi, id, display_name, publication_year, so, cited_by_count, JIF_OA,
            contains("first_au"), contains("last_au"), 
            FNCS, FNPR)


saveRDS(indexes, file="processed_data/results.RDS")
saveRDS(papers2, file="processed_data/papers.RDS")
