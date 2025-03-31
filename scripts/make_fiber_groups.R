# title: make_fiber_groups
# author: Sarah Blecksmith
# purpose: Compute the lowest quartile fiber consumers and those
# consuming adequate fiber (>= 14 g/1000kcals) on both FQ and ASA24


library(tidyverse)

# Read in the FFQ variables, n = 359
ffq_fiber <- read.csv("data/fiber_FFQ_FL100.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
# make the FFQ per 1000kcal variable
ffq_fiber <- ffq_fiber %>% mutate(ffq_fiber_perKcal =  (dt_fibe / dt_kcal)*1000)

# Read in the ASA24 variables, n = 378
asa_fiber <- read.table("data/ASA24_average_fiber_summary_variables.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Merge the dataframes
all_fiber <- merge(x = asa_fiber, y = ffq_fiber, by = 'subject_id') %>% # n=351
  rename(asa_fiber_perKcal = perKcal_fiber_tnfs)

# Find the bottom quantile
ffq_quantile <- quantile(all_fiber$ffq_fiber_perKcal)[2] # 9.291580 
asa_quantile <- quantile(all_fiber$asa_fiber_perKcal)[2] # 7.863094


# the adequate fiber group has more than 14g per 1000 cal in both instruments
# low fiber will be the lowest quantile in both instruments
all_fiber$fiber_group = case_when(all_fiber$ffq_fiber_perKcal >= 14 & all_fiber$asa_fiber_perKcal >= 14 ~ "adequate_fiber",
                                  all_fiber$ffq_fiber_perKcal <= ffq_quantile & all_fiber$asa_fiber_perKcal <= asa_quantile ~ "low_fiber",
                                   TRUE ~ NA)


write.csv(all_fiber[, c("subject_id", "fiber_group", "asa_fiber_perKcal", "ffq_fiber_perKcal" )], "data/fiber_groups_all.csv", row.names = FALSE)

all_fiber <- all_fiber %>% drop_na(fiber_group)
write.csv(all_fiber[, c("subject_id", "fiber_group", "asa_fiber_perKcal", "ffq_fiber_perKcal" )], "data/fiber_groups.csv", row.names = FALSE)