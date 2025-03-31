# title: merge_FL100_variables
# author: Sarah Blecksmith, adapted from Stephanie Wilson
# purpose: Pool FL100 data into one file

library(tidyverse)



stool <- read.csv("data/FL100_stool_variables.txt", header = TRUE, sep = "\t") %>%
  rename('plasma_lbp' = 'plasma_lbp_bd1') 

# Faith's phylogenetic diversity for fiber in average grams of fiber per day
fiber_div  = read.csv("data/qiime1_alphadiv_fiber.txt", header = TRUE, sep = "\t") %>%
  rename('subject_id' = 'X',
         'faiths_diversity_fiber' = 'PD_whole_tree') %>%
  select(c(subject_id, faiths_diversity_fiber))

# Faith's phylogenetic diversity for total carbohydrate in grams of carbohydrate per food per day
carb_div  <- read.csv("data/qiime1_alphadiv_carb.txt", header = TRUE, sep = "\t") %>%
  rename('subject_id' = 'X',
        'faiths_diversity_carb' = 'PD_whole_tree') %>%
    select(c(subject_id, faiths_diversity_carb))

# Faith's phylogenetic diversity for whole diet in average grams of food per day
diet_div  <- read.csv("data/qiime1_alphadiv_filt.txt", header = TRUE, sep = "\t") %>%
  rename('subject_id' = 'X',
         'faiths_diversity_diet' = 'PD_whole_tree') %>%
  select(c(subject_id, faiths_diversity_diet))

ffq_fiber <- read.csv("data/fiber_FFQ_FL100.txt", sep = "\t", header = TRUE) %>%
  mutate(fibe_perKcal =  (dt_fibe / dt_kcal)*1000) %>%
  mutate(sol_fibe_perKcal = (dt_fiber_sol / dt_kcal)*1000)

asa_fiber <- read.table("data/ASA24_average_fiber_summary_variables.txt", sep = "\t", header = TRUE)

hei <- read.csv("data/CTSC24532USDAWHNRCNu_DATA_2022-12-15_1416.csv", header = TRUE) %>%
  rename("dii_totalscore" = "dii_total_score") %>%
  select(c(subject_id, hei_asa24_totalscore, hei_ffq_totalscore, dii_totalscore))

age_sex_bmi <- read.csv("data/FL100_FINAL_Bins_agesexbmi_clean.csv", header = TRUE)

activity <- read.table("data/QC_Actical_Data_trimmed.txt", header = TRUE) %>%
  select(-SUM)

bodyfat <- read.csv("data/CTSC24532USDAWHNRCNu-DEXA_DATA_2022-12-15_1408.csv", header = TRUE) %>%
  select(c(subject_id, bodyfat_pcnt))


merged = stool %>%
  left_join(fiber_div, by = "subject_id") %>%
  left_join(carb_div, by = "subject_id") %>%
  left_join(diet_div, by = "subject_id") %>%
  left_join(hei, by = 'subject_id') %>%
  left_join(ffq_fiber, by = 'subject_id') %>%
  left_join(asa_fiber, by = 'subject_id') %>%
  left_join(age_sex_bmi, by = 'subject_id') %>%
  left_join(activity, by = 'subject_id') %>%
  left_join(bodyfat, by = 'subject_id')
  
  
write.csv(merged, 'data/FL100_merged_variables.csv', row.names = FALSE)


#Filter out where AfterV2 = 0, we don't want to use the subjects who submitted stool after the test meal
merged_stool <- merged %>% filter(AfterV2 == 0) # now 363

# Filter out where stool sample not processed within 24 hours
merged_stool <-merged_stool %>% filter(diff_time_hrs < 24) # now 316
