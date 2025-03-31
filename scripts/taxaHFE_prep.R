# title: taxaHFE_prep
# author: Sarah Blecksmith
# purpose: Generate files of quartiles of plant cazyme abundance, Shannon diversity, chao1 diversity 
# and muc2plant plus covariates to run for taxaHFE
# clean up food tree file


library(dplyr)

# load the dietary and microbiome data
FL100_data <- read.csv("data/FL100_merged_variables.csv", header = TRUE) 
microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE) 
food_tree <- read.table("data/fl100_otu_abundance.txt", header = TRUE, sep="\t", check.names = FALSE) %>%
  rename(clade_name = taxonomy) %>%
  select(-c(FoodCode))
food_tree$clade_name <- gsub(";", "|", food_tree$clade_name)
write.csv(food_tree, 'data/fl100_otu_abundance_clean.csv', row.names = FALSE)

# merge the data
merged = FL100_data %>%
  right_join(microbiome_data, by = "subject_id") %>%
  mutate(sex = factor(sex)) %>%
  mutate(frank_inflammation = ifelse(fecal_calprotectin >= 100, 1, 0)) %>%
  mutate(stoolc_soft = ifelse(StoolConsistencyClass == "soft", 1, 0)) %>%
  mutate(stoolc_normal = ifelse(StoolConsistencyClass == "normal", 1, 0)) %>%
  mutate(stoolc_hard = ifelse(StoolConsistencyClass == "hard", 1, 0)) 


abundance <- merged %>%  
  filter(plant_cazyme_abundance_quartile == 1 | plant_cazyme_abundance_quartile == 4) %>%
  mutate(feature_of_interest = ifelse(plant_cazyme_abundance_quartile == 1, "bottom_quartile", "top_quartile")) %>%
  select(c(subject_id, feature_of_interest, age, sex, bmi_final, st_wt, stoolc_soft, stoolc_normal, stoolc_hard, frank_inflammation))
write.csv(abundance, 'data/taxaHFE_quartile_abundance.csv', row.names = FALSE)

shannon <- merged %>% 
  filter(Shannon_quartile == 1 | Shannon_quartile == 4) %>%
  mutate(feature_of_interest = ifelse(Shannon_quartile == 1, "bottom_quartile", "top_quartile")) %>%
  select(c(subject_id, feature_of_interest, age, sex, bmi_final, st_wt, stoolc_soft, stoolc_normal, stoolc_hard, frank_inflammation))
write.csv(shannon, 'data/taxaHFE_quartile_shannon.csv', row.names = FALSE)

chao1 <- merged %>% 
  filter(Chao1_quartile == 1 | Chao1_quartile == 4) %>%
  mutate(feature_of_interest = ifelse(Chao1_quartile == 1, "bottom_quartile", "top_quartile")) %>%
  select(c(subject_id, feature_of_interest, age, sex, bmi_final, st_wt, stoolc_soft, stoolc_normal, stoolc_hard, frank_inflammation))
write.csv(chao1, 'data/taxaHFE_quartile_chao1.csv', row.names = FALSE)

muc2plant <- merged %>%  
  filter(muc2plantGHPL_quartile == 1 | muc2plantGHPL_quartile == 4) %>%
  mutate(feature_of_interest = ifelse(muc2plantGHPL_quartile == 1, "bottom_quartile", "top_quartile")) %>%
  select(c(subject_id, feature_of_interest, age, sex, bmi_final, st_wt, stoolc_soft, stoolc_normal, stoolc_hard, frank_inflammation))
write.csv(muc2plant, 'data/taxaHFE_quartile_muc2plant.csv', row.names = FALSE)

