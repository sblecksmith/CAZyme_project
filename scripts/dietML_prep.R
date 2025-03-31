# title: dietML_prep
# author: Sarah Blecksmith
# purpose: Prepare data for running dietML


library(dplyr)


ffq <- read.csv("data/FL100_FFQ_cleaned_all_dt.csv", header = TRUE)
asa <- read.csv("data/FL100_ASA24_avgs_cleaned_all_dt.csv", header = TRUE)

activity <- read.table("data/QC_Actical_Data_trimmed.txt", header = TRUE) %>%
  select(-SUM)
write.csv(activity, 'data/activity.csv', row.names = FALSE)

bodyfat <- read.csv("data/CTSC24532USDAWHNRCNu-DEXA_DATA_2022-12-15_1408.csv", header = TRUE) %>%
  select(c(subject_id, bodyfat_pcnt)) %>%
  filter(!is.na(bodyfat_pcnt))
write.csv(bodyfat, 'data/bodyfat_percentage.csv', row.names = FALSE)

hei <- read.csv("data/CTSC24532USDAWHNRCNu_DATA_2022-12-15_1416.csv", header = TRUE)
mono_div <- read.csv("data/non_glucose_shannon.csv", header = TRUE)

abundance <- read.csv("data/taxaHFE_quartile_abundance.csv", header = TRUE) %>%
  rename(abundance = feature_of_interest)
write.csv(abundance, 'data/dietML_quartile_abundance.csv', row.names = FALSE)
  
chao1 <- read.csv("data/taxaHFE_quartile_chao1.csv", header = TRUE) %>%
  rename(chao1 = feature_of_interest)
write.csv(chao1, 'data/dietML_quartile_chao1.csv', row.names = FALSE)
  
shannon <- read.csv("data/taxaHFE_quartile_shannon.csv", header = TRUE) %>%
  rename(shannon = feature_of_interest)
write.csv(shannon, 'data/dietML_quartile_shannon.csv', row.names = FALSE)
  
muc2plant <- read.csv("data/taxaHFE_quartile_muc2plant.csv", header = TRUE) %>%
  rename(muc2plant = feature_of_interest)
write.csv(muc2plant, 'data/dietML_quartile_muc2plant.csv', row.names = FALSE)
  
# make file of dii variables
dii_hei <- select(hei, c(subject_id, starts_with("dii")))
write.csv(dii_hei, 'data/dii_data.csv', row.names = FALSE)

# add the hei variables to the FFQ and ASA24 data
ffq_hei <- select(hei, c(subject_id, starts_with("hei_ffq")))
asa_hei <- select(hei, c(subject_id, starts_with("hei_asa24")))

merged_asa = asa %>%
  left_join(asa_hei, by = "subject_id")
write.csv(merged_asa, 'data/asa24_data.csv', row.names = FALSE)

merged_ffq = ffq %>%
  left_join(ffq_hei, by = "subject_id") 
write.csv(merged_ffq, 'data/ffq_data.csv', row.names = FALSE)

# bodyfat files need to be trimmed to just relevant subject_ids for generic_combine to work
abundance_bodyfat <- abundance %>%
  left_join(bodyfat, by = "subject_id") %>%
  select(c(subject_id, bodyfat_pcnt))
write.csv(abundance_bodyfat, 'data/bodyfat_percentage_abundance.csv', row.names = FALSE)

shannon_bodyfat <- shannon %>%
  left_join(bodyfat, by = "subject_id") %>%
  select(c(subject_id, bodyfat_pcnt))
write.csv(shannon_bodyfat, 'data/bodyfat_percentage_shannon.csv', row.names = FALSE)

chao1_bodyfat <- chao1 %>%
  left_join(bodyfat, by = "subject_id") %>%
  select(c(subject_id, bodyfat_pcnt))
write.csv(chao1_bodyfat, 'data/bodyfat_percentage_chao1.csv', row.names = FALSE)

muc2plant_bodyfat <- muc2plant %>%
  left_join(bodyfat, by = "subject_id") %>%
  select(c(subject_id, bodyfat_pcnt))
write.csv(muc2plant_bodyfat, 'data/bodyfat_percentage_muc2plant.csv', row.names = FALSE)
