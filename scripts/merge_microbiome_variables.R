# title: merge_microbiome_variables
# author: Sarah Blecksmith
# purpose: Making one file of muc2plant and cazyme diversity measures, weeding out the after V2 and >24hr samples


library(dplyr)
library(Hmisc)

#muc2plant <- read.csv("data/muc2plant_ratioGH_GHPL.csv", header = TRUE) %>%
 # select(c(subject_id, muc2plantGH, muc2plantGHPL))

muc2plant <- read.csv("data/muc2plant_sulf.csv", header = TRUE) %>%
  select(c(subject_id, plant_family_totalGH, plant_family_totalGHPL, mucin_family_total, muc2plantGH, muc2plantGHPL, sulfatase_rpkg, sulfatase_no_akk_rpkg))

alpha_diversity <- read.csv("data/cazyme_families_rounded_diversity.csv", header = TRUE) %>%
  select(c(subject_id, Observed, Shannon, Chao1))
abundance <- read.csv("data/plant_cazyme_abundance.csv", header = TRUE)
GHPLabundance <- read.csv("data/GHPL_abundance.csv", header = TRUE, check.names = FALSE)
stool <- read.csv("data/FL100_stool_variables.txt", header = TRUE, sep = "\t") %>%
  select(c(subject_id, AfterV2, diff_time_hrs))


merged = muc2plant %>%
  left_join(alpha_diversity, by = "subject_id") %>%
  left_join(abundance, by = "subject_id") %>%
  left_join(GHPLabundance, by = "subject_id") %>%
  left_join(stool, by = "subject_id") 

# Make quartiles of shannon, chao1, muc2plant, plant cazyme abundance
merged$Shannon_quartile = as.numeric(cut2(merged$Shannon, g = 4))
merged$Chao1_quartile = as.numeric(cut2(merged$Chao1, g = 4))
merged$muc2plantGH_quartile = as.numeric(cut2(merged$muc2plantGH, g = 4))
merged$muc2plantGHPL_quartile = as.numeric(cut2(merged$muc2plantGHPL, g = 4))
merged$plant_cazyme_abundance_quartile = as.numeric(cut2(merged$plant_cazyme_abundance, g = 4))
merged$sulfatase_rpkg_quartile = as.numeric(cut2(merged$sulfatase_rpkg, g = 4))
merged$sulfatase_no_akk_rpkg_quartile = as.numeric(cut2(merged$sulfatase_no_akk_rpkg, g = 4))
#Filter out where AfterV2 = 0, we don't want to use the subjects who submitted stool after the test meal
merged <- merged %>% filter(AfterV2 == 0) # now 303

# Filter out where stool sample not processed within 24 hours
merged <-merged %>% filter(diff_time_hrs < 24) # now 285


write.csv(merged[,!(colnames(merged) %in% c("AfterV2", "diff_time_hrs"))], "data/microbiome_merged_variablesGH_GHPL.csv", row.names = FALSE)

