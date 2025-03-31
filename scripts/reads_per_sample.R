# title: reads_per_sample
# author: Sarah Blecksmith
# purpose:  calculate mean and SD of reads per sample

library(dplyr)

# want just the samples in this analysis
microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE)

read_count <- read.table("data/FL100_extFrag_readcounts.txt", header = FALSE, sep = '\t') %>%
  rename(subject_id = V1) %>%
  rename(count = V2)

# merge the data
merged = read_count %>%
  right_join(microbiome_data, by = "subject_id") 

mean(merged$count) # 22,468,865
sd(merged$count)    # 7,622,850 
