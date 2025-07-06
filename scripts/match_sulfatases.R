# title: match_sulfatases
# author: Sarah Blecksmith
# purpose: collect sulfatase abundance from humann3 output, normalize with Microbe Census and calculate muc2plantGHPL_sulf

library(dplyr)
library(tidyr)


# uniref ids for S1_4, S1_11, S1_15, S1_16, S1_20, S1_46
# files downloaded from Sulfatlas and concatenated on the command line
in_FL100 <- read.csv("FL100_sulfatase_gene_list.csv", header = TRUE, check.names = FALSE) 
FL100_list = in_FL100$uniprot1

# Open the giant humann3 output file
genefams <- read.csv("merged_genefamilies.tsv", header = TRUE, check.names = FALSE, sep="\t") %>%
  slice(-1) %>%
  dplyr::rename(gene_family = '# Gene Family')

genefams$gene_family <- gsub("UniRef90_", "", genefams$gene_family)

genefams <- genefams %>% 
  separate_wider_delim(cols=c('gene_family'), delim = '|', names = c("uniprot", "organism"), too_few = "align_start") 

sulf_abund <- genefams[genefams$uniprot %in% FL100_list,]

write.csv(sulfatases, "FL100sulfatases.csv", row.names = FALSE)

sulf_abund <- sulf_abund %>%
  filter(is.na(organism)) %>%
  select(-"organism") %>%
  column_to_rownames(var = "uniprot")

sulf_sum <- data.frame(subject_id = colnames(sulf_abund),sulfatase_abundance=colSums(sulf_abund))
sulf_sum$subject_id <- gsub(".extendedFrags_Abundance-RPKs", "", sulf_sum$subject_id)                                 
rownames(sulf_sum) = NULL 

# load in Microbe Census genome equivalents to normalize for genome size
# this was pulled out of the XXXX_mc.txt files with a command line script
genome_equivalents <- read.csv("data/FL100_genome_equivalents.txt", header = FALSE, check.names = FALSE) %>%
  dplyr::rename(line = V1)
genome_equivalents$line <- gsub("genome_equivalents: ", "", genome_equivalents$line)
genome_equivalents <- separate(genome_equivalents, into = c("subject_id", "gene_eq"),line, sep = " ", fill = "right")
genome_equivalents$subject_id <- gsub("_mc.txt", "", genome_equivalents$subject_id)
genome_equivalents$gene_eq <- as.numeric(genome_equivalents$gene_eq)

muc2plant<- read.csv("data/muc2plant_ratioGH_GHPL.csv", header = TRUE, check.names = FALSE)
merged <- merge(muc2plant, sulf_sum, by = "subject_id")  

merged <- merge(merged, genome_equivalents, by = "subject_id")

# normalize with gene equivalent
merged$sulfatase_rpkg <- merged$sulfatase_abundance/merged$gene_eq
merged$muc2plantGHPL_sulf <- (merged$sulfatase_rpkg + merged$mucin_family_total)/merged$plant_family_totalGHPL

# Adding a few extra normalizations
bestNormalize(merged$muc2plantGHPL_sulf)
microbiome_transformations <- rbind(microbiome_transformations, c("muc2plantGHPL_sulf", "boxcox"))

write.csv(merged, "data/muc2plant_sulf.csv", row.names = FALSE)