# title: 1.5_raw_CAZy_families
# author: Sarah Blecksmith
# purpose: make a table of aggregated un-normalized CAZy families and subfamilies

library(stringr)
library(tidyr)
library(stats)


cazy_genes <- read.csv("data/cazyme_raw_gene_table_330.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


# split the gene and family name apart  
# need extra backslashes due to special character
cazy_genes$gene <- str_split(as.character(cazy_genes$family), "\\|", n=3, simplify = TRUE)[,1]

# make a column of the family and EC number (if there is one)
cazy_genes$gene_less <- str_split(as.character(cazy_genes$family), "\\|", n=2, simplify = TRUE)[,2]

# Get the max number of chunks when splitting on |  = 8
nmax <- max(str_count(cazy_genes$gene_less, "\\|")) + 1
# split on | and make nmax columns with the chunks
cazy_genes <- separate(cazy_genes, gene_less, paste0("col", seq_len(nmax)), sep = "\\|", fill = "right")

# move the family columns to the front to make it easier to check
cazy_genes <- cazy_genes %>% relocate(gene,col1, col2, col3, col4, col5, col6, col7, col8)

# Ugly, but works
# Want to make a dataframe with each column that could be a cazy family, plus all the subject data 
# then rbind them
# This loop preserves the subfamilies so GH5_2 is not counted with GH5_3
cazy_genes_sep <- data.frame()
for (i in 1:nmax){
  family <- data.frame()
  # grab the coli column and all the subject_id columns (4 digit number)
  family <- cazy_genes %>% select(c(paste0("col",i), matches("[0-9]{4}")))
  family <- rename(family, cazy_fam = paste0("col",i))
  cazy_genes_sep <- rbind(cazy_genes_sep, family)
}


# now want only the CAZyme families, these are the cazy_fam entries that start with letters
cazy_genes_fams <- filter(cazy_genes_sep, grepl("^[A-Z]+", cazy_fam) )

# Aggregate by the cazy fanily
cazy_genes_agg <- aggregate(.~cazy_fam, 
                            data=cazy_genes_fams,
                            sum)

write.csv(cazy_genes_agg, "data/raw_cazyme_families.csv", row.names = FALSE)
