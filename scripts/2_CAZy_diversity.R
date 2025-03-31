# title: 2_CAZy_diversity
# author: Sarah Blecksmith
# purpose: use phyloseq to calculate plant cazyme family diversity and GH/PL diversity. uses substrate 
# annotation scheme from DOI: 10.1126/science.aan4834


library(phyloseq)
library(dplyr)

families_rounded <- read.csv("data/cazyme_families_rounded.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
families_unrounded <- read.csv("data/cazyme_families_unrounded.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
plant <- read.csv("data/smits_cazyme_unique_plant.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


# select just the plant families
plant_families <- data.frame()
for (fam in plant$cazyme_family){
  families <- data.frame()
  # for family GH11, we want to catch all the subfamilies but not
  # GH114 or GH117.  So this grep matches on GH11 alone and all the GH11_X
  grep_phrase <- paste0("^", fam, "$|^", fam, "_")
  families <- filter(families_rounded, grepl(grep_phrase, cazy_fam))
  plant_families <- rbind(plant_families, families)
  
}
write.csv(plant_families, "data/plant_families_rounded.csv", row.names = FALSE)

# make matrix for phyloseq
row.names(plant_families) <- plant_families$Family
plant_families <- subset(plant_families, select=-c(Family)) %>% as.matrix.data.frame()
ps_plant_families <- phyloseq(otu_table(plant_families, taxa_are_rows = TRUE))


adiv_plant <- estimate_richness(ps_plant_families, split = TRUE, measures = c("Shannon", "Chao1", "Observed"))

# clean up
adiv_plant$subject_id <- row.names(adiv_plant)
adiv_plant$subject_id <- gsub("X", "", adiv_plant$subject_id)

write.csv(adiv_plant, "data/cazyme_families_rounded_diversity.csv", row.names = FALSE)


# select all GH and PL families
GHPL <- families_rounded[grep("GH|PL", families_rounded$cazy_fam), ]
# make matrix for phyloseq
row.names(GHPL) <- GHPL$cazy_fam
GHPL <- subset(GHPL, select=-c(cazy_fam)) %>% as.matrix.data.frame()
ps_GHPL <- phyloseq(otu_table(GHPL, taxa_are_rows = TRUE))


adiv_GHPL <- estimate_richness(ps_GHPL, split = TRUE, measures = c("Shannon", "Chao1", "Observed"))

# clean up
adiv_GHPL$subject_id <- row.names(adiv_GHPL)
adiv_GHPL$subject_id <- gsub("X", "", adiv_GHPL$subject_id)

write.csv(adiv_GHPL, "data/cazyme_GHPL_rounded_diversity.csv", row.names = FALSE)
