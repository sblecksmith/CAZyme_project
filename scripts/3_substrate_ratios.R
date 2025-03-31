# title: 3_substrate_ratios
# author: Sarah Blecksmith
# purpose: calculate the mucin to plant cazyme ratio using the unrounded aggregated CAZyme family table
# and the families unique to one substrate output from CAZy_family_substrates.R uses substrate annotation scheme 
# from DOI: 10.1126/science.aan4834


library(tidyverse)

# Want to use the unrounded CAZyme counts here
families_unrounded <- read.csv("data/cazyme_families_unrounded.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


# List of CAZyme families unique to each substrate
all_substrates_sep <- read.csv("data/smits_cazyme_substrates_sep.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


# Collect the plant families
plant <- filter(all_substrates_sep, substrate == 'plant')
plant_fams <- data.frame()
for (fam in plant$cazyme_family){
  fams <- data.frame()
  # for family GH11, we want to catch all the subfamilies but not
  # GH114 or GH117.  So this grep matches on GH11 alone and all the GH_X
  fams <- filter(families_unrounded, grepl(paste0("^", fam, "$|^", fam, "_"), cazy_fam))
  plant_fams <- rbind(plant_fams, fams)
}

# Collect the mucin families
mucin <- filter(all_substrates_sep, substrate == 'mucin')
mucin_fams <- data.frame()
for (fam in mucin$cazyme_family){
  fams <- data.frame()
  # for family GH11, we want to catch all the subfamilies but not
  # GH114 or GH117.  So this grep matches on GH11 alone and all the GH_X
  fams <- filter(families_unrounded, grepl(paste0("^", fam, "$|^", fam, "_"), cazy_fam))
  mucin_fams <- rbind(mucin_fams, fams)
}

# Sum the mucin and plant families and make new dataframe
plant_sumGHPL <- data.frame(subject_id = colnames(plant_fams[,-1]),plant_family_totalGHPL=colSums(plant_fams[, -1]))
plant_famsGH <- plant_fams[grep("GH", plant_fams$cazy_fam), ]
plant_sumGH <- data.frame(subject_id = colnames(plant_famsGH[,-1]),plant_family_totalGH=colSums(plant_famsGH[, -1]))

mucin_sum <- data.frame(subject_id = colnames(mucin_fams[,-1]),mucin_family_total=colSums(mucin_fams[, -1]))


merged = plant_sumGHPL %>%
  left_join(plant_sumGH, by = "subject_id") %>%
  left_join(mucin_sum, by = "subject_id") 

# calculate muc2plant
merged$muc2plantGHPL <- merged$mucin_family_total/merged$plant_family_totalGHPL
merged$muc2plantGH <- merged$mucin_family_total/merged$plant_family_totalGH
write.csv(merged, "data/muc2plant_ratioGH_GHPL.csv", row.names = FALSE)

