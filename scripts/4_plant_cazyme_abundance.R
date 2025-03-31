# title: 4_plant_cazyme_abundance
# author: Sarah Blecksmith
# purpose: calculate the plant cazyme abundance using the normalized but unrounded aggregated CAZyme family table
# and the families unique to one substrate output from CAZy_family_substrates.R using annotation scheme 
# from DOI: 10.1126/science.aan4834


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


write.csv(plant_fams, "data/plant_families_unrounded.csv", row.names = FALSE)

plant_sum <- data.frame(subject_id = colnames(plant_fams[,-1]),plant_cazyme_abundance=colSums(plant_fams[, -1]))


write.csv(plant_sum, "data/plant_cazyme_abundance.csv", row.names = FALSE)
