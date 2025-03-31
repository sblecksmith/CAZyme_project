# title: CAZy_family_substrates
# author: Sarah Blecksmith
# purpose: read in the smits family substrate annotation pdf, DOI: 10.1126/science.aan4834

library(stringr)
library(dplyr)


# CAZyme families for each substrate copied from supplemental data pdf of Smits paper
plant <- "GH1; GH2; GH3; GH4; GH5; GH8; GH9; GH11; GH12; GH15; GH16; GH17; GH26; GH27; GH28; GH29; GH36; GH39; GH43; GH44; GH48; GH51; GH53; GH55; GH67; GH74; GH78; GH93; GH94; GH95; GH115; GH117; GH121; PL1; PL2; PL6; PL7; PL9; PL11; PL15; PL22"

animal <- "GH1; GH2; GH3; GH4; GH18; GH19; GH20; GH29; GH33; GH38; GH58; GH79; GH84; GH85; GH88; GH89; GH92; GH95; GH98; GH99; GH101; GH105; GH109; GH110; GH113; PL6; PL8; PL12; PL13; PL21"

sugars <- "GH32; GH68; GH70; GH91"

mucin <- "GH2; GH20; GH27; GH29; GH33; GH35; GH36; GH95; GH89; GH110; GH129"


# Separate each substrate string on ; and make a dataframe
plant_families <- as.data.frame(str_split(plant, ";"), col.names = c("cazyme_family"))
plant_families$cazyme_family <- gsub(" ", "", plant_families$cazyme_family)
plant_families$substrate <- "plant"

animal_families <- as.data.frame(str_split(animal, ";"), col.names = c("cazyme_family"))
animal_families$cazyme_family <- gsub(" ", "", animal_families$cazyme_family)
animal_families$substrate <- "animal"

sugar_families <- as.data.frame(str_split(sugars, ";"), col.names = c("cazyme_family"))
sugar_families$cazyme_family <- gsub(" ", "", sugar_families$cazyme_family)
sugar_families$substrate <- "sucrose_fructans"

mucin_families <- as.data.frame(str_split(mucin, ";"), col.names = c("cazyme_family"))
mucin_families$cazyme_family <- gsub(" ", "", mucin_families$cazyme_family)
mucin_families$substrate <- "mucin"


all_substrates <- rbind(plant_families, animal_families, sugar_families, mucin_families)

all_substrates_agg <- all_substrates %>%
  group_by(cazyme_family) %>%
  mutate(multi = toString(substrate)) %>%
  as.data.frame()

all_substrates_sep <- all_substrates_agg %>%
  filter(!grepl(",", multi)) %>%
  select("cazyme_family", "substrate")


# write full file of unique substrate families and separate files for plant and mucin
write.csv(all_substrates_sep, "data/smits_cazyme_substrates_sep.csv", row.names = FALSE)
write.csv(all_substrates_sep[all_substrates_sep$substrate == "plant",], "data/smits_cazyme_unique_plant.csv", row.names = FALSE)
write.csv(all_substrates_sep[all_substrates_sep$substrate == "mucin",], "data/smits_cazyme_unique_mucin.csv", row.names = FALSE)
  