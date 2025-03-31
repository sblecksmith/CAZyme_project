# title: taxaHFE_foods
# author: Sarah Blecksmith
# purpose: look at foods in the categories that appear in SHAP plots


library(data.tree)

# read taxaHFE files
muc2plant_taxaHFE = read.csv('../CAZyme_ML/taxaHFE/input_data/quartile_muc2plant_food_taxaHFE_raw_data.tsv', header = T, 
                             stringsAsFactors = F, check.names = F, sep = "\t")

shannon_taxaHFE = read.csv('../CAZyme_ML/taxaHFE/input_data/quartile_shannon_food_taxaHFE_raw_data.tsv', header = T, 
                           stringsAsFactors = F, check.names = F, sep = "\t")

chao_taxaHFE = read.csv('../CAZyme_ML/taxaHFE/input_data/quartile_chao1_food_taxaHFE_raw_data.tsv', header = T, 
                           stringsAsFactors = F, check.names = F, sep = "\t")

abundance_taxaHFE = read.csv('../CAZyme_ML/taxaHFE/input_data/quartile_abundance_food_taxaHFE_raw_data.tsv', header = T, 
                           stringsAsFactors = F, check.names = F, sep = "\t")


# muc2plant
muc2plant_potatoes <- muc2plant_taxaHFE %>% filter(., grepl("L2_White_potatoes_and_Puerto_Rican_starchy_vegetables", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

write.table(print(muc2plant_potatoes, row.names = FALSE), file = "output/muc2plant_taxaHFE_foods.txt", row.names = FALSE)

# Shannon
shannon_cereal <- shannon_taxaHFE %>% filter(., grepl("L2_Cereals_not_cooked", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

shannon_cheese <- shannon_taxaHFE %>% filter(., grepl("L5_Cheddar", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

shannon_milk <- shannon_taxaHFE %>% filter(., grepl("L3_Milk_fluid[^_]", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

shannon_bread <- shannon_taxaHFE %>% filter(., grepl("L3_White_breads", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

shannon_rice <- shannon_taxaHFE %>% filter(., grepl("L5_Rice[^_]", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

write.table(print(shannon_cereal, row.names = FALSE), file = "output/shannon_taxaHFE_foods.txt", row.names = FALSE)
write.table(print(shannon_cheese, row.names = FALSE), file = "output/shannon_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(shannon_milk, row.names = FALSE), file = "output/shannon_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(shannon_bread, row.names = FALSE), file = "output/shannon_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(shannon_rice, row.names = FALSE), file = "output/shannon_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)

# Chao1
chao_sugars <- chao_taxaHFE %>% filter(., grepl("L2_Sugars_and_sweets", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

chao_cereal <- chao_taxaHFE %>% filter(., grepl("L2_Cereals_not_cooked", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

chao_berries <- chao_taxaHFE %>% filter(., grepl("L3_Berries", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

chao_water <- chao_taxaHFE %>% filter(., grepl("L3_Water_not_bottled", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

chao_fish <- chao_taxaHFE %>% filter(., grepl("L2_Fish_and_shellfish", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

write.table(print(chao_sugars, row.names = FALSE), file = "output/chao_taxaHFE_foods.txt", row.names = FALSE)
write.table(print(chao_cereal, row.names = FALSE), file = "output/chao_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(chao_berries, row.names = FALSE), file = "output/chao_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(chao_water, row.names = FALSE), file = "output/chao_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(chao_fish, row.names = FALSE), file = "output/chao_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)


# abundance
abundance_cheese <- abundance_taxaHFE %>% filter(., grepl("L5_Cheddar", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

abundance_sugars <- abundance_taxaHFE %>% filter(., grepl("L2_Sugars_and_sweets", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

abundance_soda <- abundance_taxaHFE %>% filter(., grepl("L3_Soft_drinks_carbonated", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

abundance_tomato <- abundance_taxaHFE %>% filter(., grepl("L2_Tomatoes_and_tomato_mixtures", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

abundance_cereal <- abundance_taxaHFE %>% filter(., grepl("L2_Cereals_not_cooked", pathString)) %>% filter(., passed_prevelance == "TRUE" & passed_abundance == "TRUE") %>% data.tree::as.Node()

write.table(print(abundance_cheese, row.names = FALSE), file = "output/abundance_taxaHFE_foods.txt", row.names = FALSE)
write.table(print(abundance_sugars, row.names = FALSE), file = "output/abundance_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(abundance_soda, row.names = FALSE), file = "output/abundance_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(abundance_tomato, row.names = FALSE), file = "output/abundance_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)
write.table(print(abundance_cereal, row.names = FALSE), file = "output/abundance_taxaHFE_foods.txt", row.names = FALSE, append = TRUE)

