# title: figure_5
# author: Sarah Blecksmith
# purpose: plot dietML mean balanced accuracy for each muc2plant and SHAP plots
---
  
library(tidyverse) 
library(rvest) 
library(xml2) 
library(stringr) 
library(Hmisc) 
library(ggpubr)
library(grid)
library(shapviz)
library(cowplot)
library(gridExtra)
library(showtext)
library(bestNormalize)

font_add_google("Montserrat", "mont")
showtext_auto()

#Load Functions
source('scripts/functions/find_rds.R')
source('scripts/functions/extract_rds_data.R')
source('scripts/functions/extract_performance_metrics.R')
source('scripts/functions/create_beeswarm_RDS.R')

rds = read.csv('output/dietML_max_seed_paths.csv') 

# Load model features
top_features = read.csv('output/dietML_top_features.csv') 


#Get the paths of all rds files
rds_files = as.data.frame(find_rds_files("../CAZyme_ML/dietML")) %>%
  dplyr::rename('path' = 1)

tags = separate_wider_delim(rds_files, cols = path, delim = "/", names_sep = "/")[,c("path/4","path/6", "path/8")] %>%
  dplyr::rename('outcome' = 1,
                'dataset' = 2,
                'filename' = 3) %>%
  mutate(seed = str_extract(filename, "[[:digit:]]+"))
  


#combine the dataframes
rds_info = cbind(rds_files, tags) %>%
  mutate(label = paste(outcome, dataset, seed, sep = ','))

write.csv(rds_info, 'output/dietML_rds_paths.csv', row.names = FALSE)

### 3) Performance metrics
metrics_data = list()

# Loop through each RDS file and extract the data
for (i in seq_along(rds_info$path)) {
  file_path = rds_info$path[i]
  extracted_data = extract_performance(file_path)
  metrics_data[[i]] = extracted_data
}

# Format
all_metrics = bind_rows(metrics_data) %>%
  rename('metric' = 1 ,
         'estimator' = 2, 
         'estimate' = 3, 
         'config' = 4, 
         'label' = 5) %>%
  left_join(rds_info) %>%
  select(-path) 
write.csv(all_metrics, 'output/dietML_performance_metrics.csv', row.names = FALSE)


mean_bal_acc <- filter(all_metrics, metric == "bal_accuracy") %>%
  group_by(outcome, dataset) %>%
  summarise(mean = mean(estimate))

max_seed <- filter(all_metrics, metric == "bal_accuracy") %>%
  group_by(outcome, dataset) %>%
  mutate(max_seed = seed[which.max(estimate)]) %>%
  filter(seed == max_seed) %>%
  select(metric, estimate, outcome, dataset, seed) %>%
  mutate(label = paste(outcome, dataset, seed, sep = ',')) %>%
  left_join(rds_info %>% select(c("label","path")), by = "label")
write.csv(max_seed, 'output/dietML_max_seed_paths.csv', row.names = FALSE)


# plot the mean balanced accuracy for each dataset
p1 <- mean_bal_acc %>%
  filter(outcome == "muc2plant") %>%
  mutate(dataset = case_when(dataset == "mono_div" ~ "monosaccharide diversity",
                             dataset == "food_taxaHFE" ~ "food taxaHFE",
                             dataset == "microbe_taxaHFE" ~ "microbial taxaHFE",
                             TRUE ~ dataset)) %>%
  #mutate(outcome = case_when(outcome == "muc2plant" ~ "mucin:plant", TRUE ~ outcome)) %>%
  ggplot(aes(x = dataset, y = mean)) +
  geom_bar(position = "dodge", stat = "identity", show.legend = FALSE, fill = "deepskyblue4") +
  geom_text(aes(label = dataset),angle = 90, hjust = 1.1, color = "white") +
  geom_text(aes(label=round(mean, digits = 2)), nudge_y = 0.025) + 
  labs(
    y = "mean balanced accuracy",
    tag = "A") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        theme(text = element_text(size=18,family = "mont"))) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 1) 

p1
#ggsave("figure_6.tiff", device = "tiff", dpi = 300, width = 10, height = 5, units = "in", path = "output", fig6)

##Regression Beeswarm Plots
regression_list = list()

# Loop through paths in your dataframe
for (i in seq_along(rds$path)) {
  path = rds$path[i]
  feature_list = top_features
  opt = paste0(rds$outcome[i],"_", rds$dataset[i])
  
  plot_result = create_beeswarm(path, feature_list)
  
  # Generate a unique name for each plot based on the opt values
  unique_name = paste0("plot_", opt, sep = "")
  
  # Store the plot_result in the list with the unique name
  regression_list[[unique_name]] = plot_result
}


p2 <- grid.arrange(arrangeGrob(regression_list[["plot_muc2plant_food_taxaHFE"]]), 
                                            #top = text_grob("food_taxaHFE", size = 14, face = "bold", hjust = .2)), 
                                            #padding = unit(3, "line")), 
                                arrangeGrob(regression_list[["plot_muc2plant_microbe_taxaHFE"]]), 
                                            #top = text_grob("microbe_taxaHFE", size = 14, face = "bold", hjust = .87)), 
                                            #padding = unit(3, "line")), 
                                clip="off", nrow = 2) #padding = unit(3, "line"), 
                                



#fig5 <- grid.arrange(p1,p2, clip="off", ncol = 1, nrow = 2, padding = unit(3, "line"))
#fig5 <- grid.arrange(p1,p2, clip="off", ncol = 1, nrow = 2)

fig5 <- grid.arrange(p1, p2, clip="off",
                     #widths=c(1,1,1),
                     layout_matrix = rbind(c(1,2,2,2),
                                           c(1,2,2,2),
                                           c(1,2,2,2)))

ggsave("figure_5.tiff", device = "tiff", dpi = 300, width = 16, height = 8, units = "in", path = "output", fig5)


