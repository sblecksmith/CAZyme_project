# title: dietML_feature_formatting
# author: Sarah Blecksmith, adapted from Dr. Stephanie Wilson
# purpose: Summarize and format feature names from dietML runs

# Summarize and format feature names from dietML runs
  
#  This script pools output from multiple RDS files, which are a key output file from the dietML program built by Dr. Andrew Oliver. Each RDS file is a packaged R environment which contains numerous dataframes. This script:
  
#1) Extract and formats feature names
#2) Pull out SHAP importance data
#3) Create Directionality Plot


library(tidyverse) 
library(rvest) 
library(xml2) 
library(stringr) 
library(Hmisc) 
library(ggpubr)
library(grid)

source('scripts/functions/find_rds.R')
source('scripts/functions/extract_rds_data.R')


rds_info = read.csv('output/dietML_max_seed_paths.csv')

### 2) Pull out SHAP importance data
#Run Loop to pull out data from each of our RDS files

shap_data = list()
# Loop through each RDS file and extract the data
for (i in seq_along(rds_info$path)) {
  file_path = rds_info$path[i]
  extracted_data = extract_data_from_rds(file_path)
  shap_data[[i]] = extracted_data
}


# Use map2_dfr to iterate over each list and add the label to the MeanShaps dataframe
merged_MeanShaps = map2_dfr(shap_data, rds_info$label, add_label_to_MeanShaps) %>%
  separate(label,
           into = c("outcome", "dataset", "seed"),
           sep=",",
           remove = FALSE) %>%
  
  
  mutate(Direction = ifelse(Avg_SHAP > 0, 'Positive', 'Negative')) %>%
  left_join(features, by = 'feature') %>%
  relocate(feature_publish, .after= 'feature') %>%
  select(-c(marker, label_model)) %>%
  rename('marker' = 'response_type')

top_features = merged_MeanShaps %>%
  #For each model outcome combination
  group_by(label) %>%
  #Extract top five features based off Importance
  top_n(n = 5, wt = Importance) %>%
  arrange(desc(Importance))

write.csv(top_features, 'output/dietML_top_features.csv', row.names = FALSE)



### 3) Directionality Plot
#Model plots in publication are represented by beeswarm plots. 
PLOT = ggplot(top_features, aes(x = marker, y = reorder(feature_publish, Importance), fill = Direction)) + 
  labs(x = "", y = '', 
       subtitle = '')+
  geom_point(aes(colour = factor(Direction)), size = 2) +
  theme_bw() + 
  coord_fixed(ratio = 0.3) + 
  guides(color = guide_legend(title = "Direction")) + 
  scale_color_manual(values = c("#CB7A5C", "#5785C1")) + 
  theme(legend.position = 'bottom',
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        plot.margin = margin(c(0, 0, 0, 0)))

PLOT
