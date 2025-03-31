# title: figure_6
# author: Sarah Blecksmith
# purpose: plot dietML mean balanced accuracy for each dataset 
---
  
library(tidyverse) 
library(rvest) 
library(xml2) 
library(stringr) 
library(Hmisc) 
library(ggpubr)
library(grid)


#Load Functions
source('scripts/functions/find_rds.R')
source('scripts/functions/extract_rds_data.R')
source('scripts/functions/extract_performance_metrics.R')


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

# Figure 6
# plot the mean balanced accuracy for each outcome and dataset
fig6 <- mean_bal_acc %>%
  mutate(outcome = case_when(outcome == "muc2plant" ~ "mucin:plant", TRUE ~ outcome)) %>%
  ggplot(aes(x = dataset, y = mean)) +
  geom_bar(position = "dodge", stat = "identity", show.legend = FALSE, fill = "deepskyblue4") +
  geom_text(aes(label = dataset),angle = 90, hjust = 1.2, color = "white") +
  geom_text(aes(label=round(mean, digits = 2)), nudge_y = 0.05) + 
  facet_wrap(~outcome) +
  ylab("mean balanced accuracy") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  ggtitle("Mean balanced accuracy for 10 seeds with each dataset")

  ggsave("figure_6.tiff", device = "tiff", dpi = 300, width = 18, height = 9, units = "in", path = "output", fig6)
