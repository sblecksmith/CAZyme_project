# title: figure_7
# author: Sarah Blecksmith
# purpose: make beeswarm plots of top 5 features for models of interest

  
library(tidyverse) 
library(shapviz)
library(ggpubr)
library(cowplot)
library(gridExtra)

source('scripts/functions/create_beeswarm_RDS.R')

rds = read.csv('output/dietML_max_seed_paths.csv') 

# Load model features
top_features = read.csv('output/dietML_top_features.csv') 


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


muc2plant_plots <- grid.arrange(arrangeGrob(regression_list[["plot_muc2plant_food_taxaHFE"]], 
                                            top = text_grob("A. food_taxaHFE", size = 16, face = "bold", hjust = .2), 
                                            padding = unit(3, "line")), 
                                arrangeGrob(regression_list[["plot_muc2plant_microbe_taxaHFE"]], 
                                            top = text_grob("B. microbe_taxaHFE", size = 16, face = "bold", hjust = .87), 
                                            padding = unit(3, "line")), 
                                clip="off", ncol = 2, padding = unit(3, "line"), 
                                bottom = text_grob("SHAP values for mucin:plant", size = 20, face = "bold"))


shannon_plots <- grid.arrange(arrangeGrob(regression_list[["plot_shannon_food_taxaHFE"]], 
                                            top = text_grob("C. food_taxaHFE", size = 16, face = "bold", hjust = .2), 
                                            padding = unit(3, "line")), 
                                arrangeGrob(regression_list[["plot_shannon_microbe_taxaHFE"]], 
                                            top = text_grob("D. microbe_taxaHFE", size = 16, face = "bold", hjust = .87), 
                                            padding = unit(3, "line")), 
                                clip="off", ncol = 2, padding = unit(3, "line"), 
                              bottom = text_grob("SHAP values for Plant CAZyme Shannon diversity", size = 20, face = "bold"))


chao_plots <- grid.arrange(arrangeGrob(regression_list[["plot_chao1_food_taxaHFE"]], 
                                          top = text_grob("E. food_taxaHFE", size = 16, face = "bold", hjust = .2), 
                                          padding = unit(3, "line")), 
                              arrangeGrob(regression_list[["plot_chao1_microbe_taxaHFE"]], 
                                          top = text_grob("F. microbe_taxaHFE", size = 16, face = "bold", hjust = .87), 
                                          padding = unit(3, "line")), 
                              clip="off", ncol = 2, padding = unit(3, "line"), 
                           bottom = text_grob("SHAP values for Plant CAZyme Chao1 diversity", size = 20, face = "bold"))


abundance_plots <- grid.arrange(arrangeGrob(regression_list[["plot_abundance_food_taxaHFE"]], 
                                       top = text_grob("G. food_taxaHFE", size = 16, face = "bold", hjust = .2), 
                                       padding = unit(3, "line")), 
                           arrangeGrob(regression_list[["plot_abundance_microbe_taxaHFE"]], 
                                       top = text_grob("H. microbe_taxaHFE", size = 16, face = "bold", hjust = .87), 
                                       padding = unit(3, "line")), 
                           clip="off", ncol = 2, padding = unit(3, "line"), 
                           bottom = text_grob("SHAP values for Plant CAZyme abundance", size = 20, face = "bold"))


fig7 <- grid.arrange(muc2plant_plots, shannon_plots, chao_plots, abundance_plots, clip="off", ncol = 1, nrow = 4, padding = unit(3, "line"))
ggsave("figure_7.tiff", device = "tiff", dpi = 300, width = 22, height = 21, units = "in", path = "output", fig7)

