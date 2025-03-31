# title: figure_1
# author: Sarah Blecksmith
# purpose: generate figure 1 - Plant-unique GH and PL CAZyme Shannon (A) and Chao1 (B) diversity in participants based on robust CAZyme abundance

library(dplyr)
library(ggplot2)
library(gridExtra)

microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE) 


# range of Shannon rounded
shannon <- microbiome_data %>% ggplot() +
  geom_bar(aes(reorder(subject_id, Shannon), Shannon, fill = Shannon), stat = "identity", 
           color = "#47818d", show.legend = FALSE) +
  labs(y="Shannon Diversity", x = "Subject", title = "Range of plant CAZyme Shannon Diversity",
       tag = "A")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


# range of Chao1
chao1 <- microbiome_data %>% ggplot() +
  geom_bar(aes(reorder(subject_id, Chao1), Chao1, fill = Chao1), stat = "identity", color = "#47818d",
           show.legend = FALSE) +
  labs(y="Chao1 Diversity", x = "Subject", title = "Range of plant CAZyme Chao1 Diversity",
       tag = "B")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())



fig1 <- grid.arrange(shannon, chao1, clip="off", ncol = 2)

ggsave("figure_1.tiff", device = "tiff", dpi = 300, width = 14, height = 5, units = "in", path = "output", fig1)
