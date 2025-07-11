# title: figure_2
# author: Sarah Blecksmith
# purpose: Make Figure 2 -- Shannon, Chao1 and abundance with energy adjusted soluble fiber (A, B, C) and fecal pH (D, E, F)


library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(showtext)
library(bestNormalize)

font_add_google("Montserrat", "mont")
showtext_auto()

# load the dietary and microbiome data
FL100_data <- read.csv("data/FL100_merged_variables.csv", header = TRUE) 
microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE) 

# merge the data
merged = FL100_data %>%
  right_join(microbiome_data, by = "subject_id") %>%
  mutate(sex = factor(sex))

# load the transformations
microbiome_transformations <- read.csv("data/microbiome_transformations.csv", header = TRUE) 

# Transform the variables
for (outcome in microbiome_transformations$Outcome) {
  #Extract recommended transformation for outcome variable   
  transform_recommendation = (microbiome_transformations %>% filter(Outcome == outcome))[1,2]
  
  #Retrieve the function object
  transform = match.fun(transform_recommendation)
  
  
  outcome_transformed = paste0(outcome, "_", transform_recommendation)
  #Transformed outcome and pulling out the transformed data
  merged[,outcome_transformed] = transform(merged[[outcome]])$x.t
}


fiber_merged <- subset(merged, !is.na(merged[,"sol_fibe_perKcal"]))


# Figure 2A - Shannon and FFQ energy-adjusted soluble fiber
# partial regressions
shannon_resid <- resid(lm(data=fiber_merged, Shannon_exp_x ~ age + sex + bmi_final))
sol_fibe_resid <- resid(lm(data=fiber_merged,sol_fibe_perKcal ~ age + sex + bmi_final))

# there are fewer people with sol_fibe_perKcal than 
shannon_resid <- as.numeric(shannon_resid)
sol_fibe_resid <- as.numeric(sol_fibe_resid)

p1 <-ggplot(data = NULL, aes(x=sol_fibe_resid, y=shannon_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  labs(
       y = "Plant CAZyme\nShannon Diversity",
       x = "Energy-adjusted Habitual Soluble Fiber",
       tag = "A") 



# Figure 2B - Chao1 and FFQ energy-adjusted soluble fiber
# partial regressions
chao1_resid <- resid(lm(data=fiber_merged, Chao1_yeojohnson ~ age + sex + bmi_final))
sol_fibe_resid <- resid(lm(data=fiber_merged,sol_fibe_perKcal ~ age + sex + bmi_final))

chao1_resid <- as.numeric(chao1_resid)
sol_fibe_resid <- as.numeric(sol_fibe_resid)


p2 <-ggplot(data = NULL, aes(x=sol_fibe_resid, y=chao1_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  labs(
       y = "Plant CAZyme\nChao1 Diversity",
       x = "Energy-adjusted Habitual Soluble Fiber",
       tag = "B") 

# Figure 2C - Plant CAZyme abundance and FFQ energy-adjusted soluble fiber
# partial regressions
plant_cazyme_abundance_resid <- resid(lm(data=fiber_merged, plant_cazyme_abundance_yeojohnson ~ age + sex + bmi_final))
sol_fibe_resid <- resid(lm(data=fiber_merged, sol_fibe_perKcal ~ age + sex + bmi_final))

plant_cazyme_abundance_resid <- as.numeric(plant_cazyme_abundance_resid)
sol_fibe_resid <- as.numeric(sol_fibe_resid)


p3 <-ggplot(data = NULL, aes(x=sol_fibe_resid, y=plant_cazyme_abundance_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  labs(
       y = "Plant CAZyme Abundance",
       x = "Energy-adjusted Habitual Soluble Fiber",
       tag = "C") 



# Not every sample has a fecal ph measurement
ph_merged <- subset(merged, !is.na(merged[,"fecal_ph"]))

# Figure 2D - Plant CAZyme Shannon and fecal pH
# partial regressions
shannon_ph_resid <- resid(lm(data=ph_merged, Shannon_exp_x ~ age + sex + bmi_final))
ph_resid <- resid(lm(data=ph_merged, fecal_ph ~ age + sex + bmi_final))

shannon_ph_resid <- as.numeric(shannon_ph_resid)
ph_resid <- as.numeric(ph_resid)


p4 <-ggplot(data = NULL, aes(x=ph_resid, y=shannon_ph_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  labs(
       y = "Plant CAZymen\nShannon Diversity",
       x = "Fecal pH",
       tag = "D") 


# Figure 2E - Plant CAZyme Chao1 diversity and fecal pH
# partial regressions
chao1_ph_resid <- resid(lm(data=ph_merged, Chao1_yeojohnson ~ age + sex + bmi_final))
ph_resid <- resid(lm(data=ph_merged, fecal_ph ~ age + sex + bmi_final))

chao1_ph_resid <- as.numeric(chao1_ph_resid)
ph_resid <- as.numeric(ph_resid)


p5 <-ggplot(data = NULL, aes(x=ph_resid, y=chao1_ph_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  labs(
       y = "Plant CAZyme\nChao1 Diversity",
       x = "Fecal pH",
       tag = "E") 


# Figure 2F - Plant CAZyme abundance and fecal pH
# partial regressions
plant_cazyme_abundance_ph_resid <- resid(lm(data=ph_merged, plant_cazyme_abundance_yeojohnson ~ age + sex + bmi_final))
ph_resid <- resid(lm(data=ph_merged, fecal_ph ~ age + sex + bmi_final))

plant_cazyme_abundance_ph_resid <- as.numeric(plant_cazyme_abundance_ph_resid)
ph_resid <- as.numeric(ph_resid)


p6 <-ggplot(data = NULL, aes(x=ph_resid, y=plant_cazyme_abundance_ph_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  labs(
       y = "Plant CAZyme Abundance",
       x = "Fecal pH",
       tag = "F") 


fig2 <- grid.arrange(p1, p2, p3, p4, p5, p6, clip="off", ncol = 3)


ggsave("figure_2.tiff", device = "tiff", dpi = 300, width = 14, height = 8, units = "in", path = "output", fig2)
