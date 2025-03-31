# title: figure_4
# author: Sarah Blecksmith
# purpose:  Make figure 5, plant cazyme Shannon diversity and abundance with fecal calprotectin and fecal mpo


library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(showtext)

font_add_google("Montserrat", "mont")
showtext_auto()

# load the dietary and microbiome data
FL100_data <- read.csv("data/FL100_merged_variables.csv", header = TRUE) 
microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE) 

# merge the data
merged = FL100_data %>%
  right_join(microbiome_data, by = "subject_id") %>%
  mutate(sex = factor(sex))


inflamm_variables = c("fecal_neopterin", "fecal_mpo")

# read in saved transformations
inflammation_transformations <- read.csv('data/inflammation_transformations.csv', header = TRUE)

# Transform the variables
for (outcome in inflammation_transformations$Outcome) {
  #Extract recommended transformation for outcome variable   
  transform_recommendation = (inflammation_transformations %>% filter(Outcome == outcome))[1,2]
  
  #Retrieve the function object
  transform = match.fun(transform_recommendation)
  
  
  outcome_transformed = paste0(outcome, "_", transform_recommendation)
  #Transformed outcome and pulling out the transformed data
  merged[,outcome_transformed] = transform(merged[[outcome]])$x.t
}

# NEOPTERIN
merged_neopt <- subset(merged, !is.na(merged[,"fecal_neopterin"]))
# Neopterin - Plant cazyme Shannon
shannon_neopt_resid <- resid(lm(data=merged_neopt, Shannon ~ age + sex + bmi_final))
neopt_resid <- resid(lm(data=merged_neopt,fecal_neopterin_yeojohnson ~ age + sex + bmi_final))

shannon_neopt_resid <- as.numeric(shannon_neopt_resid)
neopt_resid <- as.numeric(neopt_resid)


p1 <- ggplot(data = NULL, aes(x=shannon_neopt_resid, y=neopt_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Plant CAZyme Shannon diversity by normalized fecal neopterin",
       subtitle = "Covariates age, sex and BMI",
       x = "Plant CAZyme Shannon Diversity",
       y = "Normalized Fecal Neopterin",
       tag = "A")

# Neopterin - Plant cazyme abundance
abund_neopt_resid <- resid(lm(data=merged_neopt, plant_cazyme_abundance ~ age + sex + bmi_final))
neopt_resid <- resid(lm(data=merged_neopt,fecal_neopterin_yeojohnson ~ age + sex + bmi_final))

abund_neopt_resid <- as.numeric(abund_neopt_resid)
neopt_resid <- as.numeric(neopt_resid)


p2 <- ggplot(data = NULL, aes(x=abund_neopt_resid, y=neopt_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Plant CAZyme abundance by normalized fecal neopterin",
       subtitle = "Covariates age, sex and BMI",
       x = "Plant CAZyme Abundance",
       y = "Normalized Fecal Neopterin",
       tag = "B") 





# MPO
# MPO - Plant cazyme Shannon
shannon_resid <- resid(lm(data=merged, Shannon ~ age + sex + bmi_final))
mpo_resid <- resid(lm(data=merged,fecal_mpo_boxcox ~ age + sex + bmi_final))

shannon_resid <- as.numeric(shannon_resid)
mpo_resid <- as.numeric(mpo_resid)

p3 <-ggplot(data = NULL, aes(x=shannon_resid, y=mpo_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Plant CAZyme Shannon diversity by normalized fecal myeloperoxidase",
       subtitle = "Covariates age, sex and BMI",
       x = "Plant CAZyme Shannon Diversity",
       y = "Normalized Fecal Myeloperoxidase",
       tag = "C") 


# MPO - Plant cazyme abundance
abund_resid <- resid(lm(data=merged, plant_cazyme_abundance ~ age + sex + bmi_final))
mpo_resid <- resid(lm(data=merged,fecal_mpo_boxcox ~ age + sex + bmi_final))

abund_resid <- as.numeric(abund_resid)
mpo_resid <- as.numeric(mpo_resid)


p4 <- ggplot(data = NULL, aes(x=abund_resid, y=mpo_resid)) +
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Plant CAZyme abundance by normalized fecal myeloperoxidase",
       subtitle = "Covariates age, sex and BMI",
       x = "Plant CAZyme Abundance",
       y = "Normalized Fecal Myeloperoxidase",
       tag = "D")


fig4 <- grid.arrange(p1, p2, p3, p4, ncol=2)

ggsave("figure_4.tiff", device = "tiff", dpi = 300, width = 15, height = 10, units = "in", path = "output", fig4)
