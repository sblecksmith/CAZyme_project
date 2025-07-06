# title: figure_4
# author: Sarah Blecksmith
# purpose:  Make figure 4, muc2plant plots

library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(gridExtra)
library(showtext)
library(bestNormalize)

font_add_google("Montserrat", "mont")
showtext_auto()

# load the dietary and microbiome data
FL100_data <- read.csv("data/FL100_merged_variables.csv", header = TRUE) 
microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE) 


# calculate number of subjects above 1 standard deviation for muc2plant
sum(microbiome_data$muc2plantGHPL - mean(microbiome_data$muc2plantGHPL) >= sd(microbiome_data$muc2plantGHPL)) #34

# calculate number of subjects below 1 standard deviation for muc2plant
sum(microbiome_data$muc2plantGHPL - mean(microbiome_data$muc2plantGHPL) <= -sd(microbiome_data$muc2plantGHPL)) #32


# merge the data
merged = FL100_data %>%
  right_join(microbiome_data, by = "subject_id") %>%
  mutate(sex = factor(sex))


inflamm_variables = c("fecal_calprotectin", "fecal_neopterin")

# read in saved transformations
inflammation_transformations <- read.csv('data/inflammation_transformations.csv', header = TRUE)
microbiome_transformations <- read.csv("data/microbiome_transformations.csv", header = TRUE) 

# merge the transformations
transformations <- rbind(inflammation_transformations,microbiome_transformations)

# Transform the variables
for (outcome in transformations$Outcome) {
  #Extract recommended transformation for outcome variable   
  transform_recommendation = (transformations %>% filter(Outcome == outcome))[1,2]
  
  #Retrieve the function object
  transform = match.fun(transform_recommendation)
  
  
  outcome_transformed = paste0(outcome, "_", transform_recommendation)
  #Transformed outcome and pulling out the transformed data
  merged[,outcome_transformed] = transform(merged[[outcome]])$x.t
}



p1 <- ggplot(merged, aes(x=muc2plantGHPL)) +
  theme(text = element_text(size=14,family = "mont")) +
  geom_density(fill="#47818d", color="#e9ecef", alpha=0.8) +
  labs(x= "Muc2plant", y="Density",
       tag = "A")

p2 <- ggplot(merged,aes(y=muc2plantGHPL_log_x,x=bmi_final))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=13)),
        axis.title.x = element_text(margin=margin(t=15)))+
  #scale_y_continuous(expansion(c(0,0.08)))+
  scale_color_identity() +
  xlim(NA,43.9) +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    label.y = 4,
    label.x = 30,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 4.5) +
  labs(y = "Muc2plant", 
       x = "BMI",
       tag = "B") 

p3 <- merged %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  ggplot(aes(x = sex, y = muc2plantGHPL, fill = sex))+
  geom_boxplot()+ 
  theme(text = element_text(family = "mont", size=12),
        axis.title.y = element_text(margin=margin(r=12)),
        axis.title.x = element_text(margin=margin(t=15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#47818d","#f14902"), guide = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +

  labs(
       y = "Muc2plant",
       x = "sex",
       tag = "C") +
  stat_compare_means(method = "wilcox.test", label.x = 1, label.y = .077, size=4.5, vjust = .99)


#Plotting residuals
ratio_resid = resid(lm(data = merged, muc2plantGHPL ~ age + sex + bmi_final))
cal_resid = resid(lm(data = merged, fecal_calprotectin_yeojohnson ~ age + sex + bmi_final))


p4 <- ggplot(data = NULL, aes(x=ratio_resid,y=cal_resid))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    label.x = -0.005,
    label.y = 4.4,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(
       tag = "D",
       x = "Muc2plant",
       y = "Calprotectin") 

p4

# There aren't neopterin measures for everyone
merged_neopt <- subset(merged, !is.na(merged[,"fecal_neopterin"]))
ratio_neopt_resid = resid(lm(data = merged_neopt, muc2plantGHPL ~ age + sex + bmi_final))
neopt_resid = resid(lm(data = merged_neopt, fecal_neopterin_yeojohnson ~ age + sex + bmi_final))


p5 <- ggplot(data = NULL, aes(x=ratio_neopt_resid,y=neopt_resid))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=14,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  #scale_color_manual(values = "#464646") +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    label.y = 2.6,
    label.x = -.005,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(
       x = "Muc2plant",
       y = "Neopterin",
       tag = "E") 

p5

fig4 <- grid.arrange(p1, p2, p3,p4,p5, clip="off",
                 #widths=c(1,1,1),
                 layout_matrix = rbind(c(1,1,2,3),
                                       c(1,1,2,3),
                                       c(4,4,5,5),
                                       c(4,4,5,5)))

ggsave("figure_4.tiff", device = "tiff", dpi = 300, width = 14, height = 8, units = "in", path = "output", fig4)


