# title: supplemental_figures
# author: Sarah Blecksmith
# purpose: generate plots for the supplemental figures


library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(bestNormalize)
library(gridExtra)
library(showtext)
library(gt)
source('scripts/functions/looped_regression.R')

font_add_google("Montserrat", "mont")
showtext_auto()

# load the dietary and microbiome data
FL100_data <- read.csv("data/FL100_merged_variables.csv", header = TRUE) 
microbiome_data <- read.csv("data/microbiome_merged_variablesGH_GHPL.csv", header = TRUE) 
fiber_groups <- read.csv("data/fiber_groups_all.csv", header = TRUE)
GHPL_diversity <- read.csv("data/cazyme_GHPL_rounded_diversity.csv", header = TRUE)
stool <- read.csv("data/FL100_stool_variables.txt", header = TRUE, sep = "\t") %>%
  select(c(subject_id, AfterV2, diff_time_hrs))
#stool$subject_id <-  as.character(stool$subject_id)
plant_families_unrounded <- read.csv("data/plant_families_unrounded.csv", header = TRUE, check.names = FALSE) 
plant_families_rounded <- read.csv("data/plant_families_rounded.csv", header = TRUE, check.names = FALSE) 

# merge the data
merged = FL100_data %>%
  right_join(microbiome_data, by = "subject_id") %>%
  mutate(sex_num = sex) %>%
  mutate(sex = ifelse(sex==1, "male", "female")) %>%
  mutate(sex = factor(sex))

# load the transformations
microbiome_transformations <- read.csv("data/microbiome_transformations.csv", header = TRUE) 

# Transform the variables
for (outcome in microbiome_transformations$Outcome) {
  #Extract recommended transformation for outcome variable   
  transform_recommendation = (microbiome_transformations %>% filter(Outcome == outcome))[1,2]
  
  #Retrieve the function object
  transform = match.fun(transform_recommendation)
  
  outcome_transformed = paste0(outcome, "_norm")
  #Transformed outcome and pulling out the transformed data
  merged[,outcome_transformed] = transform(merged[[outcome]])$x.t
}

# Figure S1 - CONSORT diagram for Nutritional phenotyping study


# Table S1 - Characteristics of participants (n = 285) with fecal shotgun metagenomes from stool samples provided within 24 hours and not collected after the standard meal in study visit 2.
#number of men and women
men = sum(merged$sex_num == 1) #men = 142
women = sum(merged$sex_num == 2) #women = 143

# age groups women
women_age1 = sum(merged$sex_num == 2 & merged$age_cat == 1) # 51
women_age2 = sum(merged$sex_num == 2 & merged$age_cat == 2) # 46
women_age3 = sum(merged$sex_num == 2 & merged$age_cat == 3) # 46


# age groups men
men_age1 = sum(merged$sex_num == 1 & merged$age_cat == 1) # 44
men_age2 = sum(merged$sex_num == 1 & merged$age_cat == 2) # 53
men_age3 = sum(merged$sex_num == 1 & merged$age_cat == 3) # 45


#bmi groups women
women_age1_bmi1 = sum(merged$sex_num == 2 & merged$age_cat == 1 & merged$bmi_cat == 1) # 18
women_age1_bmi2 = sum(merged$sex_num == 2 & merged$age_cat == 1 & merged$bmi_cat == 2) # 18
women_age1_bmi3 = sum(merged$sex_num == 2 & merged$age_cat == 1 & merged$bmi_cat == 3) # 15

women_age2_bmi1 = sum(merged$sex_num == 2 & merged$age_cat == 2 & merged$bmi_cat == 1) # 17
women_age2_bmi2 = sum(merged$sex_num == 2 & merged$age_cat == 2 & merged$bmi_cat == 2) # 13
women_age2_bmi3 = sum(merged$sex_num == 2 & merged$age_cat == 2 & merged$bmi_cat == 3) # 16

women_age3_bmi1 = sum(merged$sex_num == 2 & merged$age_cat == 3 & merged$bmi_cat == 1) # 15
women_age3_bmi2 = sum(merged$sex_num == 2 & merged$age_cat == 3 & merged$bmi_cat == 2) # 23
women_age3_bmi3 = sum(merged$sex_num == 2 & merged$age_cat == 3 & merged$bmi_cat == 3) # 8

#bmi groups men
men_age1_bmi1 = sum(merged$sex_num == 1 & merged$age_cat == 1 & merged$bmi_cat == 1) # 20
men_age1_bmi2 = sum(merged$sex_num == 1 & merged$age_cat == 1 & merged$bmi_cat == 2) # 16
men_age1_bmi3 = sum(merged$sex_num == 1 & merged$age_cat == 1 & merged$bmi_cat == 3) # 8

men_age2_bmi1 = sum(merged$sex_num == 1 & merged$age_cat == 2 & merged$bmi_cat == 1) # 20
men_age2_bmi2 = sum(merged$sex_num == 1 & merged$age_cat == 2 & merged$bmi_cat == 2) # 19
men_age2_bmi3 = sum(merged$sex_num == 1 & merged$age_cat == 2 & merged$bmi_cat == 3) # 14

men_age3_bmi1 = sum(merged$sex_num == 1 & merged$age_cat == 3 & merged$bmi_cat == 1) # 18
men_age3_bmi2 = sum(merged$sex_num == 1 & merged$age_cat == 3 & merged$bmi_cat == 2) # 20
men_age3_bmi3 = sum(merged$sex_num == 1 & merged$age_cat == 3 & merged$bmi_cat == 3) # 7


# make dataframe for table
Sex <- c(paste0("Female (n=", women, ")"), "-", "-", "-", "-","-", "-", "-", "-",
         paste0("Male (n=", men, ")"), "-", "-", "-", "-","-", "-", "-", "-")

Age <- c(paste0("18-33 (n=", women_age1, ")"), "-", "-", 
         paste0("34-49 (n=", women_age2, ")"),"-", "-",
         paste0("50-65 (n=", women_age3, ")"), "-", "-", 
         paste0("18-33 (n=", men_age1, ")"), "-", "-", 
         paste0("34-49 (n=", men_age2, ")"),"-", "-",
         paste0("50-65 (n=", men_age3, ")"), "-", "-")

BMI <- c(paste0("<25 (n=",women_age1_bmi1, ")"),
         paste0("25-29.9 (n=",women_age1_bmi2, ")"),
         paste0(">30 (n=",women_age1_bmi3, ")"),
         paste0("<25 (n=",women_age2_bmi1, ")"),
         paste0("25-29.9 (n=",women_age2_bmi2, ")"),
         paste0(">30 (n=",women_age2_bmi3, ")"),
         paste0("<25 (n=",women_age3_bmi1, ")"),
         paste0("25-29.9 (n=",women_age3_bmi2, ")"),
         paste0(">30 (n=",women_age3_bmi3, ")"),
         paste0("<25 (n=",men_age1_bmi1, ")"),
         paste0("25-29.9 (n=",men_age1_bmi2, ")"),
         paste0(">30 (n=",men_age1_bmi3, ")"),
         paste0("<25 (n=",men_age2_bmi1, ")"),
         paste0("25-29.9 (n=",men_age2_bmi2, ")"),
         paste0(">30 (n=",men_age2_bmi3, ")"),
         paste0("<25 (n=",men_age3_bmi1, ")"),
         paste0("25-29.9 (n=",men_age3_bmi2, ")"),
         paste0(">30 (n=",men_age3_bmi3, ")"))

age_sex_bmi <- data.frame(Sex, Age, BMI)


tableS1 <-  age_sex_bmi %>% 
  gt() %>%
  text_transform(locations = cells_body(columns = c(Age, Sex)),
                 fn = function(x) {
                   ifelse(x=="-", " ", x)}
  ) %>%
  gtsave("table_S1.html", path = "output")


# Figure S2 - CAZyme abundance by age, sex, BMI
GHPL_sex <- ggplot(merged, aes(x = sex, y = GHPLabundance, fill = sex))+
  geom_boxplot()+ 
  theme(text = element_text(size=12)) +
  labs(title = "GH + PL CAZyme Abundance by Sex",
       y = "GH + PL CAZyme Abundance",
       x = "Sex") +
  scale_fill_viridis_d(guide = "none") +
  stat_compare_means(method = "wilcox.test", label.x = 1.3)

GHPL_bmi <- ggplot(merged,aes(y=GHPLabundance_norm, x=bmi_final))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "GH + PL CAZyme Abundance by BMI",
       y = "GH + PL CAZyme Abundance",
       x = "BMI")

GHPL_age <- ggplot(merged,aes(y=GHPLabundance_norm, x=age))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "GH + PL CAZyme abundance by age",
       y = "GH + PL CAZyme abundance",
       x = "Age")

figS2 <- grid.arrange(GHPL_sex, GHPL_age, GHPL_bmi, clip="off", ncol = 3)
ggsave("figure_S2.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS2)


# Figure S3 - range of rounded plant cazyme families
# The rounded families (robust abundance)
row.names(plant_families_rounded) <- plant_families_rounded$cazy_fam
plant_families_rounded <- as.data.frame(t(plant_families_rounded[,-1]))
plant_families_rounded$subject_id <- as.numeric(row.names(plant_families_rounded))

# merge the data
plant_families_rounded <- plant_families_rounded %>%
  left_join(stool, by = "subject_id") %>%
  filter(diff_time_hrs < 24 & AfterV2 == 0)

rownames(plant_families_rounded) = plant_families_rounded$subject_id
plant_families_rounded <- plant_families_rounded %>% select(-c(subject_id, diff_time_hrs, AfterV2))

plant_families_rounded$count <- rowSums(plant_families_rounded != 0)
min(plant_families_rounded$count) # 5
max(plant_families_rounded$count) # 34


plant_families_rounded$subject_id = rownames(plant_families_rounded)
figS3 <- plant_families_rounded %>% ggplot() +
  geom_bar(aes(reorder(subject_id, count), count, fill = count), stat = "identity") +
  labs(y="plant cazyme count", x = "Subject ID", title = "Range of rounded plant unique CAZyme families/subfamilies")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

ggsave("figure_S3.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS3)


# Figure S4 - range of Shannon and Chao1 Diversity of GH and PL families
# range of Shannon rounded
GHPL_diversity <- GHPL_diversity %>%
  left_join(stool, by = "subject_id") %>%
  filter(diff_time_hrs < 24 & AfterV2 == 0)

shannon <- GHPL_diversity %>% ggplot() +
  geom_bar(aes(reorder(subject_id, Shannon), Shannon, fill = Shannon), stat = "identity") +
  #scale_fill_viridis_c(guide = "none") +
  labs(y="Shannon Diversity", x = "Subject ID", title = "Range of rounded GH and PL family Shannon Diversity",
       tag = "A")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


# range of Chao1
chao1 <- GHPL_diversity %>% ggplot() +
  geom_bar(aes(reorder(subject_id, Chao1), Chao1, fill = Chao1), stat = "identity") +
  #scale_fill_viridis_c(guide = "none") +
  labs(y="Chao1 Diversity", x = "Subject ID", title = "Range of rounded GH and PL family Chao1 Diversity",
       tag = "B")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

figS4 <- grid.arrange(shannon, chao1, clip="off", ncol = 2)
ggsave("figure_S4.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS4)


# Figure S5 - Plant Shannon and Chao1 by sex
shannon_sex <- ggplot(merged, aes(x = sex, y = Shannon, fill = sex))+
  geom_boxplot()+ 
  theme(text = element_text(size=15)) +
  labs(title = "Shannon plant cazyme diversity by sex",
       y = "Shannon plant cazyme diversity",
       x = "sex", 
       tag = "A") +
  scale_fill_viridis_d(guide = "none") +
  stat_compare_means(method = "wilcox.test", label.x = 1.3)

chao1_sex <- ggplot(merged, aes(x = sex, y = Chao1, fill = sex))+
  geom_boxplot()+ 
  theme(text = element_text(size=15)) +
  labs(title = "Chao1 plant cazyme diversity by sex",
       y = "Chao1 plant cazyme diversity",
       x = "sex",
       tag = "B") +
  scale_fill_viridis_d(guide = "none") +
  stat_compare_means(method = "wilcox.test", label.x = 1.3)
figS5 <- grid.arrange(shannon_sex, chao1_sex, clip="off", ncol = 2)
ggsave("figure_S5.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS5)


# Figure S6 - Plant Shannon and Chao1 by BMI
shannon_bmi <- ggplot(merged,aes(y=Shannon_norm, x=bmi_final))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Shannon plant cazyme diversity by BMI",
       y = "Shannon plant cazyme diversity",
       x = "BMI",
       tag = "A")

chao1_bmi <- ggplot(merged,aes(y=Chao1_norm, x=bmi_final))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Chao1 plant cazyme diversity by BMI",
       y = "Chao1 plant cazyme diversity",
       x = "BMI",
       tag = "B")

figS6 <- grid.arrange(shannon_bmi, chao1_bmi, clip="off", ncol = 2)
ggsave("figure_S6.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS6)


# Figure S7 - Plant Shannon and Chao1 by age
shannon_age <- ggplot(merged,aes(y=Shannon_norm, x=age))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Shannon plant cazyme diversity by age",
       y = "Shannon plant cazyme diversity",
       x = "age",
       tag = "A")

chao1_age <- ggplot(merged,aes(y=Chao1_norm, x=age))+
  geom_point(aes(colour = "#464646"))+
  theme(text = element_text(size=12,family = "mont"),
        axis.title.y = element_text(margin=margin(r=15)),
        axis.title.x = element_text(margin=margin(t=15))) +
  scale_color_identity() +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "#f14902") +
  stat_poly_eq(
    geom = "text",
    hjust = 0,
    aes(label = paste(after_stat(p.value.label),
                      after_stat(n.label),
                      after_stat(rr.label), sep = "*\", \"*")),
    size = 5) +
  labs(title = "Chao1 plant cazyme diversity by age",
       y = "Chao1 plant cazyme diversity",
       x = "age",
       tag = "B")

figS7 <- grid.arrange(shannon_age, chao1_age, clip="off", ncol = 2)
ggsave("figure_S7.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS7)


# Table S2 – Fiber variables and plant-unique CAZyme Shannon diversity, Chao1 diversity and abundance, adjusted for age, sex and BMI
tableS2_variables = c("Shannon", "Chao1", "plant_cazyme_abundance")
fiber_variables = c("avg_fibe_tnfs", "perKcal_fiber_tnfs", "dt_fibe", "dt_fiber_sol", "fibe_perKcal", "sol_fibe_perKcal")

# read in saved transformations
microbiome_transformations <- read.csv('data/microbiome_transformations.csv', header = TRUE)

# run models
fiber_models <- looped_regression(merged, tableS2_variables, fiber_variables, microbiome_transformations, FALSE) 

table_S2 <- fiber_models %>%
  mutate(outcome_transform = paste0(Outcome, " (transform: ", Transformation, ")")) %>%
  mutate(Variable = case_when(Variable == "avg_fibe_tnfs" ~ "ASA24 total fiber",
                              Variable == "perKcal_fiber_tnfs" ~ "ASA24 energy adjusted total fiber",
                              Variable == "dt_fibe"  ~ "FFQ total fiber",
                              Variable == "dt_fiber_sol"  ~ "FFQ soluble fiber",
                              Variable == "fibe_perKcal"  ~ "FFQ energy adjusted total fiber",
                              Variable == "sol_fibe_perKcal"  ~  "FFQ energy adjusted soluble fiber",
                              Variable == "hei_asa24_totalscore"  ~  "ASA24 HEI total score",
                              Variable == "hei_ffq_totalscore"  ~  "FFQ HEI total score",
                              Variable == "dii_totalscore"  ~  "DII total score",
                              Variable == "faiths_diversity_fiber"  ~  "Faith's diversity of fiber",
                              Variable == "faiths_diversity_carb"  ~ "Faith's diversity of carbohydrates",
                              Variable == "faiths_diversity_diet"  ~  "Faith's diversity of diet",
                              Variable == "fecal_ph" ~ "Fecal pH")) %>%
  select(-c(Outcome, Transformation)) %>%
  gt(groupname_col = "outcome_transform") %>%
  tab_header(
    title = md("**Transformed microbiome variables and dietary fiber measures**")) %>%
  tab_style(
    style = (
      cell_text(weight = "bold")
    ),
    location = cells_row_groups()) %>%
  tab_style(
    style = (cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      rows = P_Value < 0.05
    )
  ) %>%
  fmt_number(decimals = 3) %>%
  gtsave("table_S2.html", path = "output")



# Table S3 - Phylogenetic diversity of whole diet, carbohydrates and fiber with plant-unique Shannon and Chao1 diversity, adjusted for age, sex and BMI

tableS3_variables = c("Shannon", "Chao1", "plant_cazyme_abundance")
diet_variables = c("faiths_diversity_diet", "faiths_diversity_carb", "faiths_diversity_fiber")

# run models
diet_models <- looped_regression(merged, tableS3_variables, diet_variables, microbiome_transformations, FALSE) 

table_S3 <- diet_models %>%
  mutate(outcome_transform = paste0(Outcome, " (transform: ", Transformation, ")")) %>%
  mutate(Variable = case_when(Variable == "avg_fibe_tnfs" ~ "ASA24 total fiber",
                              Variable == "perKcal_fiber_tnfs" ~ "ASA24 energy adjusted total fiber",
                              Variable == "dt_fibe"  ~ "FFQ total fiber",
                              Variable == "dt_fiber_sol"  ~ "FFQ soluble fiber",
                              Variable == "fibe_perKcal"  ~ "FFQ energy adjusted total fiber",
                              Variable == "sol_fibe_perKcal"  ~  "FFQ energy adjusted soluble fiber",
                              Variable == "hei_asa24_totalscore"  ~  "ASA24 HEI total score",
                              Variable == "hei_ffq_totalscore"  ~  "FFQ HEI total score",
                              Variable == "dii_totalscore"  ~  "DII total score",
                              Variable == "faiths_diversity_fiber"  ~  "Faith's diversity of fiber",
                              Variable == "faiths_diversity_carb"  ~ "Faith's diversity of carbohydrates",
                              Variable == "faiths_diversity_diet"  ~  "Faith's diversity of diet",
                              Variable == "fecal_ph" ~ "Fecal pH")) %>%
  select(-c(Outcome, Transformation)) %>%
  gt(groupname_col = "outcome_transform") %>%
  tab_header(
    title = md("**Transformed microbiome variables and dietary diversity measures**")) %>%
  tab_style(
    style = (
      cell_text(weight = "bold")
    ),
    location = cells_row_groups()) %>%
  tab_style(
    style = (cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      rows = P_Value < 0.05
    )
  ) %>%
  fmt_number(decimals = 3) %>%
  gtsave("table_S3.html", path = "output")



# Table S4 - ASA24 and FFQ HEI scores with plant-unique Shannon and Chao1 diversity, adjusted for age, sex and BMI
tableS4_variables = c("Shannon", "Chao1", "plant_cazyme_abundance")
hei_variables = c("hei_asa24_totalscore", "hei_ffq_totalscore", "dii_totalscore")

# run models
hei_models <- looped_regression(merged, tableS4_variables, hei_variables, microbiome_transformations, FALSE) 

table_S4 <- hei_models %>%
  mutate(outcome_transform = paste0(Outcome, " (transform: ", Transformation, ")")) %>%
  mutate(Variable = case_when(Variable == "avg_fibe_tnfs" ~ "ASA24 total fiber",
                              Variable == "perKcal_fiber_tnfs" ~ "ASA24 energy adjusted total fiber",
                              Variable == "dt_fibe"  ~ "FFQ total fiber",
                              Variable == "dt_fiber_sol"  ~ "FFQ soluble fiber",
                              Variable == "fibe_perKcal"  ~ "FFQ energy adjusted total fiber",
                              Variable == "sol_fibe_perKcal"  ~  "FFQ energy adjusted soluble fiber",
                              Variable == "hei_asa24_totalscore"  ~  "ASA24 HEI total score",
                              Variable == "hei_ffq_totalscore"  ~  "FFQ HEI total score",
                              Variable == "dii_totalscore"  ~  "DII total score",
                              Variable == "faiths_diversity_fiber"  ~  "Faith's diversity of fiber",
                              Variable == "faiths_diversity_carb"  ~ "Faith's diversity of carbohydrates",
                              Variable == "faiths_diversity_diet"  ~  "Faith's diversity of diet",
                              Variable == "fecal_ph" ~ "Fecal pH")) %>%
  select(-c(Outcome, Transformation)) %>%
  gt(groupname_col = "outcome_transform") %>%
  tab_header(
    title = md("**Transformed microbiome variables and dietary HEI scores**")) %>%
  tab_style(
    style = (
      cell_text(weight = "bold")
    ),
    location = cells_row_groups()) %>%
  tab_style(
    style = (cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      rows = P_Value < 0.05
    )
  ) %>%
  fmt_number(decimals = 3) %>%
  gtsave("table_S4.html", path = "output")





# Table S7. Inflammatory variables and plant-unique CAZyme Shannon diversity, Chao1 diversity and abundance, adjusted for age, sex and BMI
tableS7_variables = c("Shannon", "Chao1", "plant_cazyme_abundance")
inflamm_variables = c("fecal_calprotectin", "fecal_neopterin", "fecal_mpo", "plasma_lbp")

# read in saved transformations
inflammation_transformations <- read.csv('data/inflammation_transformations.csv', header = TRUE)

# run models
inflamm_models <- looped_regression(merged, inflamm_variables, tableS7_variables, inflammation_transformations, FALSE) 

table_S5 <- inflamm_models %>%
  mutate(Outcome = case_when(Outcome == "fecal_calprotectin" ~ "Fecal calprotectin",
                             Outcome == "fecal_mpo" ~ "Fecal myeloperoxidase",
                             Outcome == "fecal_neopterin" ~ "Fecal neopterin",
                             Outcome == "plasma_lbp" ~ "Plasma lipopolysaccharide binding protein"),
         Variable = case_when(Variable == "plant_cazyme_abundance" ~ "Plant CAZyme abundance",
                              Variable == "Shannon" ~ "Plant CAZyme Shannon diversity",
                              Variable == "Chao1" ~ "Plant CAZyme Chao1 diversity")) %>%
  mutate(outcome_transform = paste0(Outcome, " (transform: ", Transformation, ")")) %>%
  select(-c(Outcome, Transformation)) %>%
  gt(groupname_col = "outcome_transform") %>%
  tab_header(
    title = md("**Transformed inflammatory variables and microbiome variables**")) %>%
  tab_style(
    style = (
      cell_text(weight = "bold")
    ),
    location = cells_row_groups()) %>%
  tab_style(
    style = (cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      rows = P_Value < 0.05
    )
  ) %>%
  fmt_number(decimals = 3) %>%
  gtsave("table_S5.html", path = "output")


# Figure S8 difference in muc2plant by menopause status
menstrual <- read.csv("data/CTSC24532USDAWHNRCNu-MenstruationData_DATA_2022-12-15_1413.csv", header = TRUE)
merged = FL100_data %>%
  right_join(microbiome_data, by = "subject_id") %>%
  right_join(menstrual, by = 'subject_id')

mutate(sex = factor(sex))

figS8 <- merged %>%
  filter(!is.na(screen_endmenstr)) %>%
  mutate(screen_endmenstr = ifelse(screen_endmenstr == 1, "Non-menopausal", "Menopausal")) %>%
  ggplot(aes(x = screen_endmenstr, y = muc2plantGHPL, fill = screen_endmenstr))+
  geom_boxplot() + 
  theme(text = element_text(family = "mont", size=12),
        axis.title.y = element_text(margin=margin(r=20)),
        axis.title.x = element_text(margin=margin(t=15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#47818d","#f14902"), guide = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(title = "Mucin:Plant by Menopausal Status",
       y = "Mucin:Plant",
       x = "Menopausal Status",
       tag = "C") +
  stat_compare_means(method = "wilcox.test", label.x = 1.3, label.y = .077, size=6, vjust = .99)

ggsave("figure_S8.tiff", device = "tiff", dpi = 300, width = 12, height = 5, units = "in", path = "output", figS8)


# Table S6 mucin:plant and dietary variables
merged$muc2plantGHPL_sulf <- (merged$sulfatase_rpkg + merged$mucin_family_total)/merged$plant_family_totalGHPL

tableS6_variables = c("muc2plantGHPL")
diet_variables = c("avg_fibe_tnfs", "perKcal_fiber_tnfs", "dt_fibe", "dt_fiber_sol", "fibe_perKcal", "sol_fibe_perKcal", "hei_asa24_totalscore", "hei_ffq_totalscore", "dii_totalscore", "faiths_diversity_fiber", "faiths_diversity_carb", "faiths_diversity_diet", "fecal_ph")

# read in saved transformations
microbiome_transformations <- read.csv('data/microbiome_transformations.csv', header = TRUE)
# Adding a few extra normalizations
bestNormalize(merged$muc2plantGHPL_sulf)
microbiome_transformations <- rbind(microbiome_transformations, c("muc2plantGHPL_sulf", "boxcox"))

# run models
diet_models <- looped_regression(merged, tableS8_variables, diet_variables, microbiome_transformations, FALSE) 

table_S6 <- diet_models %>%
  mutate(Outcome = case_when(Outcome == "muc2plantGHPL" ~ "mucin:plant", TRUE ~ Outcome)) %>%
  mutate(Variable = case_when(Variable == "avg_fibe_tnfs" ~ "ASA24 total fiber",
                              Variable == "perKcal_fiber_tnfs" ~ "ASA24 energy adjusted total fiber",
                              Variable == "dt_fibe"  ~ "FFQ total fiber",
                              Variable == "dt_fiber_sol"  ~ "FFQ soluble fiber",
                              Variable == "fibe_perKcal"  ~ "FFQ energy adjusted total fiber",
                              Variable == "sol_fibe_perKcal"  ~  "FFQ energy adjusted soluble fiber",
                              Variable == "hei_asa24_totalscore"  ~  "ASA24 HEI total score",
                              Variable == "hei_ffq_totalscore"  ~  "FFQ HEI total score",
                              Variable == "dii_totalscore"  ~  "DII total score",
                              Variable == "faiths_diversity_fiber"  ~  "Faith's diversity of fiber",
                              Variable == "faiths_diversity_carb"  ~ "Faith's diversity of carbohydrates",
                              Variable == "faiths_diversity_diet"  ~  "Faith's diversity of diet",
                              Variable == "fecal_ph" ~ "Fecal pH")) %>%
  mutate(outcome_transform = paste0(Outcome, " (transform: ", Transformation, ")")) %>%
  select(-c(Outcome, Transformation)) %>%
  gt(groupname_col = "outcome_transform") %>%
  tab_header(
    title = md("**Transformed microbiome variables and diet measures**")) %>%
  tab_style(
    style = (
      cell_text(weight = "bold")
    ),
    location = cells_row_groups()) %>%
  tab_style(
    style = (cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      rows = P_Value < 0.05
    )
  ) %>%
  fmt_number(decimals = 3) %>%
  gtsave("table_S6.html", path = "output")


# Table S7 - muc2plant with sulfatases and inflammatory variables

tableS7_variables = c("muc2plantGHPL_sulf")
inflamm_variables = c("fecal_calprotectin", "fecal_neopterin", "fecal_mpo", "plasma_lbp")


# run models
inflamm_models <- looped_regression(no_seaweed, inflamm_variables, tableS7_variables, inflammation_transformations, FALSE) 

table_S7 <- inflamm_models %>%
  mutate(Outcome = case_when(Outcome == "fecal_calprotectin" ~ "Fecal calprotectin",
                             Outcome == "fecal_mpo" ~ "Fecal myeloperoxidase",
                             Outcome == "fecal_neopterin" ~ "Fecal neopterin",
                             Outcome == "plasma_lbp" ~ "Plasma lipopolysaccharide binding protein"),
         Variable = case_when(Variable == "muc2plantGHPL_sulf" ~ "Mucin and sulfatase to plant ratio")) %>%
  mutate(outcome_transform = paste0(Outcome, " (transform: ", Transformation, ")")) %>%
  select(-c(Outcome, Transformation)) %>%
  gt(groupname_col = "outcome_transform") %>%
  tab_header(
    title = md("**Transformed inflammatory variables and microbiome variables**")) %>%
  tab_style(
    style = (
      cell_text(weight = "bold")
    ),
    location = cells_row_groups()) %>%
  tab_style(
    style = (cell_fill(color = "#F9E3D6")
    ),
    locations = cells_body(
      rows = P_Value < 0.05
    )
  ) %>%
  fmt_number(decimals = 3) 
table_S7 %>%
gtsave("table_S7_sulfatases.html", path = "output")


