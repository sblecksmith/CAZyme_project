# title: figure_3
# author: Sarah Blecksmith
# purpose: use DESeq2 to determine differentially abundant cazymes between groups of adequate 
# and low fiber consumers

library(phyloseq)
library(DESeq2)
library(dplyr)
library(ggplot2)
library(viridis)
library(reshape2)
library(tibble)
library(ggpubr)
library(gridExtra)
library(showtext)



fiber_groups <- read.csv("data/fiber_groups.csv", header = TRUE,  stringsAsFactors = FALSE, check.names = FALSE) %>%
  select(c(subject_id, fiber_group))

raw_families <- read.csv("data/raw_cazyme_families.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

cazy_genes <- read.csv("data/cazyme_raw_gene_table_330.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

stool <- read.csv("data/FL100_stool_variables.txt", header = TRUE, sep = "\t") %>%
  select(c(subject_id, AfterV2, diff_time_hrs))

################################################################################
# differentially abundant CAZyme families
################################################################################

# need to transpose family df and make a subject id column
row.names(raw_families) <- raw_families$cazy_fam
raw_families <- as.data.frame(t(raw_families[,-1]))
raw_families$subject_id <- row.names(raw_families)
raw_families$subject_id <- as.numeric(raw_families$subject_id)
fiber_groups$subject_id <- as.numeric(fiber_groups$subject_id)
# merge with stool data and fiber groups
families_merged <- raw_families %>%
  left_join(stool, by = "subject_id") %>%
  right_join(fiber_groups, by = "subject_id") 

# Filter out where AfterV2 = 0, we don't want to use the subjects who submitted stool after the test meal
families_merged <- families_merged %>% filter(diff_time_hrs < 24) %>%
                                        filter(AfterV2 ==0)
families_merged <- subset(families_merged, select=-c(diff_time_hrs, fiber_group, AfterV2))
#families_merged <- subset(families_merged, select=-c(diff_time_hrs, AfterV2))

# now re-transpose
row.names(families_merged) <- families_merged$subject_id
families_merged <- subset(families_merged, select=-c(subject_id))
families_merged <- as.data.frame(t(families_merged))

# make into matrix for phyloseq
families_merged <- as.matrix.data.frame(families_merged)

# make fiber groups our subject data
fiber_groups <- tibble::column_to_rownames(fiber_groups, "subject_id")
fiber_groups$fiber_group <- relevel(factor(fiber_groups$fiber_group), ref="low_fiber")
#fiber_groups$fiber_group <- relevel(factor(fiber_groups$fiber_group), ref="adequate_fiber")

# Make the phyloseq object
ps_CAZy_fam <- phyloseq(otu_table(families_merged, taxa_are_rows = TRUE), sample_data(fiber_groups))
# Convert to DESeqDataSet
ds_CAZy_fam <- phyloseq_to_deseq2(ps_CAZy_fam, ~fiber_group)
# run deseq2
ds_CAZy_fam <- DESeq(ds_CAZy_fam)
resultsNames(ds_CAZy_fam)
# log2(adequate_fiber/low_fiber)
res_CAZy_fam <- results(ds_CAZy_fam, contrast=c("fiber_group", "adequate_fiber", "low_fiber"))

res_CAZy_fam <- as.data.frame(res_CAZy_fam)
res_CAZy_fam$gene <- row.names(res_CAZy_fam)
#results_CAZy_fam <- res_CAZy_fam %>% filter(padj <= .05 & abs(log2FoldChange) > 2 & grepl("GH|PL", gene)) # 2 families
results_CAZy_fam <- res_CAZy_fam %>% filter(padj <= .05  & grepl("GH|PL", gene)) 
#write.csv(results_CAZy_fam, "data/diff_abund_CAZyme_families.csv", row.names = FALSE)

deseq2VST_fam <- varianceStabilizingTransformation(ds_CAZy_fam)
deseq2VST_fam <- assay(deseq2VST_fam)
deseq2VST_fam <- as.data.frame(deseq2VST_fam)
deseq2VST_fam$gene <- rownames(deseq2VST_fam)

# Select just the significant family genes
deseq2VST_fam <- deseq2VST_fam[deseq2VST_fam$gene %in% results_CAZy_fam$gene,]

# Now overwrite our original data frame with the long format
deseq2VST_fam <- melt(deseq2VST_fam, id.vars=c("gene"))
deseq2VST_fam <- dplyr::rename(deseq2VST_fam, subject_id = variable)
fiber_groups$subject_id <- rownames(fiber_groups)
deseq2VST_fam <- merge(x= deseq2VST_fam, y = fiber_groups, all.x = TRUE, by = 'subject_id')
deseq2VST_fam$fiber_group <- as.character(deseq2VST_fam$fiber_group)
deseq2VST_fam <- merge(x= deseq2VST_fam, y = results_CAZy_fam[,c("gene", "log2FoldChange", "padj", "baseMean")], all.x = TRUE, by = 'gene')


# Make a heatmap
family_heatmap <- deseq2VST_fam %>%
  ggplot(aes(x=subject_id, y=reorder(gene,log2FoldChange), fill=value)) + 
  geom_raster() + scale_fill_viridis(trans="sqrt") + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), element_text(size=30)) +
  labs(y="CAZyme Family", x = "Subject", 
       title = "CAZyme families differentially abundant between \nfiber consumption groups", tag = "A") +
  facet_grid(
  cols = vars(fiber_group), scales= "free", space = "free_y") +
  scale_y_discrete(expand = c(0, 0)) 
family_heatmap


prevdf = apply(X = otu_table(ps_CAZy_fam),
               MARGIN = ifelse(taxa_are_rows(ps_CAZy_fam), yes = 1, no = 2),
               FUN = function(x){sum(x > 0)})
#Add taxonomy and total read counts to this data.frame
prevdf = data.frame(Prevalence = prevdf,
                    TotalAbundance = taxa_sums(ps_CAZy_fam))

################################################################################
# differentially abundant CAZyme genes
################################################################################
fiber_groups <- read.csv("data/fiber_groups.csv", header = TRUE,  stringsAsFactors = FALSE, check.names = FALSE) %>%
  select(c(subject_id, fiber_group))

# Organize the data
# need to transpose cazy gene df and make a subject id column
row.names(cazy_genes) <- cazy_genes$family
cazy_genes <- as.data.frame(t(cazy_genes[,-1]))
cazy_genes$subject_id <- row.names(cazy_genes)
cazy_genes$subject_id <- as.numeric(cazy_genes$subject_id)
fiber_groups$subject_id <- as.numeric(fiber_groups$subject_id)

# merge with stool data and fiber groups
cazy_genes_merged <- cazy_genes %>%
  left_join(stool, by = "subject_id") %>%
  right_join(fiber_groups, by = "subject_id") 

# Filter out where AfterV2 = 0, we don't want to use the subjects who submitted stool after the test meal
cazy_genes_merged <- cazy_genes_merged %>% filter(diff_time_hrs < 24) %>% # 
  filter(AfterV2 ==0)

# 86 total
sum(cazy_genes_merged$fiber_group == "adequate_fiber") # 48
sum(cazy_genes_merged$fiber_group == "low_fiber") # 38

cazy_genes_merged <- subset(cazy_genes_merged, select=-c(diff_time_hrs, fiber_group, AfterV2))

# now re-transpose
row.names(cazy_genes_merged) <- cazy_genes_merged$subject_id
cazy_genes_merged<- subset(cazy_genes_merged, select=-c(subject_id))
cazy_genes_merged <- as.data.frame(t(cazy_genes_merged))


# Prep for phyloseq
# make into matrix for phyloseq
cazy_genes_merged <- as.matrix.data.frame(cazy_genes_merged)
# make fiber groups our subject data
fiber_groups <- tibble::column_to_rownames(fiber_groups, "subject_id")
fiber_groups$fiber_group <- relevel(factor(fiber_groups$fiber_group), ref="low_fiber")


# Make the phyloseq object
ps_CAZy_genes <- phyloseq(otu_table(cazy_genes_merged, taxa_are_rows = TRUE), sample_data(fiber_groups))
# Convert to DESeqDataSet
ds_CAZy_genes <- phyloseq_to_deseq2(ps_CAZy_genes, ~fiber_group)
# Run deseq2
ds_CAZy_genes <- DESeq(ds_CAZy_genes)
# log2(adequate_fiber/low_fiber)
res_CAZy_genes <- results(ds_CAZy_genes, contrast=c("fiber_group", "adequate_fiber", "low_fiber"))

res_CAZy_genes <- as.data.frame(res_CAZy_genes)
res_CAZy_genes$gene <- row.names(res_CAZy_genes)
#results_CAZy_genes <- res_CAZy_genes %>% filter(padj <= .05 & abs(log2FoldChange) > 2  & grepl("GH|PL", gene)) # 40
results_CAZy_genes <- res_CAZy_genes %>% filter(padj <= .05 & grepl("GH|PL", gene))




write.csv(results_CAZy_genes, "data/diff_abund_CAZyme_genes_list_long.csv", row.names = FALSE)

deseq2VST_genes <- vst(ds_CAZy_genes)
deseq2VST_genes <- assay(deseq2VST_genes)
deseq2VST_genes <- as.data.frame(deseq2VST_genes)
deseq2VST_genes$gene <- rownames(deseq2VST_genes)

# Select just the significant family genes
deseq2VST_genes <- deseq2VST_genes[deseq2VST_genes$gene %in% results_CAZy_genes$gene,]

# Now overwrite our original data frame with the long format
deseq2VST_genes <- melt(deseq2VST_genes, id.vars=c("gene"))
deseq2VST_genes <- dplyr::rename(deseq2VST_genes, subject_id = variable)
fiber_groups$subject_id <- rownames(fiber_groups)
deseq2VST_genes <- merge(x= deseq2VST_genes, y = fiber_groups, all.x = TRUE, by = 'subject_id')
deseq2VST_genes$fiber_group <- as.character(deseq2VST_genes$fiber_group)
deseq2VST_genes <- merge(x= deseq2VST_genes, y = results_CAZy_genes[,c("gene", "log2FoldChange", "padj", "baseMean")], all.x = TRUE, by = 'gene')


# Make a heatmap
gene_heatmap <- deseq2VST_genes %>%
  ggplot(aes(x=subject_id, y=reorder(gene,log2FoldChange), fill=value)) + 
  geom_raster() + scale_fill_viridis(trans="sqrt") + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), element_text(size=25)) + 
  labs(y="CAZyme Gene", x = "Subject", 
       title = "CAZyme genes differentially abundant between fiber consumption groups", tag = "B") +
  facet_grid(
    cols = vars(fiber_group), scales= "free", space = "free_y") +
  scale_y_discrete(expand = c(0, 0)) 
gene_heatmap


showtext_auto()
showtext_opts(dpi = 300)
fig3<- ggarrange(family_heatmap, gene_heatmap, common.legend = TRUE, widths = c(1,2), legend = "bottom")

ggsave("figure_3.tiff", device = "tiff", dpi = 300, width = 15, height = 6, units = "in", path = "output", bg = "white", fig3)

