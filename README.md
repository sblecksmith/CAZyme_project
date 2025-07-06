# Gut microbiome genes involved in plant and mucin breakdown correlate with diet and gastrointestinal inflammation in healthy US adults
## Sarah E. Blecksmith, Andrew Oliver, Zeynep Alkan and Danielle G. Lemay.

## Main Scripts

**1_CAZy_families.R** -- makes a table of aggregated CAZy families and subfamilies, rounded and unrounded.

**1_5_raw_CAZy_families.R** -- makes a table of aggregated un-normalized CAZy families and subfamilies

**2_CAZy_diversity.R** -- use phyloseq to calculate plant cazyme family diversity and GH/PL diversity. uses substrate 
annotation scheme from DOI: 10.1126/science.aan4834

**3_substrate_ratios.R** -- calculate the mucin to plant cazyme ratio using the unrounded aggregated CAZyme family table
and the families unique to one substrate output from CAZy_family_substrates.R

**4_plant_CAZyme_abundance.R** -- calculate the plant cazyme abundance using the normalized but unrounded aggregated CAZyme family table
and the families unique to one substrate output from CAZy_family_substrates.R

**CAZy_family_substrates.R** -- read in the family substrate annotation pdf, DOI: 10.1126/science.aan4834

**dietML_feature_formatting.R** -- adapted from Dr. Stephanie Wilson, summarizes and format feature names from dietML runs

**dietML_prep.R** -- prepares data for running Dr. Andrew Oliver's dietML

**match_sulfatases.R** -- collect sulfatase abundance from humann3 output, normalizes with Microbe Census to rpkg and calculates muc2plantGHPL_sulf

**merge_FL100_vartiables.R** -- adapted from Dr. Stephanie Wilson, pools FL100 data into one file

**reads_per_sample.R** -- calculate mean and SD of reads per sample

**taxaHFE_prep.R** -- generates files of quartiles of plant cazyme abundance, Shannon diversity, chao1 diversity and muc2plant plus covariates to run for taxaHFE. 
clean up food tree file

**taxaHFE_foods.R** -- looks at foods in the categories that appear in SHAP plots

## Functions from Dr. Stephanie Wilson:

**create_beeswarm_rds.R**

**extract_performance_metrics.R**

**extract_rds_data.R**

**find_rds.R**

**find_transformations.R**

**looped_regression.R**

## Figure generating scripts:

**figure_1.R**

**figure_2.R**

**figure_3.R**

**figure_4.R**

**figure_5.R**

**supplemental_figures.R**
