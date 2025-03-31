# Extract Data from dietML RDS Files
# Dr. Stephanie Wilson

## This script pools output from multiple RDS files, 
## which are a key output file from the dietML program built by Dr. Andrew Oliver. 
## Each RDS file is a packaged R environment which contains numerous dataframes. 

## This script
### 1) Isolates the pathways for each of the RDS files created by one run of dietML and 
### 2) Iteratively extracts SHAP values and other essential data from each of the RDS files

## Inputs 
### list of RDS file paths from dietML output housed within a dataframe, rds_info
### Columns with Formatted Example
### path = HFE/LOHI/Full/CAL_dietML/output/ML_r_workspace.rds
### response_type = LOHI
### model = Full
### marker = CAL
### label = LOHI_Full_CAL

## Outputs
### Large List where each list contains data from one model including
### Input features, Feature Importance, Avg SHAP for each feature, model label


extract_data_from_rds = function(path) {
  
  # Load the RDS file and extract the environment
  load(path)
  
  # Extract the required dataframes
  input = input
  shap_explainations_full = shap_explainations_full
  
  # Calculate the Feature Importance
  MeanShaps = as.data.frame(colMeans(abs(shap_explainations_full))) %>%
    dplyr::rename('Importance' = 1)
  MeanShaps = rownames_to_column(MeanShaps, var = "feature") 
  
  Raw = as.data.frame(colMeans(shap_explainations_full)) %>%
    dplyr::rename('Avg_SHAP' = 1)
  Raw = rownames_to_column(Raw, var = "feature") 
  
  MeanShaps = left_join(MeanShaps, Raw, by = 'feature')
  
  # Extract the label from rds_info
  label = rds_info[rds_info$path == file_path, "label"]
  
  # Return a list with all the extracted data and the label
  return(list(input = input, shap_explainations_full = shap_explainations_full,
              MeanShaps = MeanShaps, label = label))
}

# Function to bind the label to the MeanShaps dataframe for each list
add_label_to_MeanShaps = function(data, label) {
  data$MeanShaps$label = label
  return(data$MeanShaps)
}