# Extract Metric Data from dietML RDS Files
# Dr. Stephanie Wilson

## This script pools performance metric output from multiple RDS files, 
## which are a key output file from the dietML program built by Dr. Andrew Oliver. 
## Each RDS file is a packaged R environment which contains numerous dataframes. 

## This script
### 1) Isolates the pathways for each of the RDS files created by one run of dietML and 
### 2) Iteratively extracts model performance metrics values and other essential data from each of the RDS files

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
### Dataframe on models performance metrics including a model label


extract_performance = function(path) {
  
  # Load the RDS file and extract the environment
  load(path)
  
  # Extract the label from rds_info
  label = rds_info[rds_info$path == file_path, "label"]
  
  # Extract the required dataframes
  metrics = final_res$.metrics
  metrics$label = label
  
  # Return a list with all the extracted data and the label
  return(data.frame(metrics = metrics))
}