# Adapted from Dr. Stephanie Wilson

# Create a function to find the best transformation for each taxonomic class.

# Requirements
## data = dataframe that includes as columns, each taxonomic class, each outcome, and covariates of interest
## class_names = names of columns to include the screening
# Outputs
## Dataframe with 2 columns, containing each variable and the recommended transformation.

find_transformation = function(data, class_names){
  
  # Create an empty list to store the results
  finaloutput = data.frame()
  
  for (class in class_names) {
    # Call bestNormalize function for each class, don't want to allow ordernorm
    result =  bestNormalize(data[[class]], allow_orderNorm = FALSE,)
    
    chosentransformation = result$norm_stats
    df = data.frame(
      transformation = names(chosentransformation),
      values = as.numeric(chosentransformation),
      stringsAsFactors = FALSE) %>%
      filter(values == min(values))
    
    # Store the results in the list
    output = data.frame(
      Outcome = class,
      transformation = df$transformation)
    
    finaloutput = rbind(finaloutput, output)
  }
  return(finaloutput)
}  