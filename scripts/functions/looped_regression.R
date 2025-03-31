# title: looped_regression
# author: Sarah Blecksmith, adapted from Dr. Stephanie Wilson

# Requirements
## data, long formatted data
## markers, a vector of outcomes you would like to run. 

# Outputs
## Dataframe with each predictor estimates, p-values, and adjusted p-values for each model. 

# Perform regression for each microbiome outcome with each predictor
looped_regression = function(data, outcomes, predictors, transformations, inflamm_control){
  
#library requirement
library(tidyverse)
library(bestNormalize)
  
# Create an empty list to store the regression results
finaloutput = data.frame()
  
  for (outcome in outcomes) {
    #Extract recommended transformation for outcome variable   
    transform_recommendation = (transformations %>% filter(Outcome == outcome))[1,2]
    
    #Retrieve the function object
    transform = match.fun(transform_recommendation)
    
    
    outcome_transformed = paste0(outcome, "_", transform_recommendation)
    #Transformed outcome and pulling out the transformed data
    data[,outcome_transformed] = transform(data[[outcome]])$x.t
    
    # want to filter out calprotectin > 100 if inflamm_flag = TRUE
    if (inflamm_control==TRUE) {
      data_for_models = data %>% filter(fecal_calprotectin < 100)
    } else {
      data_for_models = data
    }
    
  
    for (predictor in predictors) {
    
      # filter NA
      data_full = data_for_models %>% filter(!is.na(predictor))
      # Create the model
      formula = paste0(outcome_transformed, " ~ ", predictor, " + age + sex + bmi_final")
      model = lm(data = data_full, formula = formula)
    
    
      # Perform summary on each
      summary = summary(model)
    
      # Pull out the Coefficients
      coefficient <- summary$coefficients[predictor, "Estimate"]

    
      # Pull out the P values
      p_values <- summary$coefficients[predictor, "Pr(>|t|)"]
    
      # Pull out the r2 value
      r2 <- summary$r.squared
    
    
      # Store the results in the list
      output = data.frame(
        #Outcome_transformed = outcome_transformed,
        Outcome = outcome,
        Transformation = transform_recommendation,
        Variable = predictor,
        Coefficient = coefficient,
        R_Squared = r2,
        P_Value = round(p_values, digits = 5))
      
    
      finaloutput = rbind(finaloutput, output) %>%
        arrange(Outcome)
    }
  }
  return(finaloutput)
}
