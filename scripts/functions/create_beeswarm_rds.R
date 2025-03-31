# Create beeswarm plot function 

## Dr.Stephanie Wilson
## Loads an RDS environment, adjusts the feature names, and creates a beeswarm plot as output.

## Requires a feature list from RF models that has two columns
## Column 1 is feature, as formatted in dietML output
## Column 2 is feature_publish, which is a publishing-friendly format to the feature

create_beeswarm = function(path, feature_list) {
  loaded_env = load(path)
  
  # Extract the data with SHAP values
  sv_data = data.frame(feature = colnames(sv_full[[2]])) %>%
    left_join(feature_list, by = 'feature') 
  
  # Create the beeswarm plot
  plot = shapviz::sv_importance(sv_full, kind = "bee", show_numbers = TRUE,
                                bee_width = 0.2,  max_display = 5) +#, 
                                #show_other = FALSE) + 
    theme_bw(base_size = 18) + 
    labs(x = 'SHAP value') + 
    theme(plot.margin = margin(0, 0, 0, 0, "cm"))
  return(plot)
}