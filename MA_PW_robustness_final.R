########################################################################################################################
# Masterthesis PW
# Replication file for robustness check (section 5.3)
########################################################################################################################
# Setup ####
rm(list = ls())
p_required <- c("tidyverse", "dplyr",  "fixest", "ggplot2", 
                "tidyr", "patchwork")  
packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
########################################################################################################################
# load data ####

# set the correct wd
setwd("")

# load the dataset "MA_PW_data_robustness.csv"
d_model <- read.csv2("",
                     stringsAsFactors = FALSE, 
                     header = TRUE)

########################################################################################################################
# ROBUSTNESS CHECKS ####
########################################################################################################################
# GENERAL PV: ALL Models in one plot WITH CONF. INTERVALS ####

# first stage etc. 
d_model <- d_model %>%
  filter(inc_pv_2016 <= quantile(inc_pv_2016, 0.95, na.rm = TRUE)) 

first_stage <- feols(votediff ~ Forsa*ost, data = d_model, weights = d_model$pop_insgesamt)
d_model$pred <- predict(first_stage)
FS_interaction <- etable(first_stage, headers = c("First Stage"))

# turn values before 2009 positive instead of negative
d_model <- d_model %>%
  mutate(across(matches("^inc_.*200[4-8]$"), abs))

#check
head(d_model$inc_pv_2008)

# Create an empty dataframe to store results
results_df <- data.frame()

# Create a loop for years 2004 to 2022, excluding 2009
for (year in 2004:2022) {
  if (year != 2009) {
    col_name_pv_full <- paste0("inc_pv_", year)
    col_name_pv_reduced <- paste0("inc_pv_", year)
    col_name_pv_pred_only <- paste0("inc_pv_", year)
    
    formula_full <- as.formula(paste(col_name_pv_full, "~ pred + city + ost"))
    formula_reduced <- as.formula(paste(col_name_pv_reduced, "~ pred + city"))
    formula_pred_only <- as.formula(paste(col_name_pv_pred_only, "~ pred"))
    
    # Run the full model regression and store results with variable names
    result_full <- feols(formula_full, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the full model
    coefficients_full <- coef(result_full)["pred"]
    conf_intervals_full <- confint(result_full, level = 0.90)["pred", ]
    
    # Run the reduced model regression and store results with variable names
    result_reduced <- feols(formula_reduced, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the reduced model
    coefficients_reduced <- coef(result_reduced)["pred"]
    conf_intervals_reduced <- confint(result_reduced, level = 0.90)["pred", ]
    
    # Run the pred-only model regression and store results with variable names
    result_pred_only <- feols(formula_pred_only, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the pred-only model
    coefficients_pred_only <- coef(result_pred_only)["pred"]
    conf_intervals_pred_only <- confint(result_pred_only, level = 0.90)["pred", ]
    
    # Create a data frame for the current year's results for the full model
    result_df_full <- data.frame(
      Year = year,
      Model = "Full Model",
      coefficient = coefficients_full,
      lower_limit = conf_intervals_full[1],
      upper_limit = conf_intervals_full[2]
    )
    
    # Create a data frame for the current year's results for the reduced model
    result_df_reduced <- data.frame(
      Year = year,
      Model = "Reduced Model",
      coefficient = coefficients_reduced,
      lower_limit = conf_intervals_reduced[1],
      upper_limit = conf_intervals_reduced[2]
    )
    
    # Create a data frame for the current year's results for the pred-only model
    result_df_pred_only <- data.frame(
      Year = year,
      Model = "Green Party only Model",
      coefficient = coefficients_pred_only,
      lower_limit = conf_intervals_pred_only[1],
      upper_limit = conf_intervals_pred_only[2]
    )
    
    # Append the results to the main dataframe
    results_df <- rbind(results_df, result_df_full, result_df_reduced, result_df_pred_only)
  }
}

# Plot the regression coefficients for `pred` for all three models with 90% confidence intervals
general_pv <-
  ggplot(results_df, aes(x = Year, y = coefficient, color = Model, linetype = Model)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = X5.., ymax = X95..), width = 0.2) +
  labs(title = "General PV Installations",
       x = "Year",
       y = "Coefficient") +
  theme_minimal() 

########################################################################################################################
# MUNICIPAL PV: ALL Models in one plot WITH CONF. INTERVALS ####
# Create an empty dataframe to store results
results_df <- data.frame()

# Create a loop for years 2004 to 2022, excluding 2009
for (year in 2004:2022) {
  if (year != 2009) {
    col_name_pv_full <- paste0("inc_mun_", year)
    col_name_pv_reduced <- paste0("inc_mun_", year)
    col_name_pv_pred_only <- paste0("inc_mun_", year)
    
    formula_full <- as.formula(paste(col_name_pv_full, "~ pred + city + ost"))
    formula_reduced <- as.formula(paste(col_name_pv_reduced, "~ pred + city"))
    formula_pred_only <- as.formula(paste(col_name_pv_pred_only, "~ pred"))
    
    # Run the full model regression and store results with variable names
    result_full <- feols(formula_full, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the full model
    coefficients_full <- coef(result_full)["pred"]
    conf_intervals_full <- confint(result_full, level = 0.90)["pred", ]
    
    # Run the reduced model regression and store results with variable names
    result_reduced <- feols(formula_reduced, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the reduced model
    coefficients_reduced <- coef(result_reduced)["pred"]
    conf_intervals_reduced <- confint(result_reduced, level = 0.90)["pred", ]
    
    # Run the pred-only model regression and store results with variable names
    result_pred_only <- feols(formula_pred_only, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the pred-only model
    coefficients_pred_only <- coef(result_pred_only)["pred"]
    conf_intervals_pred_only <- confint(result_pred_only, level = 0.90)["pred", ]
    
    # Create a data frame for the current year's results for the full model
    result_df_full <- data.frame(
      Year = year,
      Model = "Full Model",
      coefficient = coefficients_full,
      lower_limit = conf_intervals_full[1],
      upper_limit = conf_intervals_full[2]
    )
    
    # Create a data frame for the current year's results for the reduced model
    result_df_reduced <- data.frame(
      Year = year,
      Model = "Reduced Model",
      coefficient = coefficients_reduced,
      lower_limit = conf_intervals_reduced[1],
      upper_limit = conf_intervals_reduced[2]
    )
    
    # Create a data frame for the current year's results for the pred-only model
    result_df_pred_only <- data.frame(
      Year = year,
      Model = "Green Party only Model",
      coefficient = coefficients_pred_only,
      lower_limit = conf_intervals_pred_only[1],
      upper_limit = conf_intervals_pred_only[2]
    )
    
    # Append the results to the main dataframe
    results_df <- rbind(results_df, result_df_full, result_df_reduced, result_df_pred_only)
  }
}

# Plot the regression coefficients for `pred` for all three models with 90% confidence intervals
municipal_pv <- 
  ggplot(results_df, aes(x = Year, y = coefficient, color = Model, linetype = Model)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = X5.., ymax = X95..), width = 0.2) +
  labs(title = "Municipally owned PV Installations",
       x = "Year",
       y = "Coefficient") +
  theme_minimal() +
  theme(legend.position = "none")

########################################################################################################################
# RESIDENTIAL PV: ALL Models in one plot WITH CONF. INTERVALS ####
# Create an empty dataframe to store results
results_df <- data.frame()

# Create a loop for years 2004 to 2022, excluding 2009
for (year in 2004:2022) {
  if (year != 2009) {
    col_name_pv_full <- paste0("inc_res_", year)
    col_name_pv_reduced <- paste0("inc_res_", year)
    col_name_pv_pred_only <- paste0("inc_res_", year)
    
    formula_full <- as.formula(paste(col_name_pv_full, "~ pred + city + ost"))
    formula_reduced <- as.formula(paste(col_name_pv_reduced, "~ pred + city"))
    formula_pred_only <- as.formula(paste(col_name_pv_pred_only, "~ pred"))
    
    # Run the full model regression and store results with variable names
    result_full <- feols(formula_full, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the full model
    coefficients_full <- coef(result_full)["pred"]
    conf_intervals_full <- confint(result_full, level = 0.90)["pred", ]
    
    # Run the reduced model regression and store results with variable names
    result_reduced <- feols(formula_reduced, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the reduced model
    coefficients_reduced <- coef(result_reduced)["pred"]
    conf_intervals_reduced <- confint(result_reduced, level = 0.90)["pred", ]
    
    # Run the pred-only model regression and store results with variable names
    result_pred_only <- feols(formula_pred_only, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the pred-only model
    coefficients_pred_only <- coef(result_pred_only)["pred"]
    conf_intervals_pred_only <- confint(result_pred_only, level = 0.90)["pred", ]
    
    # Create a data frame for the current year's results for the full model
    result_df_full <- data.frame(
      Year = year,
      Model = "Full Model",
      coefficient = coefficients_full,
      lower_limit = conf_intervals_full[1],
      upper_limit = conf_intervals_full[2]
    )
    
    # Create a data frame for the current year's results for the reduced model
    result_df_reduced <- data.frame(
      Year = year,
      Model = "Reduced Model",
      coefficient = coefficients_reduced,
      lower_limit = conf_intervals_reduced[1],
      upper_limit = conf_intervals_reduced[2]
    )
    
    # Create a data frame for the current year's results for the pred-only model
    result_df_pred_only <- data.frame(
      Year = year,
      Model = "Green Party only Model",
      coefficient = coefficients_pred_only,
      lower_limit = conf_intervals_pred_only[1],
      upper_limit = conf_intervals_pred_only[2]
    )
    
    # Append the results to the main dataframe
    results_df <- rbind(results_df, result_df_full, result_df_reduced, result_df_pred_only)
  }
}

# Plot the regression coefficients for `pred` for all three models with 90% confidence intervals
residential_pv <- 
  ggplot(results_df, aes(x = Year, y = coefficient, color = Model, linetype = Model)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = X5.., ymax = X95..), width = 0.2) +
  labs(title = "Residential PV Installations",
       x = "Year",
       y = "Coefficient") +
  theme_minimal() +
  theme(legend.position = "none")

########################################################################################################################
# WIND ENERGY: ALL Models in one plot WITH CONF. INTERVALS ####
# Create an empty dataframe to store results
results_df <- data.frame()

# Create a loop for years 2004 to 2022, excluding 2009
for (year in 2004:2022) {
  if (year != 2009) {
    col_name_pv_full <- paste0("inc_wind_", year)
    col_name_pv_reduced <- paste0("inc_wind_", year)
    col_name_pv_pred_only <- paste0("inc_wind_", year)
    
    formula_full <- as.formula(paste(col_name_pv_full, "~ pred + city + ost"))
    formula_reduced <- as.formula(paste(col_name_pv_reduced, "~ pred + city"))
    formula_pred_only <- as.formula(paste(col_name_pv_pred_only, "~ pred"))
    
    # Run the full model regression and store results with variable names
    result_full <- feols(formula_full, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the full model
    coefficients_full <- coef(result_full)["pred"]
    conf_intervals_full <- confint(result_full, level = 0.90)["pred", ]
    
    # Run the reduced model regression and store results with variable names
    result_reduced <- feols(formula_reduced, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the reduced model
    coefficients_reduced <- coef(result_reduced)["pred"]
    conf_intervals_reduced <- confint(result_reduced, level = 0.90)["pred", ]
    
    # Run the pred-only model regression and store results with variable names
    result_pred_only <- feols(formula_pred_only, data = d_model)
    
    # Extract coefficients and 90% confidence intervals for the `pred` variable from the pred-only model
    coefficients_pred_only <- coef(result_pred_only)["pred"]
    conf_intervals_pred_only <- confint(result_pred_only, level = 0.90)["pred", ]
    
    # Create a data frame for the current year's results for the full model
    result_df_full <- data.frame(
      Year = year,
      Model = "Full Model",
      coefficient = coefficients_full,
      lower_limit = conf_intervals_full[1],
      upper_limit = conf_intervals_full[2]
    )
    
    # Create a data frame for the current year's results for the reduced model
    result_df_reduced <- data.frame(
      Year = year,
      Model = "Reduced Model",
      coefficient = coefficients_reduced,
      lower_limit = conf_intervals_reduced[1],
      upper_limit = conf_intervals_reduced[2]
    )
    
    # Create a data frame for the current year's results for the pred-only model
    result_df_pred_only <- data.frame(
      Year = year,
      Model = "Green Party only Model",
      coefficient = coefficients_pred_only,
      lower_limit = conf_intervals_pred_only[1],
      upper_limit = conf_intervals_pred_only[2]
    )
    
    # Append the results to the main dataframe
    results_df <- rbind(results_df, result_df_full, result_df_reduced, result_df_pred_only)
  }
}

# Plot the regression coefficients for `pred` for all three models with 90% confidence intervals
wind <- 
  ggplot(results_df, aes(x = Year, y = coefficient, color = Model, linetype = Model)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = X5.., ymax = X95..), width = 0.2) +
  labs(title = "Wind Installations",
       x = "Year",
       y = "Coefficient") +
  theme_minimal() +
  theme(legend.position = "none")


########################################################################################################################
# combine all plots ####
combined_plot <- general_pv + residential_pv + municipal_pv + wind
# Display the combined plots
combined_plot

########################################################################################################################