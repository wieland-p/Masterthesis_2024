########################################################################################################################
# Masterthesis PW
# Replication file for IV regression analysis and descriptive statistics 
########################################################################################################################
# Setup ####
rm(list = ls())
p_required <- c("tidyverse", "dplyr", "fixest", "cragg", 
                "modelsummary", "flextable", "tinytable")  
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

# load the dataset "MA_PW_data.csv"
d_pred_full <- read.csv2("",
                    stringsAsFactors = FALSE, 
                    header = TRUE)


########################################################################################################################
# data setups ####
# set NAs to zero for wind and create renewable energy per person variable
d_pred_full$wind_pop <- ifelse(is.na(d_pred_full$wind_pop), 0, d_pred_full$wind_pop)
d_pred_full$renew_pop <- d_pred_full$wind_pop + d_pred_full$pv_pop

# delete the top 5% of pv_pop (outliers)
d_pred <- d_pred_full %>%
  filter(pv_pop <= quantile(pv_pop, 0.95, na.rm = TRUE)) 

########################################################################################################################
# first stage and f-statistic (Section 5.2.1 & 5.2.2) ####
# Setting a dictionary 
setFixest_dict(c(pred = "Local Green Votes", ost = "East Germany",
                 city = "Urban Cluster", pv_pop = "PV Installation", res_pop = "Residential PV Installation",
                 renew_pop = "Renewables Installation", wind_pop = "Wind power Installation",
                 votediff = "Green Vote Differences", Forsa = "National Election Survey",
                 mun_pop = "PV Municipal Buildings"))


### First-Stage 
first_stage <- feols(votediff ~ Forsa*ost, data = d_pred, weights = d_pred$pop_insgesamt)
d_pred$pred <- predict(first_stage)
FS_interaction <- etable(first_stage, headers = c("First Stage"))

# OUTPUT to word
m_fs <- list(
  "I" = first_stage)

modelsummary(m_fs, stars = TRUE, gof_omit = "IC|RMSE|F", output = "table_fs.docx")
modelsummary(m_fs, stars = TRUE, gof_omit = "IC|RMSE", 
             output = "table_fs_head.docx", 
             title = "Dependent Variable: Local Green Vote Differences")

# F-statistic
d_pred$int_forsa_ost = d_pred$Forsa*d_pred$ost
stock_yogo_test(
  ~ost,  
  ~votediff, 
  ~Forsa,   
  B=.10,       
  size_bias="size", 
  data = d_pred)

########################################################################################################################
# Second Stage PV WIND RENEW (Section 5.2.3.1) ####
# run all nine second stage regressions (no_k = no control variables) 
pv_no_k <- feols(pv_pop ~ pred, data = d_pred)
pv_city <- feols(pv_pop ~ pred + city, data = d_pred)
pv_ost_city <- feols(pv_pop ~ pred + ost + city, data = d_pred)

wind_no_k <- feols(wind_pop ~ pred, data = d_pred)
wind_city <- feols(wind_pop ~ pred + city, data = d_pred)
wind_ost_city <- feols(wind_pop ~ pred + ost + city, data = d_pred)

renew_no_k <- feols(renew_pop ~ pred, data = d_pred)
renew_city <- feols(renew_pop ~ pred + city, data = d_pred)
renew_ost_city <- feols(renew_pop ~ pred + ost + city, data = d_pred)


# add several p_values using fit statistics
fitstat_register(type = "p_std", alias = "pvalue (Standard)",
                 fun = function(x) pvalue(x, vcov = "iid")["pred"])

fitstat_register(type = "p_kreis", alias = "pvalue (Kreis)",
                 fun = function(x) pvalue(x, vcov = ~kreis)["pred"])

# Reset the default values set in the previous sections
setFixest_etable(reset = TRUE)

# Display the results with the new fit statistics
general_table <- etable(pv_no_k, pv_city, pv_ost_city, 
                         wind_no_k, wind_city, wind_ost_city,
                         renew_no_k, renew_city, renew_ost_city,
                         fitstat = ~ . + p_std + p_kreis)

# export to WORD using modelsummary and tinytable
models_all <- list(
  "I" = pv_no_k,
  "II" = pv_city,
  "III" = pv_ost_city,
  "IV" = wind_no_k,
  "V" = wind_city,
  "VI" = wind_ost_city,
  "VII" = renew_no_k,
  "VIII" = renew_city,
  "IX" = renew_ost_city
)

savedir <- file.path("table_ss_all.docx")

modelsummary(models_all, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log") |>
  group_tt(j = list("PV installations" = 2:4, "Wind energy installations" = 5:7, 
                    "Renewable energy installations" = 8:10)) |> 
  save_tt(savedir, overwrite = TRUE)

########################################################################################################################
# Basic OLS PV WIND RENEW (Appendix A)  ####

ols_pv_no_k <- feols(pv_pop ~ votediff, data = d_pred)
ols_pv_city <- feols(pv_pop ~ votediff + city, data = d_pred)
ols_pv_ost_city <- feols(pv_pop ~ votediff + ost + city, data = d_pred)

ols_wind_no_k <- feols(wind_pop ~ votediff, data = d_pred)
ols_wind_city <- feols(wind_pop ~ votediff + city, data = d_pred)
ols_wind_ost_city <- feols(wind_pop ~ votediff + ost + city, data = d_pred)

ols_renew_no_k <- feols(renew_pop ~ votediff, data = d_pred)
ols_renew_city <- feols(renew_pop ~ votediff + city, data = d_pred)
ols_renew_ost_city <- feols(renew_pop ~ votediff + ost + city, data = d_pred)

# Display the results 
ols_table <- etable(ols_pv_no_k, ols_pv_city, ols_pv_ost_city, 
                         ols_wind_no_k, ols_wind_city, ols_wind_ost_city,
                         ols_renew_no_k, ols_renew_city,ols_renew_ost_city,
                         fitstat = ~ . + p_std + p_kreis)


# export the second stage regression table to latex
etable(ols_pv_no_k, ols_pv_city, ols_pv_ost_city, 
       ols_wind_no_k, ols_wind_city, ols_wind_ost_city,
       ols_renew_no_k, ols_renew_city,ols_renew_ost_city,
       fitstat = ~ . + p_std + p_kreis, tex = TRUE)

# OUTPUT to word
models_ols <- list(
  "I" = ols_pv_no_k,
  "II" = ols_pv_city,
  "III" = ols_pv_ost_city,
  "IV" = ols_wind_no_k,
  "V" = ols_wind_city,
  "VI" = ols_wind_ost_city,
  "VII" = ols_renew_no_k,
  "VIII" = ols_renew_city,
  "IX" = ols_renew_ost_city
)

savedir <- file.path("table_ols.docx")

modelsummary(models_ols, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log") |>
  group_tt(j = list("PV installations" = 2:4, "Wind energy installations" = 5:7, 
                    "Renewable energy installations" = 8:10)) |> 
  save_tt(savedir, overwrite = TRUE)


########################################################################################################################
# Second Stage Municipal Building PV (Section 5.2.3.3) ####
# run all nine second stage regressions (no_k = no control variables)
mun_no_k <- feols(mun_pop ~ pred, data = d_pred)
mun_city <- feols(mun_pop ~ pred + city, data = d_pred)
mun_ost_city <- feols(mun_pop ~ pred + ost + city, data = d_pred)

# Display the results with the new fit statistics
mun_table <- etable(mun_no_k, mun_city, mun_ost_city,
                         fitstat = ~ . + p_std + p_kreis)

# OUTPUT to word
models_mun <- list(
  "XIV" = mun_no_k,
  "XV" = mun_city,
  "XVI" = mun_ost_city)

savedir <- file.path("table_ss_mun.docx")

modelsummary(models_mun, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log") |>
  group_tt(j = list("PV installations" = 2:4)) |> 
  save_tt(savedir, overwrite = TRUE)

########################################################################################################################
# Second Stage Residential PV (Section 5.2.3.2) ####
# run all nine second stage regressions (no_k = no control variables)
res_no_k <- feols(res_pop ~ pred, data = d_pred)
res_city <- feols(res_pop ~ pred + city, data = d_pred)
res_ost_city <- feols(res_pop ~ pred + ost + city, data = d_pred)

# Display the results with the new fit statistics
res_table <- etable(res_no_k, res_city, res_ost_city,
                    fitstat = ~ . + p_std + p_kreis)

# OUTPUT to word
models_res <- list(
  "X" = res_no_k,
  "XI" = res_city,
  "XII" = res_ost_city)

savedir <- file.path("table_ss_res.docx")

modelsummary(models_res, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log") |>
  group_tt(j = list("Residential PV" = 2:4)) |> 
  save_tt(savedir, overwrite = TRUE)

########################################################################################################################
########################################################################################################################
########################################################################################################################
# Descriptive statistics (Section 5.1) ####
########################################################################################################################
# PV/pop 95% sample ####

# Calculate summary statistics
summary_stats <- summary(d_pred$pv_pop)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred$pv_pop)
sd_pv_pop <- sd(d_pred$pv_pop)
median_pv_pop <- median(d_pred$pv_pop)
percentile_25 <- quantile(d_pred$pv_pop, 0.25)
percentile_75 <- quantile(d_pred$pv_pop, 0.75)
min_pv_pop <- min(d_pred$pv_pop)
max_pv_pop <- max(d_pred$pv_pop)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "pv_pop_distr.docx")

########################################################################################################################
# PV/pop full sample ####
# Calculate summary statistics
summary_stats <- summary(d_pred_full$pv_pop)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred_full$pv_pop)
sd_pv_pop <- sd(d_pred_full$pv_pop)
median_pv_pop <- median(d_pred_full$pv_pop)
percentile_25 <- quantile(d_pred_full$pv_pop, 0.25)
percentile_75 <- quantile(d_pred_full$pv_pop, 0.75)
min_pv_pop <- min(d_pred_full$pv_pop)
max_pv_pop <- max(d_pred_full$pv_pop)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "pv_pop_full_distr.docx")

########################################################################################################################
# PV/residential 95% sample ####
# Calculate summary statistics
summary_stats <- summary(d_pred$res_pop)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred$res_pop)
sd_pv_pop <- sd(d_pred$res_pop)
median_pv_pop <- median(d_pred$res_pop)
percentile_25 <- quantile(d_pred$res_pop, 0.25)
percentile_75 <- quantile(d_pred$res_pop, 0.75)
min_pv_pop <- min(d_pred$res_pop)
max_pv_pop <- max(d_pred$res_pop)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "res_pop_distr.docx")

########################################################################################################################
# PV/municipal buildings 95% sample ####
# Calculate summary statistics
summary_stats <- summary(d_pred$mun_pop)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred$mun_pop)
sd_pv_pop <- sd(d_pred$mun_pop)
median_pv_pop <- median(d_pred$mun_pop)
percentile_25 <- quantile(d_pred$mun_pop, 0.25)
percentile_75 <- quantile(d_pred$mun_pop, 0.75)
min_pv_pop <- min(d_pred$mun_pop)
max_pv_pop <- max(d_pred$mun_pop)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "mun_pop_distr.docx")
########################################################################################################################
# Wind 95% sample ####
# Calculate summary statistics
summary_stats <- summary(d_pred$wind_pop)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred$wind_pop)
sd_pv_pop <- sd(d_pred$wind_pop)
median_pv_pop <- median(d_pred$wind_pop)
percentile_25 <- quantile(d_pred$wind_pop, 0.25)
percentile_75 <- quantile(d_pred$wind_pop, 0.75)
min_pv_pop <- min(d_pred$wind_pop)
max_pv_pop <- max(d_pred$wind_pop)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "wind_pop_distr.docx")
########################################################################################################################
# Green Votes 95% sample ####
# Calculate summary statistics
summary_stats <- summary(d_pred$greenvote_local)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred$greenvote_local)
sd_pv_pop <- sd(d_pred$greenvote_local)
median_pv_pop <- median(d_pred$greenvote_local)
percentile_25 <- quantile(d_pred$greenvote_local, 0.25)
percentile_75 <- quantile(d_pred$greenvote_local, 0.75)
min_pv_pop <- min(d_pred$greenvote_local)
max_pv_pop <- max(d_pred$greenvote_local)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "greenvote_distr.docx")

########################################################################################################################
# Votediff 95% sample ####
# Calculate summary statistics
summary_stats <- summary(d_pred$votediff)

# Extract the statistics you need
mean_pv_pop <- mean(d_pred$votediff)
sd_pv_pop <- sd(d_pred$votediff)
median_pv_pop <- median(d_pred$votediff)
percentile_25 <- quantile(d_pred$votediff, 0.25)
percentile_75 <- quantile(d_pred$votediff, 0.75)
min_pv_pop <- min(d_pred$votediff)
max_pv_pop <- max(d_pred$votediff)

# Create a data frame to store the summary statistics
summary_df <- data.frame(
  Mean = mean_pv_pop,
  SD = sd_pv_pop,
  Median = median_pv_pop,
  Percentile_25 = percentile_25,
  Percentile_75 = percentile_75,
  Minimum = min_pv_pop,
  Maximum = max_pv_pop
)

# Export the summary statistics to a Word document
ft <- flextable(summary_df)
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()
save_as_docx(ft, path = "votediff_distr.docx")

########################################################################################################################
# Votediff 95% sample for 2011 and 2009 ####
# Filter the data for the year 2011
d_pred_2011 <- d_pred %>% filter(year == 2011)

# Calculate summary statistics for the year 2011
mean_pv_pop_2011 <- mean(d_pred_2011$votediff)
sd_pv_pop_2011 <- sd(d_pred_2011$votediff)
median_pv_pop_2011 <- median(d_pred_2011$votediff)
percentile_25_2011 <- quantile(d_pred_2011$votediff, 0.25)
percentile_75_2011 <- quantile(d_pred_2011$votediff, 0.75)
min_pv_pop_2011 <- min(d_pred_2011$votediff)
max_pv_pop_2011 <- max(d_pred_2011$votediff)

# Create a data frame to store the summary statistics for 2011
summary_df_2011 <- data.frame(
  Year = 2011,
  Mean = mean_pv_pop_2011,
  SD = sd_pv_pop_2011,
  Median = median_pv_pop_2011,
  Percentile_25 = percentile_25_2011,
  Percentile_75 = percentile_75_2011,
  Minimum = min_pv_pop_2011,
  Maximum = max_pv_pop_2011
)

# Filter the data for the year 2009
d_pred_2009 <- d_pred %>% filter(year == 2009)

# Calculate summary statistics for the year 2009
mean_pv_pop_2009 <- mean(d_pred_2009$votediff)
sd_pv_pop_2009 <- sd(d_pred_2009$votediff)
median_pv_pop_2009 <- median(d_pred_2009$votediff)
percentile_25_2009 <- quantile(d_pred_2009$votediff, 0.25)
percentile_75_2009 <- quantile(d_pred_2009$votediff, 0.75)
min_pv_pop_2009 <- min(d_pred_2009$votediff)
max_pv_pop_2009 <- max(d_pred_2009$votediff)

# Create a data frame to store the summary statistics for 2009
summary_df_2009 <- data.frame(
  Year = 2009,
  Mean = mean_pv_pop_2009,
  SD = sd_pv_pop_2009,
  Median = median_pv_pop_2009,
  Percentile_25 = percentile_25_2009,
  Percentile_75 = percentile_75_2009,
  Minimum = min_pv_pop_2009,
  Maximum = max_pv_pop_2009
)

# Combine the two summary data frames
summary_df <- bind_rows(summary_df_2011, summary_df_2009)

# Create a flextable object for the combined summary statistics
ft <- flextable(summary_df) %>%
  set_table_properties(width = .5, layout = "autofit") %>%
  theme_zebra()

# Export the flextable to a Word document
save_as_docx(ft, path = "votediff_distr_2009_2011.docx")
########################################################################################################################
# Renewables by state ####
# Aggregate data by state
state_agg <- d_pred %>%
  group_by(land) %>%
  summarise(wind_pop = mean(wind_pop), pv_pop = mean(pv_pop))

# Reshape data to long format
state_long <- state_agg %>%
  pivot_longer(cols = c(wind_pop, pv_pop), names_to = "type", values_to = "kW")

# Create a named vector for the mapping
state_abbreviations <- c("Baden-Württemberg" = "BW", 
                         "Bayern" = "BY",
                         "Berlin" = "BE",
                         "Brandenburg" = "BB",
                         "Bremen" = "HB",
                         "Hamburg" = "HH",
                         "Hessen" = "HE",
                         "Mecklenburg-Vorpommern" = "MV",
                         "Niedersachsen" = "NI",
                         "Nordrhein-Westfalen" = "NW",
                         "Rheinland-Pfalz" = "RP",
                         "Saarland" = "SL",
                         "Sachsen" = "SN",
                         "Sachsen-Anhalt" = "ST",
                         "Schleswig-Holstein" = "SH",
                         "Thüringen" = "TH")

# Replace German state names with abbreviations in the data frame
state_long$land <- state_abbreviations[state_long$land]

# horizontal stacked bar plot 
ggplot(state_long, aes(x = land, y = kW, fill = type)) +
  geom_bar(stat = "identity") +
  labs(x = "State",
       y = "kW") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(name = "Type",
                    values = c("wind_pop" = "blue", "pv_pop" = "orange"),
                    labels = c("wind_pop" = "Wind", "pv_pop" = "PV"))

########################################################################################################################
