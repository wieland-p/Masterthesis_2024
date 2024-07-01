########################################################################################################################
# Masterthesis PW
# Replication file for Election poll data visualisation
########################################################################################################################
# Setup ####
rm(list = ls())
p_required <- c("ggplot2", "readr", "lubridate")  
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

# load the dataset "MA_PW_data_polls.csv"
data <- read.csv("",
                         stringsAsFactors = FALSE, 
                         header = TRUE)
########################################################################################################################
# Create the plot (section 4.1.4) ####
# Split the 'week' column into 'year' and 'week' components
data$year <- as.numeric(sub("_.*", "", data$week))
data$week_num <- as.numeric(sub(".*_", "", data$week))

# Create a date column representing the start of each week
data$start_date <- as.Date(paste(data$year, data$week_num, 1, sep = "-"), "%Y-%U-%u")

# Filter data for the specified time period
filtered_data <- subset(data, start_date >= as.Date("2009-01-01") & start_date <= as.Date("2011-12-31"))

# Create a list of weeks to mark and their corresponding labels
weeks_to_mark <- c("2009_23", "2009_35", "2011_07", "2011_12", "2011_20", "2011_36", "2011_37")
labels <- c("BW, MV, RP, SL, SN, ST, TH", "NW", "HH", "HE", "HB", "NI", "BE")

# Split weeks_to_mark into year and week components
years_to_mark <- as.numeric(sub("_.*", "", weeks_to_mark))
week_nums_to_mark <- as.numeric(sub(".*_", "", weeks_to_mark))

# Calculate the dates for the specific weeks to mark
dates_to_mark <- as.Date(paste(years_to_mark, week_nums_to_mark, 1, sep = "-"), "%Y-%U-%u")

# rename Green Party variable
filtered_data$GreenParty <- filtered_data$GRÜ_forsa

# Plot time series
ggplot(filtered_data, aes(x = start_date)) +
  geom_line(aes(y = GRÜ_forsa, color = "Green Party"), size = 1) +
  scale_color_manual(values = c("Green Party" = "green")) +
  labs(x = "Year",
       y = "Percent",
       color = "Legend") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ylim(0, 0.5) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 16), # Increase legend title size
    legend.text = element_text(size = 14), # Increase legend text size
    axis.title.x = element_text(size = 16), # Increase x-axis title size
    axis.title.y = element_text(size = 16), # Increase y-axis title size
    axis.text.x = element_text(size = 14), # Increase x-axis tick label size
    axis.text.y = element_text(size = 14) # Increase y-axis tick label size
  ) +
  geom_vline(xintercept = dates_to_mark, color = "red", linetype = "dashed") +
  annotate("text", x = dates_to_mark[1:5], y = 0.32, # Adjust y to be higher within the y-axis range
           label = labels[1:5], angle = 90, vjust = -0.5, hjust = 1, color = "red", size = 5) +
  annotate("text", x = dates_to_mark[6], y = 0.35, # Adjust y for the sixth annotation
           label = labels[6], angle = 90, vjust = -0.5, hjust = 1, color = "red", size = 5) +
  annotate("text", x = dates_to_mark[7:length(dates_to_mark)], y = 0.32, # Adjust y to be higher within the y-axis range
           label = labels[7:length(labels)], angle = 90, vjust = -0.5, hjust = 1, color = "red", size = 5)

########################################################################################################################