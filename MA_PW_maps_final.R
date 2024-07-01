########################################################################################################################
# Masterthesis PW
# Replication file for geographical data visualisation
########################################################################################################################
# clear environment & load packages ####
rm(list = ls())
p_required <- c("tidyverse", "dplyr", "sf", "ggplot2", 
                "RColorBrewer", "gridExtra", "patchwork")  
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
d_map <- read.csv2("",
                     stringsAsFactors = FALSE, 
                     header = TRUE)

# set NAs to 0 for wind increase per pop
d_map$wind_pop <- ifelse(is.na(d_map$wind_pop), 0, d_map$wind_pop)

# delete the top 5% of pv_pop (outliers)
d_map <- d_map %>%
  filter(pv_pop <= quantile(pv_pop, 0.95, na.rm = TRUE)) 

########################################################################################################################
# map setups ####
# source for Germany map data: https://hub.arcgis.com/datasets/esri-de-content::bundesländer-2021-mit-einwohnerzahl/explore?location=50.355462%2C11.932483%2C6.47

# load shapefile
germany_shapefile <- st_read("LAN_ew_21.shp")

# create sf object
d_map_sf <- st_as_sf(d_map, coords = c("Längengrad", "Breitengrad"), crs = st_crs(germany_shapefile))
########################################################################################################################
# local election results maps (Section 4.1.4) ####
# color scale
color_scale <- scale_color_gradient2(low = "darkred", mid = "lightyellow", high = "darkgreen", midpoint = 0, space = "Lab")

# plot map for 2009 coloured
plot_2009 <- ggplot() +
  geom_sf(data = germany_shapefile, fill = "white", color = "black") +
  geom_sf(data = d_map_sf[d_map_sf$year == 2009, ], aes(color = votediff, size = pop_insgesamt), show.legend = "point", alpha = 1) +  
  geom_sf(data = d_map_sf[d_map_sf$year == 2011, ], aes(size = pop_insgesamt), color = "grey", alpha = 0.3) +
  scale_size_continuous(name = "Population Size", breaks = c(5000, 10000, 50000, 100000, 500000, 1000000), 
                        labels = c("5k", "10k", "50k", "100k", "500k", "1M"), range = c(0.1, 10)) +
  color_scale +
  theme_void() +
  labs(title = "Vote Difference in German Municipalities 2009", color = "Vote Difference") +
  theme(legend.position = "bottom")

# plot map for 2011 coloured
plot_2011 <- ggplot() +
  geom_sf(data = germany_shapefile, fill = "white", color = "black") +
  geom_sf(data = d_map_sf[d_map_sf$year == 2011, ], aes(color = votediff, size = pop_insgesamt), show.legend = "point", alpha = 1) +  
  geom_sf(data = d_map_sf[d_map_sf$year == 2009, ], aes(size = pop_insgesamt), color = "grey", alpha = 0.3) +
  scale_size_continuous(name = "Population Size", breaks = c(5000, 10000, 50000, 100000, 500000, 1000000), 
                        labels = c("5k", "10k", "50k", "100k", "500k", "1M"), range = c(0.1, 10)) +
  color_scale +
  theme_void() +
  labs(title = "Vote Difference in German Municipalities in 2011", color = "Vote Difference") +
  theme(legend.position = "none")

### combine both plots 
plot_2009 + plot_2011

########################################################################################################################
# map of pv increase overall (Section 5.1.1) ####

breaks <- c(0, 0.5, 1 , 1.5, 2)

# color scale
pv_scale <- scale_color_gradient2(low = "lightblue", high = "darkblue", space = "Lab", breaks = breaks)

# plot
ggplot() +
  geom_sf(data = germany_shapefile, fill = "white", color = "black") +
  geom_sf(data = d_map_sf, aes(color = pv_pop, size = pop_insgesamt), show.legend = "point", alpha = 1) +  
  scale_size_continuous(name = "Population Size", breaks = c(5000, 10000, 50000, 100000, 500000, 1000000), 
                        labels = c("5k", "10k", "50k", "100k", "500k", "1M"), range = c(0.1, 10)) +
  pv_scale +
  theme_void() +
  labs(color = "PV/pop Increase") +
  theme(legend.position = "bottom")


########################################################################################################################
# map of sample cities (Section 4.2.1) ####
d_map_sf <- d_map_sf %>%
  mutate(pop_category = cut(pop_insgesamt, 
                            breaks = c(0, 5000, 10000, 50000, 100000, 500000, 1000000, Inf),
                            labels = c("0-5k", "5k-10k", "10k-50k", "50k-100k", "100k-500k", "500k-1M", ">1M"),
                            include.lowest = TRUE))

# Define a color palette for the categories
colors <- c("0-5k" = "lightyellow", "5k-10k" = "yellow", "10k-50k" = "lightgreen", 
            "50k-100k" = "green", "100k-500k" = "darkgreen", "500k-1M" = "blue", ">1M" = "darkblue")

# plot
ggplot() +
  # Plot the base map of Germany
  geom_sf(data = germany_shapefile, fill = "white", color = "black") +
  
  # Plot the municipalities with population size categories
  geom_sf(data = d_map_sf, 
          aes(size = pop_insgesamt, fill = pop_category), 
          shape = 21, 
          color = "black", 
          alpha = 1, 
          show.legend = "point") +
  
  # Add a manual fill scale for the population categories
  scale_fill_manual(values = colors, name = "Population Size: color") +
  
  # Scale the size of the points according to population size
  scale_size_continuous(name = "Population Size", 
                        breaks = c(5000, 10000, 50000, 100000, 500000, 1000000), 
                        labels = c("5k", "10k", "50k", "100k", "500k", "1M"), 
                        range = c(1, 10)) +
  
  # Use a minimal theme with no additional features
  theme_void() +
  
  # Increase the size of the colored dots in the legend
  guides(fill = guide_legend(override.aes = list(size = 6))) +
  
  # Position the legend to a more visible location
  theme(legend.position = "right")
########################################################################################################################