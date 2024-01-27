# Axonal transport cell means
# Author: Nataliya Trushina	
# Start date: 2021-01-01
# Last modified: 2024-01-14
# Description: Postprocess IMARIS collection output for axonal transport analysis
# Requirements: "merged_table_*.csv" files in the working directory from "01a_merge_multiple_tables_track_analysis.py"

# Libraries
library(tidyverse)
library(scales)
library(ggpubr)

# ---------------------------------------------------------------------------- #

# Settings
color_condition <- colorRampPalette(c("#601891", "#04279A"))
color_mobility_direction <- colorRampPalette(c("gray", "black"))

# if renaming is required for plotting
names_conditions_renamed <- c("A53T", "WT")  # check the order in line print(names_conditions_filenames)

# Set working directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("path/to/files")

# Define custom theme
theme_custom <- theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.ticks = element_line(colour = "black")
  )

# Functions
save_plot <- function(plot, file_name, width, height) {
  ggsave(paste0(file_name, ".svg"), plot = plot, width = width, height = height)
  ggsave(paste0(file_name, ".png"), plot = plot, width = width, height = height)
}

# ---------------------------------------------------------------------------- #

# Read all files with selected name ending and extension
file_names <- Sys.glob(file.path("merged_table_*"))

# Initialize data frame and condition list
dat_merged <- data.frame()
names_conditions_filenames <- c()

# Loop through all files
for (i in file_names){  
  dat <- read.table(i, sep = ";",header = TRUE)
  
  condition_name <- gsub(".csv", "",i)
  condition_name <- gsub("merged_table_", "",condition_name)
  print(condition_name)

  dat$Condition <- condition_name
  
  names_conditions_filenames <- append(names_conditions_filenames, condition_name)
  
  dat_merged <- rbind(dat_merged, dat)
}

print(names_conditions_filenames)

dat_merged <- dat_merged %>%
  dplyr::mutate(Condition = factor(Condition, levels = names_conditions_filenames, labels = names_conditions_renamed)) %>%
  dplyr::mutate(Original.Image.Name = factor(Original.Image.Name))

# ---------------------------------------------------------------------------- #

# Categorize elements as 'Mobile' or 'Immobile' based on 'Track.Displacement.X.Reference.Frame' values,
# with borderline elements included in 'Stationary' (Mobile if > 0.75 or < -0.75, otherwise Immobile).
# Borderline elements are included into stationary
dat_merged <- dat_merged %>%
  mutate(Mobility = case_when(
    Track.Displacement.X.Reference.Frame > 0.75 | Track.Displacement.X.Reference.Frame < -0.75 ~ "Mobile",
    TRUE ~ "Immobile"
  )) %>%
  mutate(Mobility = factor(Mobility, levels = c("Mobile", "Immobile")))

# similar logic for directional transport
dat_merged <- dat_merged %>%
  mutate(Directionality = case_when(
    Track.Displacement.X.Reference.Frame > 0.75 ~ "Anterograde",
    Track.Displacement.X.Reference.Frame < -0.75 ~ "Retrograde",
    TRUE ~ "Stationary"
  )) %>%
  mutate(Directionality = factor(Directionality, levels = c("Anterograde", "Stationary", "Retrograde")))

# ---------------------------------------------------------------------------- #

palette_condition <- color_condition(nlevels(dat_merged$Condition))
palette_mobility <- color_mobility_direction(nlevels(dat_merged$Mobility))
palette_direction <- color_mobility_direction(nlevels(dat_merged$Directionality))

# ---------------------------------------------------------------------------- #

p_percent_mobility <- ggplot(dat_merged, aes(x = Condition)) +
  geom_bar(aes(color = Condition, fill = Mobility), position = "fill", width = 0.5, linewidth = 1) +
  scale_color_manual(values = palette_condition) +
  scale_fill_manual(values = palette_mobility) +
  theme_custom +
  labs(x = NULL, y = "Fraction of total") +
  scale_y_continuous(limits = c(0,1),breaks=seq(0, 1, 0.2))
p_percent_mobility

save_plot(p_percent_mobility, "percentage_mobility", width = 2.5, height = 2.2)

# ---------------------------------------------------------------------------- #

p_percent_direction <- ggplot(dat_merged, aes(x = Condition)) +
  geom_bar(aes(color = Condition, fill = Directionality), position="fill", width = 0.5, linewidth = 1) +
  scale_color_manual(values = palette_condition) +
  scale_fill_manual(values = palette_direction) +
  theme_custom +
  labs(x = NULL, y = "Fraction of total") +
  scale_y_continuous(limits = c(0,1),breaks=seq(0, 1, 0.2))
p_percent_direction

save_plot(p_percent_direction, "percentage_directionality", width = 2.5, height = 2.2)

# ---------------------------------------------------------------------------- #

write.csv2(dat_merged, "all_track_table_all_conditions.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #

# Filter data for mobile elements
dat_merged_mobile <- dat_merged[dat_merged$Mobility == "Mobile",]

# Calculate Track Quality as a percentage
dat_merged_mobile$Track_quality <- 100*dat_merged_mobile$Track.Number.of.Spots/
  ((dat_merged_mobile$Track.Duration+0.2)*5)

# Calculate Velocity and Processivity (no unit conversion needed as velocity is given in s, does not need to be changed to frames)
dat_merged_mobile$Velocity <- dat_merged_mobile$Track.Displacement.Length.Reference.Frame/dat_merged_mobile$Track.Duration
dat_merged_mobile$Processivity <- dat_merged_mobile$Track.Displacement.Length.Reference.Frame/dat_merged_mobile$Track.Length.Reference.Frame

# ---------------------------------------------------------------------------- #

write.csv2(dat_merged_mobile, "mobility_table_all_conditions.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #

generate_custom_ggplot <- function(data, y_var, y_label = "") {
  ggplot(data, aes(x = Condition, y = {{y_var}}, color = Condition)) + 
    geom_boxplot(width = 0.5, alpha = 0.2) + 
    geom_jitter(size = 1.5, width = 0.2) + 
    theme_custom +
    scale_color_manual(values = palette_condition) +
    stat_compare_means(method = "t.test") +
    labs(x = NULL, y = y_label) +
    scale_y_continuous(breaks = scales::pretty_breaks(10)) +
    expand_limits(y = 0)
}

p_track_quality <- generate_custom_ggplot(dat_merged_mobile, Track_quality, "Track quality")
p_track_quality

p_speed <- generate_custom_ggplot(dat_merged_mobile, Track.Speed.Mean.Reference.Frame, "Mean speed")
p_speed

p_velocity <- generate_custom_ggplot(dat_merged_mobile, Velocity, "Velocity")
p_velocity

p_processivity <- generate_custom_ggplot(dat_merged_mobile, Processivity, "Processivity")
p_processivity

# ---------------------------------------------------------------------------- #

dat_merged_to_average <- dat_merged_mobile[c("Original.Image.Name", "Track.Displacement.Length.Reference.Frame",
                                             "Track.Duration", "Track_quality",
                                             "Track.Length.Reference.Frame", 
                                             "Track.Speed.Mean.Reference.Frame", "Track.Speed.Max.Reference.Frame",
                                             "Velocity", "Processivity")]

df_for_plot_mean <- dat_merged_to_average %>% group_by(`Original.Image.Name`) %>% dplyr::summarise(across(everything(), list(mean))) 

colnames(df_for_plot_mean) <- c("Original.Image.Name", "Track.Displacement.Length.Reference.Frame",
                                "Track.Duration", "Track_quality",
                                "Track.Length.Reference.Frame", 
                                "Track.Speed.Mean.Reference.Frame", "Track.Speed.Max.Reference.Frame",
                                "Velocity", "Processivity")

dat_merged_mobile_means <- left_join(df_for_plot_mean, dat_merged_mobile[c("Original.Image.Name", "Condition")], by = "Original.Image.Name")
dat_merged_mobile_means <- dat_merged_mobile_means[!duplicated(dat_merged_mobile_means),]

# ---------------------------------------------------------------------------- #

write.csv2(dat_merged_mobile_means, "cell_means_table_all_conditions.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #

p_mean_track_quality <- generate_custom_ggplot(dat_merged_mobile_means, Track_quality, "Track quality")

p_mean_track_displacement_length <- generate_custom_ggplot(dat_merged_mobile_means, Track.Displacement.Length.Reference.Frame, "Track displacement length [\u03BCm]")

p_mean_track_duration <- generate_custom_ggplot(dat_merged_mobile_means, Track.Duration, "Track duration [s]")

p_mean_track_length <- generate_custom_ggplot(dat_merged_mobile_means, Track.Length.Reference.Frame, "Track length [\u03BCm]")

p_mean_speed <- generate_custom_ggplot(dat_merged_mobile_means, Track.Speed.Mean.Reference.Frame, "Mean speed [\u03BCm/s]")

p_max_speed <- generate_custom_ggplot(dat_merged_mobile_means, Track.Speed.Max.Reference.Frame, "Max speed [\u03BCm/s]")

p_mean_velocity <- generate_custom_ggplot(dat_merged_mobile_means, Velocity, "Velocity = displacement length / track duration")

p_mean_processivity <- generate_custom_ggplot(dat_merged_mobile_means, Processivity, "Processivity [ratio] = displacement length / track length")

p_empty <- ggplot()

# ---------------------------------------------------------------------------- #

all_plot <- ggarrange(p_mean_track_duration + labs(tag = "A"),
                      p_mean_track_length + labs(tag = "B"),
                      p_mean_track_displacement_length + labs(tag = "C"),
                      p_empty + labs(tag = ""),
                      p_mean_velocity + labs(tag = "D"),
                      p_mean_processivity + labs(tag = "E"),
                      p_mean_speed + labs(tag = "F"),
                      p_max_speed + labs(tag = "G"),
                      ncol = 4, nrow = 2,
                      common.legend = TRUE, legend = "bottom")

all_plot

save_plot(all_plot, "all_mean_plots_together", width = 10, height = 8)
