# ---------------------------------------------------------------------------- #
# Analysis of APP trajectories
# Author: Nataliya Trushina	
# Start date: 2020-01-01
# Last modified: 2025-03-26

# Requirements: output from "2D_trajectory_analysis" csv tables in subdirectories
# ---------------------------------------------------------------------------- #

# Libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(rstatix)
library(plotly)

cbp <- c("#000000", "#A00000")

set.seed(42)

# Functions
merge_info_tables <- function(filenames) {
  data_merge <- data.frame()
  # Loop through all files
  for (i in filenames){ 
    print(i)
    x <- read.table(i, sep = ";",header = TRUE)
    x$division <- i
    data_merge <- rbind(data_merge, x)
  }
  return(data_merge)
}

theme_custom <- theme(
    plot.title = element_text(color = "black", size = 10),
    axis.ticks = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    
    strip.text = element_text(color = "black", size = 10)
  ) 

# ---------------------------------------------------------------------------- #
# State changes collected for individual cells

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read all files with selected name ending and extension
filenames <- Sys.glob(file.path("*_changes_final.csv"))

# Create data frame with collected information
data_merge_states <- merge_info_tables(filenames)

data_long_states_orig <- gather(data_merge_states, movement, measurement, RS:all_changes, factor_key=TRUE)

# Rename conditions and add treatment column
# data_long_states_orig <- data_long_states_orig %>%
#   dplyr::mutate(
#     treatment = dplyr::case_when(
#       condition %in% c('mCherry_aSynWT_DMSO', 'mCherry_aSynA53T_DMSO') ~ 'control',
#       condition %in% c('mCherry_aSynWT_SynD', 'mCherry_aSynA53T_SynD') ~ 'SynD'
#     ),
#     condition = dplyr::case_when(
#       condition %in% c('mCherry_aSynWT_DMSO', 'mCherry_aSynWT_SynD') ~ 'WT',
#       condition %in% c('mCherry_aSynA53T_DMSO', 'mCherry_aSynA53T_SynD') ~ 'A53T'
#     ),
#     condition = factor(condition, levels = c("WT", "A53T"))
#   )

names_conditions_filenames <- levels(as.factor(data_long_states_orig$condition))
print(names_conditions_filenames)
names_conditions_renamed <- c("PHP", "WT")
correct_order <- c("WT", "PHP")
my_comparisons <- list(correct_order)

data_long_states <- data_long_states_orig %>%
  dplyr::mutate(
    condition = factor(condition, levels = names_conditions_filenames, labels = names_conditions_renamed),
    condition = forcats::fct_relevel(condition, correct_order)
  )

# ---------------------------------------------------------------------------- #
# Possibly relevant comparisons

data_long_states <- data_long_states %>%
  dplyr::filter(movement %in% c("all_to_stop", "stop_to_all", "retr_to_stop_ant", "ant_to_stop_retr", "all_changes")) %>%
  dplyr::mutate(movement = forcats::fct_recode(
    movement,
    "All to stop" = "all_to_stop",
    "Stops to all" = "stop_to_all",
    "Retrograde to (stop or anterograde)" = "retr_to_stop_ant",
    "Anterograde to (stop or retrograde)" = "ant_to_stop_retr",
    "All changes" = "all_changes"
  ))

p_states_relevant <- ggplot(data_long_states, aes(x = condition, y = measurement)) +
  facet_grid(~ movement, labeller = label_wrap_gen(width = 15)) +
  geom_boxplot(width = 0.7, aes(fill = condition), alpha = 0.2, outliers = FALSE) +  # draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.3, size = 0.5, aes(color = condition), show.legend = FALSE) +  # position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Number of state changes relative\nto trajectory length") +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5), expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3.5) +  # label = "p.signif", 
  theme_classic() +
  theme_custom

p_states_relevant
ggsave(p_states_relevant, filename ="state_changes_relevant.png", width = 5.5, height = 3.5)
ggsave(p_states_relevant, filename ="state_changes_relevant.svg", width = 5.5, height = 3.5)

# ---------------------------------------------------------------------------- #
# Only "all_changes"

p_states_all_changes <- ggplot(data_long_states %>% dplyr::filter(movement %in% c("All changes")), aes(x = condition, y = measurement)) +
  facet_grid(~ movement, labeller = label_wrap_gen(width = 15)) +
  geom_boxplot(width = 0.7, aes(fill = condition), alpha = 0.2, outliers = FALSE) +  # draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.3, size = 0.5, aes(color = condition), show.legend = FALSE) +  # position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("All changes relative\nto trajectory length") +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5), expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3.5) +  # label = "p.signif", 
  theme_classic() +
  theme_custom

p_states_all_changes
ggsave(p_states_all_changes, filename ="state_changes_all_changes.png", width = 2, height = 3.5)
ggsave(p_states_all_changes, filename ="state_changes_all_changes.svg", width = 2, height = 3.5)

# # Two-way ANOVA
# res.aov2 <- aov(measurement ~ condition + treatment, data = data_long_states)
# summary(res.aov2)
# 
# # Two-way ANOVA with interaction effect
# # These two calls are equivalent
# res.aov3 <- aov(measurement ~ condition * treatment, data = data_long_states)
# summary(res.aov3)
# plot(res.aov3, 2)
# #https://www.datanovia.com/en/blog/how-to-add-p-values-to-ggplot-facets/
#   
# # Extract the residuals
# aov_residuals <- residuals(object = res.aov3)
# # Run Shapiro-Wilk test
# shapiro.test(x = aov_residuals )
