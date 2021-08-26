###############################################
##Nataliya Trushina, 2020##
##Analysis of APP trajectories##

#Requirements: csv tables in subdirectories
###############################################
#Libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(rstatix)
library(ggbeeswarm)
library(plotly)

cbp <- c("#034e61", "#a00000", "#502274")



#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("svg_plots_output_final")
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("20210704*/*_changes_final.csv"))
#make data frame to collect data into through the loop
data_merge_states <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  #x <- setNames(x,c("RS","AS","RA","AR","SR","SA","all_to_stop","stop_to_all","retr_to_stop_ant","ant_to_stop_retr", "all_changes","condition")) #changed it in main so not required
  #print(head(x))
  data_merge_states <- rbind(data_merge_states, x)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ALL STATE CHANGES
data_long_states <- gather(data_merge_states, movement, measurement, RS:all_changes, factor_key=TRUE)

#######Rename conditions and add treatment column
data_long_states$treatment <- "treatment"

data_long_states$treatment[data_long_states$condition == '20210704_PC12_APP_DMSO_0.01'] <- 'control'
data_long_states$treatment[data_long_states$condition == '20210704_PC12_APP_EpoD_5nM'] <- '5nM_EpoD'
data_long_states$treatment[data_long_states$condition == '20210704_PC12_APP_ZMP_5nM'] <- '5nM_ZMP'

#To order the conditions manually
data_long_states$treatment <- factor(data_long_states$treatment, levels=c("control", "5nM_EpoD", "5nM_ZMP"))
my_comparisons <- list(c("control", "5nM_EpoD"), c("control", "5nM_ZMP"))

# #ONLY RELEVANT
data_long_states <- data_long_states[which(data_long_states$movement == "all_to_stop" |
                                           data_long_states$movement == "stop_to_all" |
                                             data_long_states$movement == "retr_to_stop_ant"|
                                             data_long_states$movement == "ant_to_stop_retr"|
                                             data_long_states$movement == "all_changes"),]


p_states_all <- ggplot(data_long_states,aes(x=treatment,y=measurement))+
  facet_wrap(~movement, ncol = 5, strip.position = "bottom") +
  geom_boxplot(width=0.5, aes(fill=treatment), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 1, aes(color=treatment)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous() +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif",
p_states_all
svg("svg_plots_output_final/state_changes_all.svg",  width=8, height=5)
p_states_all
dev.off()



#ONLY RELEVANT
data_long_states <- data_long_states[which(data_long_states$movement == "all_changes"),] 


p_states_all <- ggplot(data_long_states,aes(x=treatment,y=measurement))+
  #facet_wrap(~movement, ncol = 5, strip.position = "bottom") +
  geom_boxplot(width=0.5, aes(fill=treatment), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 1, aes(color=treatment)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(0,2.3), breaks = seq(0,2,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_all
svg("svg_plots_output_final/state_changes_all_only_relevant.svg",  width=4, height=4)
p_states_all
dev.off()

write.table(data_long_states, "svg_plots_output_final/state_changes_wo_treatment_state_changes.csv")




#####TRAJECTORIES STATE CHANGES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("20210704*/*_changes.csv"))
#make data frame to collect data into through the loop
data_merge_states_trajectories <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  #print(head(x))
  data_merge_states_trajectories <- rbind(data_merge_states_trajectories, x)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#data_merge_states$condition <- as.factor(data_merge_states$condition)

# ALL STATE CHANGES
data_long_states_trajectories <- gather(data_merge_states_trajectories, movement, measurement, RS:all_changes, factor_key=TRUE)

#some zeroes were weird (0 and 0.000) and can not be plotted
#data_long_states_trajectories$measurement <- as.numeric(data_long_states_trajectories$measurement)

#######Rename conditions and add treatment column
data_long_states_trajectories$treatment <- "treatment"

data_long_states_trajectories$treatment[data_long_states_trajectories$condition == '20210704_PC12_APP_DMSO_0.01'] <- 'control'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == '20210704_PC12_APP_EpoD_5nM'] <- '5nM_EpoD'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == '20210704_PC12_APP_ZMP_5nM'] <- '5nM_ZMP'

#To order the conditions manually
data_long_states_trajectories$treatment <- factor(data_long_states_trajectories$treatment, levels=c("control", "5nM_EpoD", "5nM_ZMP"))
my_comparisons <- list(c("control", "5nM_EpoD"), c("control", "5nM_ZMP"))

data_long_states_trajectories <- data_long_states_trajectories[which(data_long_states_trajectories$movement == "all_to_stop" | 
                                                                       data_long_states_trajectories$movement == "stop_to_all" | 
                                                                       data_long_states_trajectories$movement == "retr_to_stop_ant"| 
                                                                       data_long_states_trajectories$movement == "ant_to_stop_retr"| 
                                                                       data_long_states_trajectories$movement == "all_changes"),]

p_states_traj_wo_all <- ggplot(data_long_states_trajectories,aes(x=treatment,y=measurement))+
  #facet_wrap(~movement, ncol = 5, strip.position = "bottom") +
  geom_boxplot(width=0.5, aes(fill=treatment), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=treatment)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length \ncheck big sample adjustments") +
  scale_y_continuous() + #limits = c(-0.001,1.3), breaks = seq(0,1.2,by = 0.1)
  stat_compare_means(aes(label = ..p.adjust..), method = "t.test",  p.adjust.method = "fdr", comparisons = my_comparisons) #label = "p.signif", #adjustment for multiple comparison, how to make it for big sample size?
p_states_traj_wo_all
svg("svg_plots_output_final/state_changes_trajectories_all.svg",  width=8, height=6)
p_states_traj_wo_all
dev.off()







#####FRACTIONS

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("20210704*/*_fractions_final.csv"))
#make data frame to collect data into through the loop
data_merge_fractions <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  print(head(x))
  data_merge_fractions <- rbind(data_merge_fractions, x)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#data_merge_fractions$condition <- as.factor(data_merge_fractions$condition) #do not set factor before renaming!


data_merge_fractions$Moving <- data_merge_fractions$Anterograde + data_merge_fractions$Retrograde
data_merge_fractions$Relative <- data_merge_fractions$Moving/(data_merge_fractions$Stationary+data_merge_fractions$Moving)
write.table(data_merge_fractions[,c("condition",   "Relative")], paste("cell_means_", "data_merge_fractions", "_stat_to_all_ratio.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)

data_long_fractions <- gather(data_merge_fractions, fraction, measurement, Relative, factor_key=TRUE)

#######Rename conditions and add treatment column
data_long_fractions$treatment <- "treatment"

data_long_fractions$treatment[data_long_fractions$condition == '20210704_PC12_APP_DMSO_0.01'] <- 'control'
data_long_fractions$treatment[data_long_fractions$condition == '20210704_PC12_APP_EpoD_5nM'] <- '5nM_EpoD'
data_long_fractions$treatment[data_long_fractions$condition == '20210704_PC12_APP_ZMP_5nM'] <- '5nM_ZMP'

#To order the conditions manually
data_long_fractions$treatment <- factor(data_long_fractions$treatment, levels=c("control", "5nM_EpoD", "5nM_ZMP"))
my_comparisons <- list(c("control", "5nM_EpoD"), c("control", "5nM_ZMP"))

# p_mov_wo <- ggdensity(data_long_fractions_wo_treatment, x = "measurement", 
#           fill = "condition", color = "condition",
#           add = "mean", rug = TRUE, alpha = .5) +
#   facet_grid(rows = vars(condition)) +
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp) +
#   #geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
#   theme_classic() +
#   theme(legend.position = "none",
#         plot.title = element_text(color = "black", size = 10),
#         axis.ticks = element_line(colour="black"),
#         axis.text.x = element_text(color = "black", size = 10),
#         axis.text.y = element_text(color = "black", size = 10),  
#         #axis.title.x = element_blank(), 
#         axis.title.y = element_blank()) +
#   ggtitle("Density of ratios of time spent \nmoving to all time with mean") +
#   scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.1)) +
#   scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
#   #stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
# p_mov_wo
# svg("svg_plots_output_final/Density of ratios of time spent moving to all time with mean no treatment.svg",  width=3, height=2)
# p_mov_wo
# dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_wo_median <- ggplot(data_long_fractions, 
       aes(x = measurement, y = treatment,
           color = treatment)) + #fill = condition, 
  stat_density_ridges(quantile_lines = TRUE, aes(fill = treatment), alpha = .2,
                      jittered_points = TRUE, scale = 0.65,
                      point_shape = '|', point_size = 2, point_alpha = 0.5) + #position = "raincloud", 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.1))
p_mov_wo_median
svg("svg_plots_output_final/density_ratios_moving_time_to_all_time_median.svg",  width=3.5, height=2.5)
p_mov_wo_median
dev.off()


# p_mov_wo_median <- ggplot(data_long_fractions, 
#                           aes(x = treatment, y = measurement,
#                               color = treatment)) + #fill = condition, 
#   geom_violin(width=0.5, aes(color = treatment)) + #position = "raincloud", 
#   geom_boxplot(width=0.2) +
#   geom_jitter() +
#   theme_classic() +
#   theme(legend.position = "none",
#         plot.title = element_text(color = "black", size = 10),
#         axis.ticks = element_line(colour="black"),
#         axis.text.x = element_text(color = "black", size = 10),
#         axis.text.y = element_text(color = "black", size = 10),  
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank()) +
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp) +
#   ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
#   stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) +
#   scale_y_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.1))
# p_mov_wo_median

p_mov_overlap <- ggplot(data_long_fractions, aes(x = measurement)) +
    geom_density(aes(color = condition), alpha = 0.4) +
    #geom_histogram(aes(fill = condition), alpha = 0.4, bins=500)+
    theme_classic() +
    theme(#legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2))
p_mov_overlap
svg("svg_plots_output_final/density_ratios_moving_time_to_all_time_overlap.svg",  width=8, height=2.5)
p_mov_overlap
dev.off()



######FRACTIONS TRAJECTORIES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("20210704*/*_assembled_state_fractions.csv"))
filenames
#make data frame to collect data into through the loop
data_merge_fractions_trajectories <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  #print(head(x))
  data_merge_fractions_trajectories <- rbind(data_merge_fractions_trajectories, x)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_merge_fractions_trajectories$Moving <- data_merge_fractions_trajectories$Anterograde + data_merge_fractions_trajectories$Retrograde
data_merge_fractions_trajectories$Relative <- data_merge_fractions_trajectories$Moving/(data_merge_fractions_trajectories$Stationary+data_merge_fractions_trajectories$Moving)
write.table(data_merge_fractions_trajectories[,c("condition",   "Relative")], paste("cell_means_", "data_merge_fractions", "_stat_to_all_ratio.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)

data_long_fractions_trajectories <- gather(data_merge_fractions_trajectories, fraction, measurement, Relative, factor_key=TRUE)

#######Rename conditions and add treatment column
data_long_fractions_trajectories$treatment <- "treatment"

data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == '20210704_PC12_APP_DMSO_0.01'] <- 'control'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == '20210704_PC12_APP_EpoD_5nM'] <- '5nM_EpoD'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == '20210704_PC12_APP_ZMP_5nM'] <- '5nM_ZMP'

#To order the conditions manually
data_long_fractions_trajectories$treatment <- factor(data_long_fractions_trajectories$treatment, levels=c("control", "5nM_EpoD", "5nM_ZMP"))
my_comparisons <- list(c("control", "5nM_EpoD"), c("control", "5nM_ZMP"))

#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_traj_wo_median <- ggplot(data_long_fractions_trajectories, 
                          aes(x = measurement, y = condition,
                              color = condition)) + #fill = condition, 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,jittered_points = TRUE, scale = 0.7) #position = "raincloud", 
p_mov_traj_wo_median
svg("svg_plots_output_final/density_ratios_trajectories_moving_time_to_all_time_median.svg",  width=5, height=2.5)
p_mov_traj_wo_median
dev.off()

p_mov_traj_overlap <- ggplot(data_long_fractions_trajectories, aes(x = measurement)) +
  geom_density(aes(color = condition), alpha = 0.4) +
  #geom_histogram(aes(fill = condition), alpha = 0.4, bins=500)+
  theme_classic() +
  theme(#legend.position = "none",
    plot.title = element_text(color = "black", size = 10),
    axis.ticks = element_line(colour="black"),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),  
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2))
p_mov_traj_overlap
svg("svg_plots_output_final/density_ratios_trajectories_moving_time_to_all_time_overlap.svg",  width=8, height=2.5)
p_mov_traj_overlap
dev.off()



### VELOCITIES

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("20210704*/*_velocities_final.csv"))
#make data frame to collect data into through the loop
data_merge_velocities <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  print(head(x))
  data_merge_velocities <- rbind(data_merge_velocities, x)
}

#velocity to positive value
data_merge_velocities$Mean_retrograde <- -data_merge_velocities$Mean_retrograde 
data_merge_velocities$Min_retrograde <- -data_merge_velocities$Min_retrograde 
#head(data_merge_velocities)

#converting data to between wide and long format
data_long_velocities <- gather(data_merge_velocities, movement, measurement, Mean_retrograde:Max_anterograde, factor_key=TRUE)

#######Rename conditions and add treatment column
data_long_velocities$treatment <- "treatment"

data_long_velocities$treatment[data_long_velocities$condition == '20210704_PC12_APP_DMSO_0.01'] <- 'control'
data_long_velocities$treatment[data_long_velocities$condition == '20210704_PC12_APP_EpoD_5nM'] <- '5nM_EpoD'
data_long_velocities$treatment[data_long_velocities$condition == '20210704_PC12_APP_ZMP_5nM'] <- '5nM_ZMP'

#To order the conditions manually
data_long_velocities$treatment <- factor(data_long_velocities$treatment, levels=c("control", "5nM_EpoD", "5nM_ZMP"))
my_comparisons <- list(c("control", "5nM_EpoD"), c("control", "5nM_ZMP"))


#additional functions for further plotting
meanFunction <- function(x){
  return(data.frame(y=round(mean(x),2),label=round(mean(x,na.rm=T),2)))}
medianFunction <- function(x){
  return(data.frame(y=round(median(x),2),label=round(median(x,na.rm=T),2)))}

#PLOT ALL FACETTED
p_vel <- ggplot(data_long_velocities,aes(x=treatment,y=measurement))+
  facet_wrap(~movement, ncol = 4) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=treatment, fill=treatment), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + 
  geom_jitter(aes(color=treatment), size = 0.7, width = 0.2) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 0) +
  ggtitle("Velocity (d(smoothed_disp)/d(time))") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=1) +
  scale_y_continuous() + #limits = c(0,0.45), breaks = seq(0,0.45,by = 0.05) #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif",  #+
#stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
p_vel
svg("svg_plots_output_final/data_long_velocities_all_facetted.svg",  width=8, height=4)
p_vel
dev.off()

#PLOT MEAN PER CELL
data_long_velocities_mean <- data_long_velocities[which(data_long_velocities$movement == "Mean_retrograde" | data_long_velocities$movement == "Mean_anterograde"),]
p_vel_mean <- ggplot(data_long_velocities_mean,aes(x=treatment,y=measurement))+
  #facet_wrap(~movement, ncol = 4) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=treatment, fill=treatment), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + 
  geom_jitter(aes(color=treatment), size = 0.7, width = 0.2) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 0) +
  ggtitle("Mean velocity (d(smoothed_disp)/d(time)), \nA&R combined") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=1) +
  scale_y_continuous() + #limits = c(0,0.2), breaks = seq(0,0.20,by = 0.05) #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_vel_mean
svg("svg_plots_output_final/data_long_velocities_mean.svg",  width=4, height=3)
p_vel_mean
dev.off()

#PLOT MAX PER CELL
data_long_velocities_max <- data_long_velocities[which(data_long_velocities$movement == "Min_retrograde" | data_long_velocities$movement == "Max_anterograde"),]
p_vel_max <- ggplot(data_long_velocities_max,aes(x=treatment,y=measurement))+
  #facet_wrap(~movement, ncol = 4) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=treatment, fill=treatment), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=treatment), size = 0.7, width = 0.2) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 0) +
  ggtitle("Max velocity (d(smoothed_disp)/d(time)), \nA&R combined") +
  stat_summary(fun = mean, geom = "point",colour = "black", size=1) +
  scale_y_continuous() + #limits = c(0,0.45), breaks = seq(0,0.45,by = 0.05) #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_vel_max
svg("svg_plots_output_final/data_long_velocities_max.svg",  width=4, height=3)
p_vel_max
dev.off()






#### FUNPLOTS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("20210704*/*_df_for_plot.csv"))
filenames
# for (i in filenames){
#   file.remove(i)
# }

#make data frame to collect data into through the loop
data_merge_funplot <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  #print(head(x))
  data_merge_funplot <- rbind(data_merge_funplot, x)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#converting data to between wide and long format
#data_long_funplots <- gather(data_merge_funplots, movement, measurement, smoothed_x_displacement:motion, factor_key=TRUE)
data_long_funplots <- data_merge_funplot
#######Rename conditions and add treatment column
data_long_funplots$treatment <- "treatment"

data_long_funplots$treatment[data_long_funplots$condition == '20210704_PC12_APP_DMSO_0.01'] <- 'control'
data_long_funplots$treatment[data_long_funplots$condition == '20210704_PC12_APP_EpoD_5nM'] <- '5nM_EpoD'
data_long_funplots$treatment[data_long_funplots$condition == '20210704_PC12_APP_ZMP_5nM'] <- '5nM_ZMP'

#To order the conditions manually
data_long_funplots$treatment <- factor(data_long_funplots$treatment, levels=c("control", "5nM_EpoD", "5nM_ZMP"))
my_comparisons <- list(c("control", "5nM_EpoD"), c("control", "5nM_ZMP"))

cols <- c("retrograde" = "#ff9955", "stationary" = "#999999", "anterograde" = "#87decd")
p_percent_counts_all <- ggplot(data_long_funplots, aes(x = factor(treatment)))+
  #facet_grid(~treatment) +
  geom_bar(aes(fill = motion), position="fill") +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Percent of respective motion points")
p_percent_counts_all
svg("svg_plots_output_final/data_percent_counts_all.svg",  width=6, height=4)
p_percent_counts_all
dev.off()

p_fun_wo <- ggplot(data_long_funplots, aes(x = smoothed_x_displacement, y = Time, color = treatment )) + #, text = paste("TrackID:",TrackID)
  #facet_grid(~treatment) + 
  #geom_path(linetype = "dashed", alpha = 0.5) + #has to have color = TrackID to be shown as separate
  geom_point(aes(color=treatment, fill=treatment), alpha = 0.1, shape=18) + #, shape=21 #, fill=motion
  scale_color_manual(values = cbp) +
  #scale_fill_manual(values = cols) +
  dark_theme_gray() +
  #theme_minimal() +
  ggtitle("Trajectories, mobile fraction only, filtered") +
  xlab("Displacement smoothed by window = 5") +
  ylab("Time") +
  theme() +
  scale_y_reverse(breaks = seq(0, 300, by = 50)) +
  #scale_y_continuous() +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) #limits = c(-5, 5), breaks = seq(-5, 5, by = 1)
#p_fun_wo
# svg("svg_plots_output_final/data_long_funplots_wo_treatment_on_one.svg",  width=8, height=6)
# p_fun_wo
# dev.off()

# Basic density plot with mean line and marginal rug
ggdensity(data_long_funplots, x = "delta_x", 
          fill = "motion", color = "treatment",
          add = "mean") +#, rug = TRUE
  facet_grid(~treatment) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Trajectories, mobile fraction only, filtered")

#weighting
# n_wt <- nrow(data_long_funplots_wo_treatment[which(data_long_funplots_wo_treatment$condition == "wt"),])
# n_c3 <- nrow(data_long_funplots_wo_treatment[which(data_long_funplots_wo_treatment$condition == "C3"),])


p_percent_counts <- ggplot(data_long_funplots, aes(x = factor(treatment)))+
  #facet_grid(~condition) +
  geom_bar(aes(fill = motion), position="fill") +
  #scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Percent counts of respective motion points")
p_percent_counts
svg("svg_plots_output_final/data_percent_counts.svg",  width=4, height=4)
p_percent_counts
dev.off()
