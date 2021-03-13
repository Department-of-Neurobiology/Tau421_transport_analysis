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

cbp <- c("#034e61", "#a00000",
         "#034e61", "#a00000",
         "#034e61", "#a00000",
         "#034e61", "#a00000")



#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("svg_plots_output_non_filtered")
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("Tau*/*_changes_final.csv"))
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

data_long_states$treatment[data_long_states$condition == 'Tau441'] <- 'no_treat'
data_long_states$treatment[data_long_states$condition == 'Tau421'] <- 'no_treat'
data_long_states$treatment[data_long_states$condition == 'Tau441_001percentDMSO'] <- 'carrier'
data_long_states$treatment[data_long_states$condition == 'Tau421_001percentDMSO'] <- 'carrier'
data_long_states$treatment[data_long_states$condition == 'Tau441_5nM_EpoD'] <- 'EpoD'
data_long_states$treatment[data_long_states$condition == 'Tau421_5nM_EpoD'] <- 'EpoD'
data_long_states$treatment[data_long_states$condition == 'Tau441_25nM_EpoD'] <- 'EpoD_25nM'
data_long_states$treatment[data_long_states$condition == 'Tau421_25nM_EpoD'] <- 'EpoD_25nM'

data_long_states$condition[data_long_states$condition == 'Tau441'] <- 'wt'
data_long_states$condition[data_long_states$condition == 'Tau421'] <- 'C3'
data_long_states$condition[data_long_states$condition == 'Tau441_001percentDMSO'] <- 'wt'
data_long_states$condition[data_long_states$condition == 'Tau421_001percentDMSO'] <- 'C3'
data_long_states$condition[data_long_states$condition == 'Tau441_5nM_EpoD'] <- 'wt'
data_long_states$condition[data_long_states$condition == 'Tau421_5nM_EpoD'] <- 'C3'
data_long_states$condition[data_long_states$condition == 'Tau441_25nM_EpoD'] <- 'wt'
data_long_states$condition[data_long_states$condition == 'Tau421_25nM_EpoD'] <- 'C3'


#WITHOUT TREATMENT
data_long_states_wo_treatment <- data_long_states[which(data_long_states$treatment == "no_treat"),]
#To order the conditions manually
data_long_states_wo_treatment$condition <- factor(data_long_states_wo_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list( c("wt", "C3"))

#ONLY RELEVANT
data_long_states_wo_treatment <- data_long_states_wo_treatment[which(data_long_states_wo_treatment$movement == "all_to_stop" | 
                                                                       data_long_states_wo_treatment$movement == "stop_to_all" | 
                                                                       data_long_states_wo_treatment$movement == "retr_to_stop_ant"| 
                                                                       data_long_states_wo_treatment$movement == "ant_to_stop_retr"| 
                                                                       data_long_states_wo_treatment$movement == "all_changes"),]

p_states_wo_all <- ggplot(data_long_states_wo_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 5, strip.position = "bottom") +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(0,1.5), breaks = seq(0,1.5,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_wo_all
svg("svg_plots_output_non_filtered/state_changes_wo_treatment_all.svg",  width=7, height=3)
p_states_wo_all
dev.off()

#ALL STATE CHANGES
data_long_states_wo_treatment <- data_long_states_wo_treatment[which(data_long_states_wo_treatment$movement == "all_changes"),]

p_states_wo_state_changes <- ggplot(data_long_states_wo_treatment,aes(x=condition,y=measurement))+
  #facet_wrap(~movement, ncol = 5) +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes \nrelative to trajectory length") +
  scale_y_continuous(limits = c(0,1.5), breaks = seq(0,1.5,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_wo_state_changes
svg("svg_plots_output_non_filtered/state_changes_wo_treatment_state_changes.svg",  width=2, height=3)
p_states_wo_state_changes
dev.off()

#WITH TREATMENT
data_long_states_with_treatment <- data_long_states[which(data_long_states$treatment != "no_treat"),]
#To order the conditions manually
data_long_states_with_treatment$condition <- factor(data_long_states_with_treatment$condition, levels=c("wt", "C3"))
data_long_states_with_treatment$treatment <- factor(data_long_states_with_treatment$treatment, levels=c("carrier", "EpoD", "EpoD_25nM"))
my_comparisons <- list( c("wt", "C3"))
#ONLY RELEVANT
data_long_states_with_treatment <- data_long_states_with_treatment[which(data_long_states_with_treatment$movement == "all_to_stop" | 
                                                                           data_long_states_with_treatment$movement == "stop_to_all" | 
                                                                           data_long_states_with_treatment$movement == "retr_to_stop_ant"| 
                                                                           data_long_states_with_treatment$movement == "ant_to_stop_retr"| 
                                                                           data_long_states_with_treatment$movement == "all_changes"),]

p_states_with_all <- ggplot(data_long_states_with_treatment,aes(x=condition,y=measurement))+
  facet_grid(movement~treatment, switch="x") +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(0,1.8), breaks = seq(0,1.8,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 

p_states_with_all
svg("svg_plots_output_non_filtered/state_changes_with_treatment_all.svg",  width=5, height=9)
p_states_with_all
dev.off()

#ALL STATE CHANGES
data_long_states_with_treatment <- data_long_states_with_treatment[which(data_long_states_with_treatment$movement == "all_changes"),]

p_states_with_state_changes <- ggplot(data_long_states_with_treatment,aes(x=condition,y=measurement))+
  facet_grid(movement~treatment, switch="x") +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes \nrelative to trajectory length") +
  scale_y_continuous(limits = c(0,1.8), breaks = seq(0,1.8,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_with_state_changes
svg("svg_plots_output_non_filtered/state_changes_with_treatment_state_changes.svg",  width=6, height=3)
p_states_with_state_changes
dev.off()

#Two-way ANOVA
res.aov2 <- aov(measurement ~ condition + treatment, data = data_long_states_with_treatment)
summary(res.aov2)
# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(measurement ~ condition * treatment, data = data_long_states_with_treatment)
summary(res.aov3)
plot(res.aov3, 2)
#https://www.datanovia.com/en/blog/how-to-add-p-values-to-ggplot-facets/
  
# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
#DATA IS NOT NORMALLY DISTRIBUTED?! W = 0.97529, p-value = 0.01649
#W = 0.97529, p-value = 0.01649




#####TRAJECTORIES STATE CHANGES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("Tau*/*_changes.csv"))
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

data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau441'] <- 'no_treat'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau421'] <- 'no_treat'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau441_001percentDMSO'] <- 'carrier'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau421_001percentDMSO'] <- 'carrier'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau441_5nM_EpoD'] <- 'EpoD'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau421_5nM_EpoD'] <- 'EpoD'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau441_25nM_EpoD'] <- 'EpoD_25nM'
data_long_states_trajectories$treatment[data_long_states_trajectories$condition == 'Tau421_25nM_EpoD'] <- 'EpoD_25nM'

data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau441'] <- 'wt'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau421'] <- 'C3'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau441_001percentDMSO'] <- 'wt'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau421_001percentDMSO'] <- 'C3'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau441_5nM_EpoD'] <- 'wt'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau421_5nM_EpoD'] <- 'C3'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau441_25nM_EpoD'] <- 'wt'
data_long_states_trajectories$condition[data_long_states_trajectories$condition == 'Tau421_25nM_EpoD'] <- 'C3'

#WITHOUT TREATMENT
data_long_states_trajectories_wo_treatment <- data_long_states_trajectories[which(data_long_states_trajectories$treatment == "no_treat"),]
#To order the conditions manually
data_long_states_trajectories_wo_treatment$condition <- factor(data_long_states_trajectories_wo_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list( c("wt", "C3"))

data_long_states_trajectories_wo_treatment <- data_long_states_trajectories_wo_treatment[which(data_long_states_trajectories_wo_treatment$movement == "all_to_stop" | 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "stop_to_all" | 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "retr_to_stop_ant"| 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "ant_to_stop_retr"| 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "all_changes"),]

p_states_traj_wo_all <- ggplot(data_long_states_trajectories_wo_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 5, strip.position = "bottom") +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length \ncheck big sample adjustments") +
  scale_y_continuous(limits = c(-0.001,1.3), breaks = seq(0,1.2,by = 0.1)) +
  stat_compare_means(aes(label = ..p.adjust..), method = "t.test",  p.adjust.method = "fdr", comparisons = my_comparisons) #label = "p.signif", #adjustment for multiple comparison, how to make it for big sample size?
p_states_traj_wo_all
svg("svg_plots_output_non_filtered/state_changes_trajectories_wo_treatment_all.svg",  width=7, height=3)
p_states_traj_wo_all
dev.off()

#ALL STATE CHANGES
data_long_states_trajectories_wo_treatment <- data_long_states_trajectories_wo_treatment[which(data_long_states_trajectories_wo_treatment$movement == "all_changes"),]

p_states_wo_state_changes <- ggplot(data_long_states_trajectories_wo_treatment,aes(x=condition,y=measurement))+
  #facet_wrap(~movement, ncol = 5) +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes \nrelative to trajectory length") +
  scale_y_continuous(limits = c(-0.001,1.25), breaks = seq(0,1.2,by = 0.1)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_wo_state_changes
svg("svg_plots_output_non_filtered/state_changes_trajectories_wo_treatment_state_changes.svg",  width=2, height=3)
p_states_wo_state_changes
dev.off()


#WITH TREATMENT
data_long_states_trajectories_with_treatment <- data_long_states_trajectories[which(data_long_states_trajectories$treatment != "no_treat"),]
#To order the conditions manually
data_long_states_trajectories_with_treatment$condition <- factor(data_long_states_trajectories_with_treatment$condition, levels=c("wt", "C3"))
data_long_states_trajectories_with_treatment$treatment <- factor(data_long_states_trajectories_with_treatment$treatment, levels=c("carrier", "EpoD", "EpoD_25nM"))
my_comparisons <- list( c("wt", "C3"))

#ONLY RELEVANT
data_long_states_trajectories_with_treatment <- data_long_states_trajectories_with_treatment[which(data_long_states_trajectories_with_treatment$movement == "all_to_stop" | 
                                                                                        data_long_states_trajectories_with_treatment$movement == "stop_to_all" | 
                                                                                        data_long_states_trajectories_with_treatment$movement == "retr_to_stop_ant"| 
                                                                                        data_long_states_trajectories_with_treatment$movement == "ant_to_stop_retr"| 
                                                                                        data_long_states_trajectories_with_treatment$movement == "all_changes"),]

p_states_traj_with_all <- ggplot(data_long_states_trajectories_with_treatment,aes(x=condition,y=measurement))+
  facet_grid(movement~treatment, switch="x") +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(-0.001,1.6), breaks = seq(0,1.5,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 

p_states_traj_with_all
svg("svg_plots_output_non_filtered/state_changes_trajectories_with_treatment_all.svg",  width=5, height=9)
p_states_traj_with_all
dev.off()

#ALL STATE CHANGES
data_long_states_trajectories_with_treatment <- data_long_states_trajectories_with_treatment[which(data_long_states_trajectories_with_treatment$movement == "all_changes"),]

p_states_with_state_changes <- ggplot(data_long_states_trajectories_with_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~treatment, ncol = 3) +
  geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
  geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
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
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes \nrelative to trajectory length") +
  scale_y_continuous(limits = c(-0.001,1.6), breaks = seq(0,1.5,by = 0.2)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_with_state_changes
svg("svg_plots_output_non_filtered/state_changes_trajectories_with_treatment_state_changes.svg",  width=6, height=3)
p_states_with_state_changes
dev.off()

#Two-way ANOVA
res.aov2 <- aov(measurement ~ condition + treatment, data = data_long_states_trajectories_with_treatment)
summary(res.aov2)
# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(measurement ~ condition * treatment, data = data_long_states_trajectories_with_treatment)
summary(res.aov3)
plot(res.aov3, 2)




#####FRACTIONS

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("Tau*/*_fractions_final.csv"))
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

data_long_fractions$treatment[data_long_fractions$condition == 'Tau441'] <- 'no_treat'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau421'] <- 'no_treat'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau441_001percentDMSO'] <- 'carrier'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau421_001percentDMSO'] <- 'carrier'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau441_5nM_EpoD'] <- 'EpoD'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau421_5nM_EpoD'] <- 'EpoD'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau441_25nM_EpoD'] <- 'EpoD_25nM'
data_long_fractions$treatment[data_long_fractions$condition == 'Tau421_25nM_EpoD'] <- 'EpoD_25nM'

data_long_fractions$condition[data_long_fractions$condition == 'Tau441'] <- 'wt'
data_long_fractions$condition[data_long_fractions$condition == 'Tau421'] <- 'C3'
data_long_fractions$condition[data_long_fractions$condition == 'Tau441_001percentDMSO'] <- 'wt'
data_long_fractions$condition[data_long_fractions$condition == 'Tau421_001percentDMSO'] <- 'C3'
data_long_fractions$condition[data_long_fractions$condition == 'Tau441_5nM_EpoD'] <- 'wt'
data_long_fractions$condition[data_long_fractions$condition == 'Tau421_5nM_EpoD'] <- 'C3'
data_long_fractions$condition[data_long_fractions$condition == 'Tau441_25nM_EpoD'] <- 'wt'
data_long_fractions$condition[data_long_fractions$condition == 'Tau421_25nM_EpoD'] <- 'C3'


#WITHOUT TREATMENT
data_long_fractions_wo_treatment <- data_long_fractions[which(data_long_fractions$treatment == "no_treat"),]
#To order the conditions manually
data_long_fractions_wo_treatment$condition <- factor(data_long_fractions_wo_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list(c("wt", "C3"))

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
# svg("svg_plots_output_non_filtered/Density of ratios of time spent moving to all time with mean no treatment.svg",  width=3, height=2)
# p_mov_wo
# dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_wo_median <- ggplot(data_long_fractions_wo_treatment, 
       aes(x = measurement, y = condition,
           color = condition)) + #fill = condition, 
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,
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
svg("svg_plots_output_non_filtered/density_ratios_moving_time_to_all_time_wo_treatment_median.svg",  width=3.5, height=2.5)
p_mov_wo_median
dev.off()

p_mov_overlap <- ggplot(data_long_fractions_wo_treatment, aes(x = measurement)) +
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
svg("svg_plots_output_non_filtered/density_ratios_moving_time_to_all_time_wo_treatment_overlap.svg",  width=4, height=1.5)
p_mov_overlap
dev.off()

#WITH TREATMENT
data_long_fractions_with_treatment <- data_long_fractions[which(data_long_fractions$treatment != "no_treat"),]
#To order the conditions manually
data_long_fractions_with_treatment$condition <- factor(data_long_fractions_with_treatment$condition, levels=c("wt", "C3"))
data_long_fractions_with_treatment$treatment <- factor(data_long_fractions_with_treatment$treatment, levels=c("EpoD_25nM", "EpoD", "carrier"))
my_comparisons <- list(c("wt", "C3"))

# p_mov_with <- ggdensity(data_long_fractions_with_treatment, x = "measurement", 
#                       fill = "condition", color = "condition",
#                       add = "mean", rug = TRUE, alpha = .2) +
#   facet_grid(rows = vars(condition,treatment)) +
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
# #stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
# p_mov_with
# svg("svg_plots_output_non_filtered/Density of ratios of time spent moving to all time with mean with treatment.svg",  width=5, height=7)
# p_mov_with
# dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_with_median <- ggplot(data_long_fractions_with_treatment, 
                          aes(x = measurement, y = condition,
                              color = condition)) + #fill = condition,
  facet_grid(rows = vars(treatment)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,
                      jittered_points = TRUE, scale = 0.65,
                      point_shape = '|', point_size = 2, point_alpha = 0.5) +
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

p_mov_with_median
svg("svg_plots_output_non_filtered/density_ratios_moving_time_to_all_time_with_treatment_median.svg",  width=3.5, height=6)
p_mov_with_median
dev.off()

p_mov_with_overlap <- ggplot(data_long_fractions_with_treatment, aes(x = measurement)) +
  geom_density(aes(color = condition), alpha = 0.4) +
  facet_grid(rows = vars(treatment)) +
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
p_mov_with_overlap
svg("svg_plots_output_non_filtered/density_ratios_moving_time_to_all_time_with_treatment_overlap.svg",  width=4, height=3)
p_mov_with_overlap
dev.off()


######## 1 additional
# cbp <- c("#3122D2", "#E83431",
#          "#574AE2", "#EF6F6C",
#          "#7D73E8", "#F28E8C",
#          "#A29BEF","#F6AFAE")
# # ggplot(data_long_fractions, aes(x = measurement)) + 
# #   geom_density(aes(color = condition), alpha = 0.4) +
# #   #geom_histogram(aes(fill = condition), alpha = 0.4, bins=500)+
# #   theme_minimal() +
# #   scale_color_manual(values = cbp) +
# #   scale_fill_manual(values = cbp) +
# #   ggtitle("Density of ratios of time spent not moving to all time") +
# #   xlab("Delta_x") + 
# #   ylab("Density")
# 
# 
# #converting data to between wide and long format
# data_long_fractions <- gather(data_merge_fractions, fraction, measurement, Anterograde:Stationary, factor_key=TRUE)
# #arrange the levels
# data_long_fractions$fraction <- factor(data_long_fractions$fraction, levels=c("Retrograde", "Stationary", "Anterograde"))
# cbp_fractions <- c("#f8766d", "#619cff", "#00ba38")
# #plot
# stat.test <- data_long_fractions %>%
#   group_by(fraction) %>%
#   t_test(measurement ~ condition) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance()
# stat.test 
# stat.test <- stat.test %>% add_xy_position(x = "condition")
# 
# pdf("fractions.pdf",  width=10, height=8)
# ggplot(data_long_fractions,aes(x=condition,y=measurement,color=condition,fill=fraction))+
#   facet_grid(~fraction) + 
#   geom_boxplot(width=0.5) +
#   geom_point(position = position_jitterdodge(), size = 0.1) +
#   geom_point(position = position_jitterdodge()) + #seed = 1, dodge.width = 0.9
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp_fractions) +
#   theme_minimal() +
#   labs(color = "Constructs and conditions") +
#   ylab("Number of state changes relative to track duration") +
#   scale_y_continuous(breaks = pretty(data_long_fractions$measurement, n = 5)) +
#   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) #+
#   #stat_pvalue_manual(stat.test)
#   #stat_compare_means(comparisons = list(c("Tau441", "Tau421")), label = "p.signif", method = "t.test") #, ref.group = "Tau441_001percentDMSO"
# dev.off()
# 
# 
# #just checked stacked barplots
# ggplot(data_long_fractions, aes(x=condition,y = measurement, fill = fraction, color=condition)) + 
#   #facet_grid(~condition) +
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp_fractions) +
#   geom_bar(position = "fill",stat = "identity")
#   #geom_density()



######FRACTIONS TRAJECTORIES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("Tau*/*_assembled_state_fractions.csv"))
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

data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau441'] <- 'no_treat'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau421'] <- 'no_treat'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau441_001percentDMSO'] <- 'carrier'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau421_001percentDMSO'] <- 'carrier'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau441_5nM_EpoD'] <- 'EpoD'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau421_5nM_EpoD'] <- 'EpoD'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau441_25nM_EpoD'] <- 'EpoD_25nM'
data_long_fractions_trajectories$treatment[data_long_fractions_trajectories$condition == 'Tau421_25nM_EpoD'] <- 'EpoD_25nM'

data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau441'] <- 'wt'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau421'] <- 'C3'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau441_001percentDMSO'] <- 'wt'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau421_001percentDMSO'] <- 'C3'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau441_5nM_EpoD'] <- 'wt'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau421_5nM_EpoD'] <- 'C3'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau441_25nM_EpoD'] <- 'wt'
data_long_fractions_trajectories$condition[data_long_fractions_trajectories$condition == 'Tau421_25nM_EpoD'] <- 'C3'

#WITHOUT TREATMENT
data_long_fractions_trajectories_wo_treatment <- data_long_fractions_trajectories[which(data_long_fractions_trajectories$treatment == "no_treat"),]
#To order the conditions manually
data_long_fractions_trajectories_wo_treatment$condition <- factor(data_long_fractions_trajectories_wo_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list(c("wt", "C3"))
#plot
# p_mov_traj_wo <- ggdensity(data_long_fractions_trajectories_wo_treatment, x = "measurement", 
#                       fill = "condition", color = "condition",
#                       add = "mean", rug = TRUE, alpha = .5) +
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
#   scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
#   scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
# #stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
# p_mov_traj_wo
# svg("svg_plots_output_non_filtered/density_ratios_trajectories_moving_time_to_all_time_wo_treatment_mean.svg",  width=3.5, height=2.5)
# p_mov_traj_wo
# dev.off()

#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_traj_wo_median <- ggplot(data_long_fractions_trajectories_wo_treatment, 
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
svg("svg_plots_output_non_filtered/density_ratios_trajectories_moving_time_to_all_time_wo_treatment_median.svg",  width=3.5, height=2.5)
p_mov_traj_wo_median
dev.off()

p_mov_traj_overlap <- ggplot(data_long_fractions_trajectories_wo_treatment, aes(x = measurement)) +
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
svg("svg_plots_output_non_filtered/density_ratios_trajectories_moving_time_to_all_time_wo_treatment_overlap.svg",  width=4, height=1.5)
p_mov_traj_overlap
dev.off()


#WITH TREATMENT
data_long_fractions_trajectories_with_treatment <- data_long_fractions_trajectories[which(data_long_fractions_trajectories$treatment != "no_treat"),]
#To order the conditions manually
data_long_fractions_trajectories_with_treatment$condition <- factor(data_long_fractions_trajectories_with_treatment$condition, levels=c("wt", "C3"))
data_long_fractions_trajectories_with_treatment$treatment <- factor(data_long_fractions_trajectories_with_treatment$treatment, levels=c("EpoD_25nM", "EpoD", "carrier"))
my_comparisons <- list(c("wt", "C3"))

# p_mov_traj_with <- ggdensity(data_long_fractions_trajectories_with_treatment, x = "measurement", 
#                         fill = "condition", color = "condition",
#                         add = "mean", rug = TRUE, alpha = .5) +
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
#   scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
#   scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
# #stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
# p_mov_traj_with
# svg("svg_plots_output_non_filtered/TRAJECTORIES Density of ratios of time spent moving to all time with mean with treatment.svg",  width=5, height=4)
# p_mov_traj_with
# dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_traj_with_median <- ggplot(data_long_fractions_trajectories_with_treatment, 
                            aes(x = measurement, y = condition,
                                color = condition)) + #fill = condition,
  facet_grid(rows = vars(treatment)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,
                      jittered_points = TRUE, scale = 0.65,
                      point_shape = '|', point_size = 2, point_alpha = 0.5) +
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

p_mov_traj_with_median
svg("svg_plots_output_non_filtered/density_ratios_trajectories_moving_time_to_all_time_with_treatment_median.svg",  width=3.5, height=6)
p_mov_traj_with_median
dev.off()

p_mov_traj_with_overlap <- ggplot(data_long_fractions_trajectories_with_treatment, aes(x = measurement)) +
  geom_density(aes(color = condition), alpha = 0.4) +
  facet_grid(rows = vars(treatment)) +
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
p_mov_traj_with_overlap
svg("svg_plots_output_non_filtered/density_ratios_trajectories_moving_time_to_all_time_with_treatment_overlap.svg",  width=4, height=3)
p_mov_traj_with_overlap
dev.off()





# 
# 
# #another way to have multiple plots
# #save legend separately
# my_legend <- get_legend(fraction_single_plot) #cowplot
# as_ggplot(my_legend) #ggpubr
# 
# #to have together aes_string and aes, because names(data_merge)[i] is a string (e.g. "AR" and not AR)
# `+.uneval` <- function(a,b) {
#   `class<-`(modifyList(a,b), "uneval")
# }
# plt <- list()
# for(i in 1:3){
#   plt[[i]] <- ggplot(data_merge_fractions,aes_string(y=names(data_merge_fractions)[i]) + aes(x=condition,color=condition))+
#     #geom_violin() + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
#     #geom_boxplot(width=0.1, fill="white") +
#     geom_boxplot(width=0.5) +
#     geom_point(position = position_jitterdodge()) +
#     #geom_point(position = position_jitterdodge(seed = 1, dodge.width = 0.9)) +
#     scale_color_manual(values = cbp) +
#     #scale_fill_brewer(palette="RdBu") + 
#     theme_minimal() +
#     ylim(0, 100) +
#     labs(color = "Constructs and conditions") +
#     theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position="none") +
#     stat_compare_means(label = "p.signif", method = "t.test",
#                        ref.group = "Tau441_001percentDMSO")
#   plt[[i]] 
#   #print(plt)
#   #Sys.sleep(2)
# }
# 
# 
# #to save the last plot as plotly object
# #p <- ggplotly(
# #  p = ggplot2::last_plot()
# #)
# #save as an interactive html widget
# #htmlwidgets::saveWidget(as_widget(p), "test.html")
# 
# #to have together aes_string and aes, because names(data_merge)[i] is a string (e.g. "AR" and not AR)
# `+.uneval` <- function(a,b) {
#   `class<-`(modifyList(a,b), "uneval")
# }
# plt <- list()
# for(i in 1:6){
#   plt[[i]] <- ggplot(data_merge_states,aes_string(y=names(data_merge_states)[i]) + aes(x=condition,color=condition))+
#     #geom_violin() + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
#     #geom_boxplot(width=0.1, fill="white") +
#     geom_boxplot(width=0.5) +
#     geom_point(position = position_jitterdodge()) +
#     #geom_point(position = position_jitterdodge(seed = 1, dodge.width = 0.9)) +
#     scale_color_manual(values = cbp) +
#     #scale_fill_brewer(palette="RdBu") + 
#     theme_minimal() +
#     ylim(0, 0.06) +
#     labs(color = "Constructs and conditions") +
#     theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position="none") +
#     stat_compare_means(label = "p.signif", method = "t.test",
#                        ref.group = "Tau441_001percentDMSO")
#   #print(plt)
#   #Sys.sleep(2)
# }
# 
# 
# 
# 
# 
# 
# 
# ### VELOCITIES
# 
# #Set working directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# #read all files with selected name ending and extension
# filenames <- Sys.glob(file.path("Tau*/*_velocities_final.csv"))
# #make data frame to collect data into through the loop
# data_merge_velocities <- data.frame()
# #loop through all files
# for (i in filenames){  
#   x <- read.table(i, sep = ";",header = TRUE)
#   print(head(x))
#   data_merge_velocities <- rbind(data_merge_velocities, x)
# }
# 
# #To order the conditions manually
# data_merge_velocities$condition <- factor(data_merge_velocities$condition, levels=c("Tau441", "Tau421", "Tau441_001percentDMSO", "Tau421_001percentDMSO", 
#                                                                                     "Tau441_5nM_EpoD", "Tau421_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421_25nM_EpoD"))
# #levels=c("Tau441", "Tau441_001percentDMSO", "Tau441_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421", "Tau421_001percentDMSO", "Tau421_5nM_EpoD", "Tau421_25nM_EpoD")
# 
# #plot histograms for collected filtered results
# cbp <- c("#3122D2", "#E83431",
#          "#574AE2", "#EF6F6C",
#          "#7D73E8", "#F28E8C",
#          "#A29BEF","#F6AFAE")
# #cbp <- c("#3122D2", "#574AE2", "#7D73E8", "#A29BEF", "#E83431", "#EF6F6C", "#F28E8C", "#F6AFAE")
# 
# #velocity to positive value
# data_merge_velocities$Retrograde <- -data_merge_velocities$Retrograde
# #head(data_merge_velocities)
# 
# #converting data to between wide and long format
# data_long_velocities <- gather(data_merge_velocities, movement, measurement, Anterograde:Retrograde, factor_key=TRUE)
# 
# my_comparisons <- list( c("Tau441", "Tau421"), 
#                         c("Tau441_001percentDMSO", "Tau421_001percentDMSO"), 
#                         c("Tau441_001percentDMSO", "Tau441_5nM_EpoD"),
#                         c("Tau441_5nM_EpoD", "Tau421_5nM_EpoD"),
#                         c("Tau421_001percentDMSO", "Tau421_5nM_EpoD"),
#                         c("Tau441_001percentDMSO", "Tau441_25nM_EpoD"), 
#                         c("Tau421_25nM_EpoD", "Tau421_001percentDMSO"))
# 
# meanFunction <- function(x){
#   return(data.frame(y=round(mean(x),2),label=round(mean(x,na.rm=T),2)))}
# medianFunction <- function(x){
#   return(data.frame(y=round(median(x),2),label=round(median(x,na.rm=T),2)))}
# 
# 
# ggplot(data_long_velocities,aes(x=condition,y=measurement))+
#   facet_wrap(~movement, ncol = 2) +
#   geom_violin(width=1.5) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
#   geom_boxplot(width=0.1, aes(color=condition)) + #fill="white", alpha=0,
#   geom_point(size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
#   scale_color_manual(values = cbp) +
#   #scale_fill_brewer(palette="RdBu") + 
#   #theme_classic2() +
#   labs(fill = "Constructs and conditions") +
#   ylab("Velocity (d(smoothed_disp)/d(time))") +
#   scale_y_continuous(breaks = pretty(data_long_velocities$measurement, n = 5)) +
#   theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
#   #stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
#   stat_summary(fun.data = medianFunction, geom ="text", color = "black", size = 3, vjust = 1.3) +
#   stat_summary(fun.y = mean, geom = "point",colour = "darkred", size=1) +
#   #stat_pvalue_manual(stat.test)
#   stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441_001percentDMSO")
# 
# 
# ggplot(data_long_velocities,aes(x=condition,y=measurement))+
#   geom_violin(width=1.5) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
#   geom_boxplot(width=0.1, aes(color=condition)) + #fill="white", alpha=0,
#   geom_point(size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
#   scale_color_manual(values = cbp) +
#   #scale_fill_brewer(palette="RdBu") + 
#   #theme_classic2() +
#   labs(fill = "Constructs and conditions") +
#   ylab("Velocity (d(smoothed_disp)/d(time)), A&R combined") +
#   scale_y_continuous(breaks = pretty(data_long_velocities$measurement, n = 5)) +
#   theme(axis.title.x=element_blank(),axis.text.x=element_blank()) + #legend.position = "none"
#   #stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
#   stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 1.3) +
#   stat_summary(fun.y = mean, geom = "point",colour = "darkred", size=1) +
#   #stat_pvalue_manual(stat.test)
#   stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441_001percentDMSO")
# 
# data_long_velocities_wo_treatment <- data_long_velocities[which(data_long_velocities$condition == "Tau441" | data_long_velocities$condition == "Tau421"),]
# cbp <- c("#3122D2", "#E83431")
# pdf("Velocity no treatment.pdf",  width=5, height=5)
# ggplot(data_long_velocities_wo_treatment,aes(x=condition,y=measurement))+
#   facet_wrap(~movement, ncol = 2) +
#   geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
#   geom_boxplot(width=0.05) +
#   geom_point(aes(color=condition)) + 
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp) +
#   theme_bw() +
#   ylab("Velocity (d(smoothed_disp)/d(time)), A&R combined") +
#   theme(axis.title.x=element_blank(),axis.text.x=element_blank()) + #legend.position = "none"
#   #stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
#   #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 1.3) +
#   stat_summary(fun = mean, geom = "point",colour = "black", size=3) +
#   scale_y_continuous(breaks = pretty(data_long_velocities_wo_treatment$measurement, n = 10)) +
#   stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
# dev.off()
# 
# data_long_velocities_with_treatment <- data_long_velocities[which(data_long_velocities$condition == "Tau441_001percentDMSO" | data_long_velocities$condition == "Tau421_001percentDMSO" | data_long_velocities$condition == "Tau441_5nM_EpoD" | data_long_velocities$condition == "Tau421_5nM_EpoD" | data_long_velocities$condition == "Tau441_25nM_EpoD" | data_long_velocities$condition == "Tau421_25nM_EpoD"),]
# cbp <- c("#574AE2", "#EF6F6C",
#          "#7D73E8", "#F28E8C",
#          "#A29BEF","#F6AFAE")
# pdf("Velocity with treatment.pdf",  width=5, height=5)
# ggplot(data_long_velocities_with_treatment,aes(x=condition,y=measurement))+
#   facet_wrap(~movement, ncol = 2) +
#   geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
#   geom_boxplot(width=0.1) +
#   geom_point(aes(color=condition)) + 
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp) +
#   theme_bw() +
#   ylab("Velocity (d(smoothed_disp)/d(time)), A&R combined") +
#   theme(axis.title.x=element_blank(),axis.text.x=element_blank()) + #legend.position = "none"
#   #stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
#   #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 1.3) +
#   stat_summary(fun = mean, geom = "point",colour = "black", size=3) +
#   scale_y_continuous(breaks = pretty(data_long_velocities_with_treatment$measurement, n = 10)) +
#   stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441_001percentDMSO")
# dev.off()



### VELOCITIES

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("Tau*/*_velocities_final.csv"))
#make data frame to collect data into through the loop
data_merge_velocities <- data.frame()
#loop through all files
for (i in filenames){  
  x <- read.table(i, sep = ";",header = TRUE)
  print(head(x))
  data_merge_velocities <- rbind(data_merge_velocities, x)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#velocity to positive value
data_merge_velocities$Mean_retrograde <- -data_merge_velocities$Mean_retrograde 
data_merge_velocities$Min_retrograde <- -data_merge_velocities$Min_retrograde 
#head(data_merge_velocities)

#converting data to between wide and long format
data_long_velocities <- gather(data_merge_velocities, movement, measurement, Mean_retrograde:Max_anterograde, factor_key=TRUE)

#######Rename conditions and add treatment column
data_long_velocities$treatment <- "treatment"

data_long_velocities$treatment[data_long_velocities$condition == 'Tau441'] <- 'no_treat'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau421'] <- 'no_treat'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau441_001percentDMSO'] <- 'carrier'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau421_001percentDMSO'] <- 'carrier'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau441_5nM_EpoD'] <- 'EpoD'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau421_5nM_EpoD'] <- 'EpoD'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau441_25nM_EpoD'] <- 'EpoD_25nM'
data_long_velocities$treatment[data_long_velocities$condition == 'Tau421_25nM_EpoD'] <- 'EpoD_25nM'

data_long_velocities$condition[data_long_velocities$condition == 'Tau441'] <- 'wt'
data_long_velocities$condition[data_long_velocities$condition == 'Tau421'] <- 'C3'
data_long_velocities$condition[data_long_velocities$condition == 'Tau441_001percentDMSO'] <- 'wt'
data_long_velocities$condition[data_long_velocities$condition == 'Tau421_001percentDMSO'] <- 'C3'
data_long_velocities$condition[data_long_velocities$condition == 'Tau441_5nM_EpoD'] <- 'wt'
data_long_velocities$condition[data_long_velocities$condition == 'Tau421_5nM_EpoD'] <- 'C3'
data_long_velocities$condition[data_long_velocities$condition == 'Tau441_25nM_EpoD'] <- 'wt'
data_long_velocities$condition[data_long_velocities$condition == 'Tau421_25nM_EpoD'] <- 'C3'

#WITHOUT TREATMENT
data_long_velocities_wo_treatment <- data_long_velocities[which(data_long_velocities$treatment == "no_treat"),]
#To order the conditions manually
data_long_velocities_wo_treatment$condition <- factor(data_long_velocities_wo_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list(c("wt", "C3"))

#additional functions for further plotting
meanFunction <- function(x){
  return(data.frame(y=round(mean(x),2),label=round(mean(x,na.rm=T),2)))}
medianFunction <- function(x){
  return(data.frame(y=round(median(x),2),label=round(median(x,na.rm=T),2)))}

#PLOT ALL FACETTED
p_vel <- ggplot(data_long_velocities_wo_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 4) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + 
  geom_jitter(aes(color=condition), size = 0.7, width = 0.2) +
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
  scale_y_continuous(limits = c(0,0.45), breaks = seq(0,0.45,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif",  #+
#stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
p_vel
svg("svg_plots_output_non_filtered/data_long_velocities_wo_treatment_all_facetted.svg",  width=7, height=3)
p_vel
dev.off()

#PLOT MEAN PER CELL
data_long_velocities_wo_treatment_mean <- data_long_velocities_wo_treatment[which(data_long_velocities_wo_treatment$movement == "Mean_retrograde" | data_long_velocities_wo_treatment$movement == "Mean_anterograde"),]
p_vel_mean <- ggplot(data_long_velocities_wo_treatment_mean,aes(x=condition,y=measurement))+
  #facet_wrap(~movement, ncol = 4) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + 
  geom_jitter(aes(color=condition), size = 0.7, width = 0.2) +
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
  scale_y_continuous(limits = c(0,0.25), breaks = seq(0,0.20,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_vel_mean
svg("svg_plots_output_non_filtered/data_long_velocities_wo_treatment_mean.svg",  width=2, height=3)
p_vel_mean
dev.off()

#PLOT MAX PER CELL
data_long_velocities_wo_treatment_max <- data_long_velocities_wo_treatment[which(data_long_velocities_wo_treatment$movement == "Min_retrograde" | data_long_velocities_wo_treatment$movement == "Max_anterograde"),]
p_vel_max <- ggplot(data_long_velocities_wo_treatment_max,aes(x=condition,y=measurement))+
  #facet_wrap(~movement, ncol = 4) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=condition), size = 0.7, width = 0.2) +
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
  scale_y_continuous(limits = c(0,0.45), breaks = seq(0,0.45,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_vel_max
svg("svg_plots_output_non_filtered/data_long_velocities_wo_treatment_max.svg",  width=2, height=3)
p_vel_max
dev.off()


# ggarrange(p_vel_mean, p_vel_max, p_vel, ncol = 2, nrow = 2, 
#           widths = c(4,2), 
#           heights = c(4,2),
#           common.legend = TRUE,
#           labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))


arrange_wo <- ggarrange(p_vel, # First row
                        ggarrange(p_vel_mean, p_vel_max, ncol = 2, widths = c(0.5,0.5), labels = c("B", "C")), # Second row with box and dot plots
                        nrow = 2, 
                        common.legend = TRUE,
                        #heights = c(4,2),
                        labels = "A" # Labels of the first plot
) 
arrange_wo
svg("svg_plots_output_non_filtered/data_long_velocities_wo_treatment_arrange_all.svg",  width=7, height=6)
arrange_wo
dev.off()


##### WITH TREATMENT #####
#WITHOUT TREATMENT
data_long_velocities_with_treatment <- data_long_velocities[which(data_long_velocities$treatment != "no_treat"),]
#To order the conditions manually
data_long_velocities_with_treatment$condition <- factor(data_long_velocities_with_treatment$condition, levels=c("wt", "C3"))
data_long_velocities_with_treatment$treatment <- factor(data_long_velocities_with_treatment$treatment, levels=c("EpoD_25nM", "EpoD", "carrier"))
my_comparisons <- list(c("wt", "C3"))


#why did na and inf arise? had to get rid of them first
data_long_velocities_with_treatment <- data_long_velocities_with_treatment[!is.infinite(data_long_velocities_with_treatment$measurement),]
data_long_velocities_with_treatment <- data_long_velocities_with_treatment[!is.na(data_long_velocities_with_treatment$measurement),]
#colMax <- function(data) sapply(data, max, na.rm = TRUE)
#colMax(data_long_velocities_with_treatment$measurement) #does not work


#PLOT ALL FACETTED

p_vel_treat <- ggplot(data_long_velocities_with_treatment,aes(x=condition,y=measurement))+
  facet_grid(treatment~movement) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + 
  geom_jitter(aes(color=condition), size = 0.7, width = 0.2) +
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
  scale_y_continuous(limits = c(0,0.42), breaks = seq(0,0.4,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif",  #+
#stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
p_vel_treat
svg("svg_plots_output_non_filtered/data_long_velocities_with_treatment_all_facetted.svg",  width=6, height=6)
p_vel_treat
dev.off()


#PLOT MEAN PER CELL
data_long_velocities_with_treatment_mean <- data_long_velocities_with_treatment[which(data_long_velocities_with_treatment$movement == "Mean_retrograde" | data_long_velocities_with_treatment$movement == "Mean_anterograde"),]
data_long_velocities_with_treatment_mean$treatment <- factor(data_long_velocities_with_treatment_mean$treatment, levels=c("carrier", "EpoD", "EpoD_25nM"))
p_vel_treat_mean <- ggplot(data_long_velocities_with_treatment_mean,aes(x=condition,y=measurement))+
  facet_grid(~treatment) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + 
  geom_jitter(aes(color=condition), size = 0.7, width = 0.2) +
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
  scale_y_continuous(limits = c(0,0.42), breaks = seq(0,0.4,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_vel_treat_mean
svg("svg_plots_output_non_filtered/data_long_velocities_with_treatment_mean.svg",  width=5, height=3)
p_vel_treat_mean
dev.off()

#PLOT MAX PER CELL
data_long_velocities_with_treatment_max <- data_long_velocities_with_treatment[which(data_long_velocities_with_treatment$movement == "Min_retrograde" | data_long_velocities_with_treatment$movement == "Max_anterograde"),]
data_long_velocities_with_treatment_max$treatment <- factor(data_long_velocities_with_treatment_max$treatment, levels=c("carrier", "EpoD", "EpoD_25nM"))
p_vel_treat_max <- ggplot(data_long_velocities_with_treatment_max,aes(x=condition,y=measurement))+
  facet_grid(~treatment) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=condition), size = 0.7, width = 0.2) +
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
  scale_y_continuous(limits = c(0,0.42), breaks = seq(0,0.4,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_vel_treat_max
svg("svg_plots_output_non_filtered/data_long_velocities_with_treatment_max.svg",  width=5, height=3)
p_vel_treat_max
dev.off()


# ggarrange(p_vel_treat_mean, p_vel_treat_max, p_vel_treat, ncol = 2, nrow = 2, 
#           widths = c(4,2), 
#           heights = c(4,2),
#           common.legend = TRUE,
#           labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))


arrange_with <- ggarrange(p_vel_treat, # First row
                          ggarrange(p_vel_treat_mean, p_vel_treat_max, ncol = 2, widths = c(0.5,0.5), labels = c("B", "C")), # Second row with box and dot plots
                          nrow = 2, 
                          common.legend = TRUE,
                          heights = c(4,2),
                          labels = "A" # Labels of the first plot
) 
arrange_with
svg("svg_plots_output_non_filtered/data_long_velocities_with_treatment_arrange_all.svg",  width=6, height=6)
arrange_with
dev.off()


arrange_all <- ggarrange(p_vel, # First row
                         ggarrange(p_vel_mean, p_vel_max, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                         ggarrange(p_vel_treat, # First row
                                   ggarrange(p_vel_treat_mean, p_vel_treat_max, ncol = 2, labels = c("E", "F")), # Second row with box and dot plots
                                   nrow = 2, 
                                   #common.legend = TRUE,
                                   heights = c(4,2),
                                   labels = "D" # Labels of the first plot
                         ),
                         nrow = 3, 
                         heights = c(1,1,4),
                         common.legend = TRUE,
                         labels = "A" # Labels of the first plot
) 
arrange_all
svg("svg_plots_output_non_filtered/data_long_velocities_arrange_all.svg",  width=10, height=12)
arrange_all
dev.off()







library(ggdark)
#Set colors
cols <- c("retrograde" = "#118ab2", "stationary" = "#ef476f", "anterograde" = "#06d6a0")
col_gradient <- colorRampPalette(c("#390099", "#9E0059", "#FF0054", "#FF5400", "#FFBD00")) #choose multiple colors for gradient

#### FUNPLOTS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#read all files with selected name ending and extension
filenames <- Sys.glob(file.path("Tau*/*_df_for_plot.csv"))
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

data_long_funplots$treatment[data_long_funplots$condition == 'Tau441'] <- 'no_treat'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau421'] <- 'no_treat'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau441_001percentDMSO'] <- 'carrier'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau421_001percentDMSO'] <- 'carrier'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau441_5nM_EpoD'] <- 'EpoD'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau421_5nM_EpoD'] <- 'EpoD'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau441_25nM_EpoD'] <- 'EpoD_25nM'
data_long_funplots$treatment[data_long_funplots$condition == 'Tau421_25nM_EpoD'] <- 'EpoD_25nM'

data_long_funplots$condition[data_long_funplots$condition == 'Tau441'] <- 'wt'
data_long_funplots$condition[data_long_funplots$condition == 'Tau421'] <- 'C3'
data_long_funplots$condition[data_long_funplots$condition == 'Tau441_001percentDMSO'] <- 'wt'
data_long_funplots$condition[data_long_funplots$condition == 'Tau421_001percentDMSO'] <- 'C3'
data_long_funplots$condition[data_long_funplots$condition == 'Tau441_5nM_EpoD'] <- 'wt'
data_long_funplots$condition[data_long_funplots$condition == 'Tau421_5nM_EpoD'] <- 'C3'
data_long_funplots$condition[data_long_funplots$condition == 'Tau441_25nM_EpoD'] <- 'wt'
data_long_funplots$condition[data_long_funplots$condition == 'Tau421_25nM_EpoD'] <- 'C3'


#ALL TREATMENT

data_long_funplots$condition <- factor(data_long_funplots$condition, levels=c("wt", "C3"))
data_long_funplots$treatment <- factor(data_long_funplots$treatment, levels=c("no_treat", "carrier", "EpoD", "EpoD_25nM"))
p_percent_counts_all <- ggplot(data_long_funplots, aes(x = factor(condition)))+
  facet_grid(~treatment) +
  geom_bar(aes(fill = motion, color = condition), position="fill") +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Percent of respective motion points")
p_percent_counts_all
svg("svg_plots_output_non_filtered/data_percent_counts_all.svg",  width=6, height=4)
p_percent_counts_all
dev.off()

#WITHOUT TREATMENT
data_long_funplots_wo_treatment <- data_long_funplots[which(data_long_funplots$treatment == "no_treat"),]
#To order the conditions manually
data_long_funplots_wo_treatment$condition <- factor(data_long_funplots_wo_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list(c("wt", "C3"))

p_fun_wo <- ggplot(data_long_funplots_wo_treatment, aes(x = smoothed_x_displacement, y = Time, color = condition )) + #, text = paste("TrackID:",TrackID)
  #facet_grid(~condition) + 
  #geom_path(linetype = "dashed", alpha = 0.5) + #has to have color = TrackID to be shown as separate
  geom_point(aes(color=condition, fill=condition), alpha = 0.1, shape=18) + #, shape=21 #, fill=motion
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
# svg("svg_plots_output_non_filtered/data_long_funplots_wo_treatment_on_one.svg",  width=8, height=6)
# p_fun_wo
# dev.off()

cols <- c("retrograde" = "#118ab2", "stationary" = "#ef476f", "anterograde" = "#06d6a0")
p_fun_wo <- ggplot(data_long_funplots_wo_treatment, aes(x = smoothed_x_displacement, y = Time, color = motion)) + #, text = paste("TrackID:",TrackID)
  facet_grid(~condition) + 
  #geom_path(linetype = "dashed", alpha = 0.5) + #has to have color = TrackID to be shown as separate
  geom_point(alpha = 1, shape=19) + #, shape=21
  scale_color_manual(values = cols) +
  dark_theme_gray() +
  theme_classic() +
  ggtitle("Trajectories, mobile fraction only, filtered") +
  xlab("Displacement smoothed by window = 5") +
  ylab("Time") +
  theme() +
  scale_y_reverse(breaks = seq(0, 300, by = 50)) +
  #scale_y_continuous() +
  scale_x_continuous(breaks = seq(-20, 20, by = 2)) #limits = c(-5, 5), breaks = seq(-5, 5, by = 1)
#p_fun_wo
# svg("svg_plots_output_non_filtered/data_long_funplots_wo_treatment.svg",  width=20, height=10)
# p_fun_wo
# dev.off()

# Density plot
# ggplot(data_long_funplots_wo_treatment, aes(x = delta_x)) + 
#   geom_density(aes(fill = motion, color = condition), alpha = 0.4) +
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cols) +
#   #geom_histogram(aes(fill = motion, color = condition), alpha = 0.4,bins=50)+
#   #dark_theme_gray() +
#   #theme_minimal() +
#   ggtitle("Trajectories, mobile fraction only, filtered") +
#   xlab("Delta_x") + 
#   ylab("Density") 

# Basic density plot with mean line and marginal rug
ggdensity(data_long_funplots_wo_treatment, x = "delta_x", 
          fill = "motion", color = "condition",
          add = "mean") +#, rug = TRUE
  facet_grid(~treatment) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Trajectories, mobile fraction only, filtered")




#weighting
# n_wt <- nrow(data_long_funplots_wo_treatment[which(data_long_funplots_wo_treatment$condition == "wt"),])
# n_c3 <- nrow(data_long_funplots_wo_treatment[which(data_long_funplots_wo_treatment$condition == "C3"),])


p_percent_counts <- ggplot(data_long_funplots_wo_treatment, aes(x = factor(condition)))+
  #facet_grid(~condition) +
  geom_bar(aes(fill = motion, color = condition), position="fill") +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Percent of respective motion points")
p_percent_counts
svg("svg_plots_output_non_filtered/data_percent_counts_wo_treatment.svg",  width=4, height=4)
p_percent_counts
dev.off()

#WITH TREATMENT
data_long_funplots_with_treatment <- data_long_funplots[which(data_long_funplots$treatment != "no_treat"),]
#To order the conditions manually
data_long_funplots_with_treatment$condition <- factor(data_long_funplots_with_treatment$condition, levels=c("wt", "C3"))
my_comparisons <- list(c("wt", "C3"))

p_fun_with <- ggplot(data_long_funplots_with_treatment, aes(x = smoothed_x_displacement, y = Time, color = condition )) + #, text = paste("TrackID:",TrackID)
  facet_grid(~treatment) + 
  #geom_path(linetype = "dashed", alpha = 0.5) + #has to have color = TrackID to be shown as separate
  geom_point(aes(color=condition), alpha = 0.1, shape=19) + #, shape=21
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  dark_theme_gray() +
  #theme_minimal() +
  ggtitle("Trajectories, mobile fraction only, filtered") +
  xlab("Displacement smoothed by window = 5") +
  ylab("Time") +
  theme() +
  scale_y_reverse(breaks = seq(0, 300, by = 50)) +
  #scale_y_continuous() +
  scale_x_continuous(breaks = seq(-20, 20, by = 1)) #limits = c(-5, 5), breaks = seq(-5, 5, by = 1)
#p_fun_with
# svg("svg_plots_output_non_filtered/data_long_funplots_with_treatment.svg",  width=12, height=5)
# p_fun_with
# dev.off()


# Basic density plot with mean line and marginal rug
ggdensity(data_long_funplots_with_treatment, x = "delta_x", 
          fill = "motion", color = "condition",
          add = "mean") +#, rug = TRUE
  facet_grid(~treatment) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  ggtitle("Trajectories, mobile fraction only, filtered")
