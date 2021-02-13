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
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7,by = 0.05)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_wo_all
svg("svg_plots_output/state_changes_wo_treatment_all.svg",  width=7, height=3)
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
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7,by = 0.1)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_wo_state_changes
svg("svg_plots_output/state_changes_wo_treatment_state_changes.svg",  width=2, height=3)
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
  scale_y_continuous(limits = c(0,0.75), breaks = seq(0,07,by = 0.1)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 

p_states_with_all
svg("svg_plots_output/state_changes_with_treatment_all.svg",  width=5, height=9)
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
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7,by = 0.1)) +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, size = 3) #label = "p.signif", 
p_states_with_state_changes
svg("svg_plots_output/state_changes_with_treatment_state_changes.svg",  width=6, height=3)
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
svg("svg_plots_output/state_changes_trajectories_wo_treatment_all.svg",  width=7, height=3)
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
svg("svg_plots_output/state_changes_trajectories_wo_treatment_state_changes.svg",  width=2, height=3)
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
svg("svg_plots_output/state_changes_trajectories_with_treatment_all.svg",  width=5, height=9)
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
svg("svg_plots_output/state_changes_trajectories_with_treatment_state_changes.svg",  width=6, height=3)
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


