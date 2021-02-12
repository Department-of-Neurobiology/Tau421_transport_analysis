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
  print(head(x))
  data_merge_states <- rbind(data_merge_states, x)
}

# ALL STATE CHANGES
data_long_states <- gather(data_merge_states, movement, measurement, RS:all_changes, factor_key=TRUE)
#WITHOUT TREATMENT
data_long_states_wo_treatment <- data_long_states[which(data_long_states$condition == "Tau441" | data_long_states$condition == "Tau421"),]
#To order the conditions manually
data_long_states_wo_treatment$condition <- factor(data_long_states_wo_treatment$condition, levels=c("Tau441", "Tau421"))
my_comparisons <- list( c("Tau441", "Tau421"))
#cbp <- c("#3122D2", "#E83431")
cbp <- c("#034e61", "#a00000")
# 
# 
# ####THAT IS WRONG ACTUALLY, they should be summed not plotted together
# data_long_states_wo_all_changes_summed <- data_long_states_wo_treatment[which(data_long_states_wo_treatment$movement  == "RS" | data_long_states_wo_treatment$movement == "SR"  | data_long_states_wo_treatment$movement == "AR"  | data_long_states_wo_treatment$movement == "RA"  | data_long_states_wo_treatment$movement == "SA"  | data_long_states_wo_treatment$movement == "AS"),]
# data_long_states_wo_all_changes_summed$movement
# data_long_states_wo_all_changes_summed <- data_long_states_wo_all_changes_summed[!is.infinite(data_long_states_wo_all_changes_summed$measurement),]
# data_long_states_wo_all_changes_summed <- data_long_states_wo_all_changes_summed[!is.na(data_long_states_wo_all_changes_summed$measurement),]
# 
# 
# p_states_all_summed <- ggplot(data_long_states_wo_all_changes_summed,aes(x=condition,y=measurement))+
#   geom_boxplot(width=0.5, aes(fill=condition), alpha=0.2) + #,draw_quantiles = c(0.25, 0.5, 0.75)
#   geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp) +
#   theme_classic() +
#   theme(legend.position = "none",
#         plot.title = element_text(color = "black", size = 10),
#         axis.ticks = element_line(colour="black"),
#         axis.text.x = element_text(color = "black", size = 10),
#         axis.text.y = element_text(color = "black", size = 10),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   labs(fill = "Constructs and conditions") +
#   ggtitle("Number of state changes \nrelative to trajectory length") +
#   scale_y_continuous(limits = c(0,0.25), breaks = seq(0,0.25,by = 0.05)) +
#   stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
# p_states_all_summed
# svg("state_changes_wo_treatment_all_summed.svg",  width=2, height=3)
# p_states_all_summed
# dev.off()

#ONLY RELEVANT
data_long_states_wo_treatment <- data_long_states_wo_treatment[which(data_long_states_wo_treatment$movement == "all_to_stop" | 
                                                                       data_long_states_wo_treatment$movement == "stop_to_all" | 
                                                                       data_long_states_wo_treatment$movement == "retr_to_stop_ant"| 
                                                                       data_long_states_wo_treatment$movement == "ant_to_stop_retr"| 
                                                                       data_long_states_wo_treatment$movement == "all_changes"),]
#To order the conditions manually
data_long_states_wo_treatment$condition <- factor(data_long_states_wo_treatment$condition, levels=c("Tau441", "Tau421"))
#my_comparisons <- list( c("Tau441", "Tau421"))
cbp <- c("#034e61", "#a00000")
p_states_wo_all <- ggplot(data_long_states_wo_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 5) +
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
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,0.7,by = 0.05)) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_states_wo_all
svg("state_changes_wo_treatment_all.svg",  width=7, height=3)
p_states_wo_all
dev.off()

#WITH TREATMENT
data_long_states_with_treatment <- data_long_states[which(data_long_states$condition == "Tau441_001percentDMSO" | 
                                                          data_long_states$condition == "Tau421_001percentDMSO" |
                                                          data_long_states$condition == "Tau441_5nM_EpoD" |
                                                          data_long_states$condition == "Tau421_5nM_EpoD"|
                                                          data_long_states$condition == "Tau441_25nM_EpoD"|
                                                          data_long_states$condition == "Tau421_25nM_EpoD"),]

#ONLY RELEVANT
data_long_states_with_treatment <- data_long_states_with_treatment[which(data_long_states_with_treatment$movement == "all_to_stop" | 
                                                                           data_long_states_with_treatment$movement == "stop_to_all" | 
                                                                           data_long_states_with_treatment$movement == "retr_to_stop_ant"| 
                                                                           data_long_states_with_treatment$movement == "ant_to_stop_retr"| 
                                                                           data_long_states_with_treatment$movement == "all_changes"),]
#To order the conditions manually
data_long_states_with_treatment$condition <- factor(data_long_states_with_treatment$condition, levels=c("Tau441_001percentDMSO", "Tau421_001percentDMSO", "Tau441_5nM_EpoD", "Tau421_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421_25nM_EpoD"))
#plot histograms for collected filtered results
my_comparisons <- list(
  c("Tau441_001percentDMSO", "Tau421_001percentDMSO"), 
  c("Tau441_001percentDMSO", "Tau441_5nM_EpoD"),
  c("Tau441_001percentDMSO", "Tau441_25nM_EpoD"), 
  c("Tau421_001percentDMSO", "Tau421_5nM_EpoD"),
  c("Tau421_001percentDMSO", "Tau421_25nM_EpoD")#,
  #c("Tau441_5nM_EpoD", "Tau421_5nM_EpoD"),
  #c("Tau441_25nM_EpoD", "Tau421_25nM_EpoD"),
)
cbp <- c("#034e61", "#a00000",
         "#034e61", "#a00000",
         "#034e61", "#a00000")
#converting data to between wide and long format

p_states_with_all <- ggplot(data_long_states_with_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 5) +
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
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,0.7,by = 0.05)) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)

p_states_with_all
svg("state_changes_with_treatment_all.svg",  width=10, height=3)
p_states_with_all
dev.off()









#####TRAJECTORIES

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
#data_merge_states$condition <- as.factor(data_merge_states$condition)

# ALL STATE CHANGES
data_long_states_trajectories <- gather(data_merge_states_trajectories, movement, measurement, RS:all_changes, factor_key=TRUE)

#some zeroes are weird and can not be plotted
#data_long_states_trajectories$measurement <- as.numeric(data_long_states_trajectories$measurement)


#WITHOUT TREATMENT
data_long_states_trajectories_wo_treatment <- data_long_states_trajectories[which(data_long_states_trajectories$condition == "Tau441" | data_long_states_trajectories$condition == "Tau421"),]
#To order the conditions manually
data_long_states_trajectories_wo_treatment$condition <- factor(data_long_states_trajectories_wo_treatment$condition, levels=c("Tau441", "Tau421"))
my_comparisons <- list( c("Tau441", "Tau421"))
#cbp <- c("#3122D2", "#E83431")
cbp <- c("#034e61", "#a00000")

data_long_states_trajectories_wo_treatment <- data_long_states_trajectories_wo_treatment[which(data_long_states_trajectories_wo_treatment$movement == "all_to_stop" | 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "stop_to_all" | 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "retr_to_stop_ant"| 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "ant_to_stop_retr"| 
                                                                                                 data_long_states_trajectories_wo_treatment$movement == "all_changes"),]
#To order the conditions manually
data_long_states_trajectories_wo_treatment$condition <- factor(data_long_states_trajectories_wo_treatment$condition, levels=c("Tau441", "Tau421"))
#my_comparisons <- list( c("Tau441", "Tau421"))
cbp <- c("#034e61", "#a00000")


p_states_traj_wo_all <- ggplot(data_long_states_trajectories_wo_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 5) +
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
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(-0.001,1.25), breaks = seq(0,1.2,by = 0.1)) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_states_traj_wo_all
svg("state_changes_trajectories_wo_treatment_all.svg",  width=7, height=3)
p_states_traj_wo_all
dev.off()


#WITH TREATMENT
data_long_states_trajectories_with_treatment <- data_long_states_trajectories[which(data_long_states_trajectories$condition == "Tau441_001percentDMSO" | 
                                                                         data_long_states_trajectories$condition == "Tau421_001percentDMSO" |
                                                                         data_long_states_trajectories$condition == "Tau441_5nM_EpoD" |
                                                                         data_long_states_trajectories$condition == "Tau421_5nM_EpoD"|
                                                                         data_long_states_trajectories$condition == "Tau441_25nM_EpoD"|
                                                                         data_long_states_trajectories$condition == "Tau421_25nM_EpoD"),]

#ONLY RELEVANT
data_long_states_trajectories_with_treatment <- data_long_states_trajectories_with_treatment[which(data_long_states_trajectories_with_treatment$movement == "all_to_stop" | 
                                                                                        data_long_states_trajectories_with_treatment$movement == "stop_to_all" | 
                                                                                        data_long_states_trajectories_with_treatment$movement == "retr_to_stop_ant"| 
                                                                                        data_long_states_trajectories_with_treatment$movement == "ant_to_stop_retr"| 
                                                                                        data_long_states_trajectories_with_treatment$movement == "all_changes"),]
#To order the conditions manually
data_long_states_trajectories_with_treatment$condition <- factor(data_long_states_trajectories_with_treatment$condition, levels=c("Tau441_001percentDMSO", "Tau421_001percentDMSO", "Tau441_5nM_EpoD", "Tau421_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421_25nM_EpoD"))
#plot histograms for collected filtered results
my_comparisons <- list(
  c("Tau441_001percentDMSO", "Tau421_001percentDMSO"), 
  c("Tau441_001percentDMSO", "Tau441_5nM_EpoD"),
  c("Tau441_001percentDMSO", "Tau441_25nM_EpoD"), 
  c("Tau421_001percentDMSO", "Tau421_5nM_EpoD"),
  c("Tau421_001percentDMSO", "Tau421_25nM_EpoD")#,
  #c("Tau441_5nM_EpoD", "Tau421_5nM_EpoD"),
  #c("Tau441_25nM_EpoD", "Tau421_25nM_EpoD"),
)
cbp <- c("#034e61", "#a00000",
         "#034e61", "#a00000",
         "#034e61", "#a00000")
#converting data to between wide and long format

p_states_traj_with_all <- ggplot(data_long_states_trajectories_with_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 5) +
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
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  labs(fill = "Constructs and conditions") +
  ggtitle("Number of state changes relative to trajectory length") +
  scale_y_continuous(limits = c(-0.001,2.3), breaks = seq(0,1.4,by = 0.1)) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)

p_states_traj_with_all
svg("state_changes_trajectories_with_treatment_all.svg",  width=10, height=3)
p_states_traj_with_all
dev.off()






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
data_merge_fractions$condition <- as.factor(data_merge_fractions$condition)
levels(data_merge_fractions$condition)

data_merge_fractions$Moving <- data_merge_fractions$Anterograde + data_merge_fractions$Retrograde
data_merge_fractions$Relative <- data_merge_fractions$Moving/(data_merge_fractions$Stationary+data_merge_fractions$Moving)
write.table(data_merge_fractions[,c("condition",   "Relative")], paste("cell_means_", "data_merge_fractions", "_stat_to_all_ratio.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)

data_long_fractions <- gather(data_merge_fractions, fraction, measurement, Relative, factor_key=TRUE)

#WITHOUT TREATMENT
data_long_fractions_wo_treatment <- data_long_fractions[which(data_long_fractions$condition == "Tau441" | data_long_fractions$condition == "Tau421"),]
#To order the conditions manually
data_long_fractions_wo_treatment$condition <- factor(data_long_fractions_wo_treatment$condition, levels=c("Tau441", "Tau421"))
my_comparisons <- list( c("Tau441", "Tau421"))
cbp <- c("#034e61", "#a00000")


p_mov_wo <- ggdensity(data_long_fractions_wo_treatment, x = "measurement", 
          fill = "condition", color = "condition",
          add = "mean", rug = TRUE, alpha = .5) +
  facet_grid(rows = vars(condition)) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  #geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("Density of ratios of time spent \nmoving to all time with mean") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
  #stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_mov_wo
svg("Density of ratios of time spent moving to all time with mean no treatment.svg",  width=3, height=2)
p_mov_wo
dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_wo_median <- ggplot(data_long_fractions_wo_treatment, 
       aes(x = measurement, y = condition,
           color = condition)) + #fill = condition, 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,jittered_points = TRUE, scale = 0.7) #position = "raincloud", 
p_mov_wo_median
svg("Density of ratios of time spent moving to all time with median no treatment.svg",  width=3, height=2)
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
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2))
p_mov_overlap
g("Density of ratios of time spent moving to all time overlap no treatment.svg",  width=3, height=2)
p_mov_overlap
dev.off()

#WITH TREATMENT
data_long_fractions_with_treatment <- data_long_fractions[which(data_long_fractions$condition == "Tau441_001percentDMSO" | 
                                                                            data_long_fractions$condition == "Tau421_001percentDMSO" |
                                                                            data_long_fractions$condition == "Tau441_5nM_EpoD" |
                                                                            data_long_fractions$condition == "Tau421_5nM_EpoD"|
                                                                            data_long_fractions$condition == "Tau441_25nM_EpoD"|
                                                                            data_long_fractions$condition == "Tau421_25nM_EpoD"),]
#To order the conditions manually
data_long_fractions_with_treatment$condition <- factor(data_long_fractions_with_treatment$condition, levels=c("Tau441_001percentDMSO", "Tau421_001percentDMSO", "Tau441_5nM_EpoD", "Tau421_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421_25nM_EpoD"))
#plot histograms for collected filtered results
my_comparisons <- list(
  c("Tau441_001percentDMSO", "Tau421_001percentDMSO"), 
  c("Tau441_001percentDMSO", "Tau441_5nM_EpoD"),
  c("Tau441_001percentDMSO", "Tau441_25nM_EpoD"), 
  c("Tau421_001percentDMSO", "Tau421_5nM_EpoD"),
  c("Tau421_001percentDMSO", "Tau421_25nM_EpoD")#,
  #c("Tau441_5nM_EpoD", "Tau421_5nM_EpoD"),
  #c("Tau441_25nM_EpoD", "Tau421_25nM_EpoD"),
)
cbp <- c("#034e61", "#a00000",
         "#034e61", "#a00000",
         "#034e61", "#a00000")
p_mov_with <- ggdensity(data_long_fractions_with_treatment, x = "measurement", 
                      fill = "condition", color = "condition",
                      add = "mean", rug = TRUE, alpha = .5) +
  facet_grid(rows = vars(condition)) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  #geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("Density of ratios of time spent \nmoving to all time with mean") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
#stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_mov_with
svg("Density of ratios of time spent moving to all time with mean with treatment.svg",  width=3, height=2)
p_mov_with
dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_with_median <- ggplot(data_long_fractions_with_treatment, 
                          aes(x = measurement, y = condition,
                              color = condition)) + #fill = condition, 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,jittered_points = TRUE, scale = 0.7) #position = "raincloud", 
p_mov_with_median
svg("Density of ratios of time spent moving to all time with median with treatment.svg",  width=3, height=2)
p_mov_with_median
dev.off()






######## 1 additional
cbp <- c("#3122D2", "#E83431",
         "#574AE2", "#EF6F6C",
         "#7D73E8", "#F28E8C",
         "#A29BEF","#F6AFAE")
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
data_merge_fractions_trajectories$condition <- as.factor(data_merge_fractions_trajectories$condition)


data_merge_fractions_trajectories$Moving <- data_merge_fractions_trajectories$Anterograde + data_merge_fractions_trajectories$Retrograde
data_merge_fractions_trajectories$Relative <- data_merge_fractions_trajectories$Moving/(data_merge_fractions_trajectories$Stationary+data_merge_fractions_trajectories$Moving)
write.table(data_merge_fractions_trajectories[,c("condition",   "Relative")], paste("cell_means_", "data_merge_fractions", "_stat_to_all_ratio.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)

data_long_fractions_trajectories <- gather(data_merge_fractions_trajectories, fraction, measurement, Relative, factor_key=TRUE)


#WITHOUT TREATMENT
data_long_fractions_trajectories_wo_treatment <- data_long_fractions_trajectories[which(data_long_fractions_trajectories$condition == "Tau441" | data_long_fractions_trajectories$condition == "Tau421"),]
#To order the conditions manually
data_long_fractions_trajectories_wo_treatment$condition <- factor(data_long_fractions_trajectories_wo_treatment$condition, levels=c("Tau441", "Tau421"))
my_comparisons <- list( c("Tau441", "Tau421"))
#cbp <- c("#3122D2", "#E83431")
cbp <- c("#034e61", "#a00000")

p_mov_traj_wo <- ggdensity(data_long_fractions_trajectories_wo_treatment, x = "measurement", 
                      fill = "condition", color = "condition",
                      add = "mean", rug = TRUE, alpha = .5) +
  facet_grid(rows = vars(condition)) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  #geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("Density of ratios of time spent \nmoving to all time with mean") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
#stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_mov_traj_wo
svg("TRAJECTORIES Density of ratios of time spent moving to all time with mean no treatment.svg",  width=3, height=2)
p_mov_traj_wo
dev.off()


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
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,jittered_points = TRUE, scale = 0.7) #position = "raincloud", 
p_mov_traj_wo_median
svg("TRAJECTORIES Density of ratios of time spent moving to all time with median no treatment.svg",  width=3, height=2)
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
    #axis.title.x = element_blank(), 
    axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2))
p_mov_traj_overlap
svg("TRAJECTORIES Density of ratios of time spent moving to all time overlap no treatment.svg",  width=3, height=2)
p_mov_traj_overlap
dev.off()


#WITH TREATMENT
data_long_fractions_trajectories_with_treatment <- data_long_fractions_trajectories[which(data_long_fractions_trajectories$condition == "Tau441_001percentDMSO" | 
                                                                                          data_long_fractions_trajectories$condition == "Tau421_001percentDMSO" |
                                                                                          data_long_fractions_trajectories$condition == "Tau441_5nM_EpoD" |
                                                                                          data_long_fractions_trajectories$condition == "Tau421_5nM_EpoD"|
                                                                                          data_long_fractions_trajectories$condition == "Tau441_25nM_EpoD"|
                                                                                          data_long_fractions_trajectories$condition == "Tau421_25nM_EpoD"),]
#To order the conditions manually
data_long_fractions_trajectories_with_treatment$condition <- factor(data_long_fractions_trajectories_with_treatment$condition, levels=c("Tau441_001percentDMSO", "Tau421_001percentDMSO", "Tau441_5nM_EpoD", "Tau421_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421_25nM_EpoD"))
#plot histograms for collected filtered results
my_comparisons <- list(
  c("Tau441_001percentDMSO", "Tau421_001percentDMSO"), 
  c("Tau441_001percentDMSO", "Tau441_5nM_EpoD"),
  c("Tau441_001percentDMSO", "Tau441_25nM_EpoD"), 
  c("Tau421_001percentDMSO", "Tau421_5nM_EpoD"),
  c("Tau421_001percentDMSO", "Tau421_25nM_EpoD")#,
  #c("Tau441_5nM_EpoD", "Tau421_5nM_EpoD"),
  #c("Tau441_25nM_EpoD", "Tau421_25nM_EpoD"),
)
cbp <- c("#034e61", "#a00000",
         "#034e61", "#a00000",
         "#034e61", "#a00000")
p_mov_traj_with <- ggdensity(data_long_fractions_trajectories_with_treatment, x = "measurement", 
                        fill = "condition", color = "condition",
                        add = "mean", rug = TRUE, alpha = .5) +
  facet_grid(rows = vars(condition)) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  #geom_jitter(width = 0.2, size = 0.5, aes(color=condition)) + #position = position_jitterdodge(seed = 1, dodge.width = 0.9)
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("Density of ratios of time spent \nmoving to all time with mean") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  scale_y_continuous(limits = c(-0.001,6.5), breaks = seq(0,6,by = 1)) #+
#stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_mov_traj_with
svg("TRAJECTORIES Density of ratios of time spent moving to all time with mean with treatment.svg",  width=3, height=2)
p_mov_traj_with
dev.off()


#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(ggridges)
# Add quantiles Q1, Q2 (median) and Q3
p_mov_traj_with_median <- ggplot(data_long_fractions_trajectories_with_treatment, 
                            aes(x = measurement, y = condition,
                                color = condition)) + #fill = condition, 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 10),
        axis.ticks = element_line(colour="black"),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),  
        #axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  ggtitle("Density of ratios of time spent \nmoving to all time \nwith median and quartiles") +
  scale_x_continuous(limits = c(-0.001,1), breaks = seq(0,1,by = 0.2)) +
  stat_density_ridges(quantile_lines = TRUE, aes(fill = condition), alpha = .2,jittered_points = TRUE, scale = 0.7) #position = "raincloud", 
p_mov_traj_with_median
svg("TRAJECTORIES Density of ratios of time spent moving to all time with median with treatment.svg",  width=3, height=2)
p_mov_traj_with_median
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
