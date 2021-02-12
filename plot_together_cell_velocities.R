###############################################
##Nataliya Trushina, 2021##
##Analysis of APP trajectories##

#Requirements: csv tables in subdirectories
###############################################
#Libraries
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(tidyr)
library(dplyr)

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

#To order the conditions manually
data_merge_velocities$condition <- factor(data_merge_velocities$condition, levels=c("Tau441", "Tau421", "Tau441_001percentDMSO", "Tau421_001percentDMSO", 
                                                                                    "Tau441_5nM_EpoD", "Tau421_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421_25nM_EpoD"))
#levels=c("Tau441", "Tau441_001percentDMSO", "Tau441_5nM_EpoD", "Tau441_25nM_EpoD", "Tau421", "Tau421_001percentDMSO", "Tau421_5nM_EpoD", "Tau421_25nM_EpoD")

#plot histograms for collected filtered results
# cbp <- c("#3122D2", "#E83431",
#          "#574AE2", "#EF6F6C",
#          "#7D73E8", "#F28E8C",
#         "#A29BEF","#F6AFAE")
#cbp <- c("#3122D2", "#574AE2", "#7D73E8", "#A29BEF", "#E83431", "#EF6F6C", "#F28E8C", "#F6AFAE")

#velocity to positive value
data_merge_velocities$Mean_retrograde <- -data_merge_velocities$Mean_retrograde 
data_merge_velocities$Min_retrograde <- -data_merge_velocities$Min_retrograde 
#head(data_merge_velocities)

#converting data to between wide and long format
data_long_velocities <- gather(data_merge_velocities, movement, measurement, Mean_retrograde:Max_anterograde, factor_key=TRUE)

#additional functions for further plotting
meanFunction <- function(x){
  return(data.frame(y=round(mean(x),2),label=round(mean(x,na.rm=T),2)))}
medianFunction <- function(x){
  return(data.frame(y=round(median(x),2),label=round(median(x,na.rm=T),2)))}

##### WITHOUT TREATMENT #####

data_long_velocities_wo_treatment <- data_long_velocities[which(data_long_velocities$condition == "Tau441" | data_long_velocities$condition == "Tau421"),]
cbp <- c("#034e61", "#a00000")
my_comparisons <- list(
  c("Tau441", "Tau421"))

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
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons) #+
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
p_vel
svg("data_long_velocities_wo_treatment_all_facetted.svg",  width=7, height=3)
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
  scale_y_continuous(limits = c(0,0.45), breaks = seq(0,0.45,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_vel_mean
svg("data_long_velocities_wo_treatment_mean.svg",  width=2, height=3)\
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
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_vel_max
svg("data_long_velocities_wo_treatment_max.svg",  width=2, height=3)
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
svg("data_long_velocities_wo_treatment_arrange_all.svg",  width=7, height=6)
arrange_wo
dev.off()


##### WITH TREATMENT #####

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

data_long_velocities_with_treatment <- data_long_velocities[which(data_long_velocities$condition == "Tau441_001percentDMSO" | data_long_velocities$condition == "Tau421_001percentDMSO" | data_long_velocities$condition == "Tau441_5nM_EpoD" | data_long_velocities$condition == "Tau421_5nM_EpoD" | data_long_velocities$condition == "Tau441_25nM_EpoD" | data_long_velocities$condition == "Tau421_25nM_EpoD"),]
#why did na and inf arise? had to get rid of them first
data_long_velocities_with_treatment <- data_long_velocities_with_treatment[!is.infinite(data_long_velocities_with_treatment$measurement),]
data_long_velocities_with_treatment <- data_long_velocities_with_treatment[!is.na(data_long_velocities_with_treatment$measurement),]
#colMax <- function(data) sapply(data, max, na.rm = TRUE)
#colMax(data_long_velocities_with_treatment$measurement) #does not work


#PLOT ALL FACETTED

p_vel_treat <- ggplot(data_long_velocities_with_treatment,aes(x=condition,y=measurement))+
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
  scale_y_continuous(limits = c(0,0.6), breaks = seq(0,0.4,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons) #+
#stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
p_vel_treat
svg("data_long_velocities_with_treatment_all_facetted.svg",  width=10, height=3)
p_vel_treat
dev.off()


#PLOT MEAN PER CELL
data_long_velocities_with_treatment_mean <- data_long_velocities_with_treatment[which(data_long_velocities_with_treatment$movement == "Mean_retrograde" | data_long_velocities_with_treatment$movement == "Mean_anterograde"),]
p_vel_treat_mean <- ggplot(data_long_velocities_with_treatment_mean,aes(x=condition,y=measurement))+
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
  scale_y_continuous(limits = c(0,0.6), breaks = seq(0,0.4,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_vel_treat_mean
svg("data_long_velocities_with_treatment_mean.svg",  width=3, height=3)
p_vel_treat_mean
dev.off()

#PLOT MAX PER CELL
data_long_velocities_with_treatment_max <- data_long_velocities_with_treatment[which(data_long_velocities_with_treatment$movement == "Min_retrograde" | data_long_velocities_with_treatment$movement == "Max_anterograde"),]
p_vel_treat_max <- ggplot(data_long_velocities_with_treatment_max,aes(x=condition,y=measurement))+
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
  scale_y_continuous(limits = c(0,0.6), breaks = seq(0,0.4,by = 0.05)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons)
p_vel_treat_max
svg("data_long_velocities_with_treatment_max.svg",  width=3, height=3)
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
          #heights = c(4,2),
          labels = "A" # Labels of the first plot
) 
arrange_with
svg("data_long_velocities_with_treatment_arrange_all.svg",  width=10, height=6)
arrange_with
dev.off()
