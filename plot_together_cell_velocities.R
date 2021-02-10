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
cbp <- c("#3122D2", "#E83431",
         "#574AE2", "#EF6F6C",
         "#7D73E8", "#F28E8C",
         "#A29BEF","#F6AFAE")
#cbp <- c("#3122D2", "#574AE2", "#7D73E8", "#A29BEF", "#E83431", "#EF6F6C", "#F28E8C", "#F6AFAE")

#velocity to positive value
data_merge_velocities$Retrograde <- -data_merge_velocities$Retrograde
#head(data_merge_velocities)

#converting data to between wide and long format
data_long_velocities <- gather(data_merge_velocities, movement, measurement, Anterograde:Retrograde, factor_key=TRUE)

#additional functions for further plotting
meanFunction <- function(x){
  return(data.frame(y=round(mean(x),2),label=round(mean(x,na.rm=T),2)))}
medianFunction <- function(x){
  return(data.frame(y=round(median(x),2),label=round(median(x,na.rm=T),2)))}

##### WITHOUT TREATMENT #####

data_long_velocities_wo_treatment <- data_long_velocities[which(data_long_velocities$condition == "Tau441" | data_long_velocities$condition == "Tau421"),]
cbp <- c("#3122D2", "#E83431")
#pdf("Velocity no treatment.pdf",  width=5, height=5)
ggplot(data_long_velocities_wo_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 2) +
  #geom_violin(width=1, aes(color=condition, fill=condition), alpha = 0.2, trim = FALSE) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_boxplot(width=0.5, aes(color=condition, fill=condition), alpha = 0.2) +
  geom_point(aes(color=condition), position = position_jitterdodge()) + 
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_bw() +
  ylab("Velocity (d(smoothed_disp)/d(time)), A&R combined") +
  theme(axis.title.x=element_blank(), legend.position = "none") + #legend.position = "none"
  #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 0) +
  stat_summary(fun = mean, geom = "point",colour = "black", size=3) +
  scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,by = 0.02)) + #pretty(data_long_velocities_wo_treatment$measurement, n = 10)
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441")
#dev.off()

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

data_long_velocities_with_treatment <- data_long_velocities[which(data_long_velocities$condition == "Tau441_001percentDMSO" | data_long_velocities$condition == "Tau421_001percentDMSO" | data_long_velocities$condition == "Tau441_5nM_EpoD" | data_long_velocities$condition == "Tau421_5nM_EpoD" | data_long_velocities$condition == "Tau441_25nM_EpoD" | data_long_velocities$condition == "Tau421_25nM_EpoD"),]
cbp <- c("#574AE2", "#EF6F6C",
         "#7D73E8", "#F28E8C",
         "#A29BEF","#F6AFAE")
#pdf("Velocity with treatment.pdf",  width=5, height=5)
ggplot(data_long_velocities_with_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 2) +
  #geom_violin(aes(color=condition, fill=condition), alpha = 0.2) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  geom_boxplot(width=0.5, aes(color=condition, fill=condition), alpha = 0.2) +
  geom_point(aes(color=condition), position = position_jitterdodge()) + 
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_bw() +
  ylab("Velocity (d(smoothed_disp)/d(time)), A&R combined") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) + #legend.position = "none"
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
  #stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 3, vjust = 1.3) +
  stat_summary(fun = mean, geom = "point",colour = "black", size=3) +
  scale_y_continuous(breaks = pretty(data_long_velocities_with_treatment$measurement, n = 10)) #+
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441_001percentDMSO")
#dev.off()

ggplot(data_long_velocities_with_treatment,aes(x=condition,y=measurement))+
  facet_wrap(~movement, ncol = 2) +
  geom_violin(aes(color=condition, fill=condition), alpha = 0.2, draw_quantiles = c(0.25, 0.5, 0.75)) + #trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75)
  #geom_boxplot(width=0.2) + #aes(color=condition, fill=condition),
  geom_point(aes(color=condition), position = position_jitterdodge()) + 
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  theme_bw() +
  ylab("Velocity (d(smoothed_disp)/d(time)), A&R combined") +
  theme(axis.title.x=element_blank(), legend.position = "none") + #axis.text.x=element_blank(), 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
  stat_summary(fun.data = meanFunction, geom ="text", color = "black", size = 5, vjust = 1.3) +
  stat_summary(fun.data = medianFunction, geom ="text", color = "red", size = 5, vjust = 3) +
  stat_summary(fun = mean, geom = "point",colour = "black", size=3) +
  scale_y_continuous(breaks = pretty(data_long_velocities_with_treatment$measurement, n = 10)) #+
#stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Tau441_001percentDMSO")