###############################################
##Nataliya Trushina, 2021##
##Plot together mean for original data (not best fits) for all conditions##

#Requirements: separate folder with files starting with "for_plotting_mean_and_se_" for all analysed conditions.
###############################################
#Libraries
library(tidyr)
library(ggplot2)
library(Rmisc)
library(stringr)
library(plotly)
library(plyr)
library(ggpubr)
library(scales)

#Additional settings
#colorRampPalette takes multiple colors for gradient, use https://coolors.co/ for finding perfect color scheme
cbp_gradient <- colorRampPalette(c("#4f4f4f", "#D62246")) 
col_gradient <- colorRampPalette(c("gray", "black"))  

#Set current directory as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#read all files with selected name ending and extension
dfs <- Sys.glob(file.path("merged_table_*"))
#make data frame to collect data into through the loop
data_merge <- data.frame()
#loop through all files
for (i in dfs){  
  x <- read.table(i, sep = ";",header = TRUE)
  condition_name <- gsub(".csv", "",i)
  condition_name <- gsub("merged_table_final_", "",condition_name)
  print(condition_name)
  x$Condition <- condition_name
  #colnames(x) <- c("Filament ID", "Total length", "Cell Name", "Condition")
  data_merge <- rbind(data_merge, x)
}
#put in all parameters for plotting

data_merge$Condition[data_merge$Condition == 'wt'] <- 'WT'
data_merge$Condition[data_merge$Condition == 'php'] <- 'PHP'

data_merge$Condition <- as.factor(data_merge$Condition)
data_merge$Condition <- factor(data_merge$Condition, levels=c("WT", "PHP"))
data_merge$Original.Image.Name  <- as.factor(data_merge$Original.Image.Name)
#data_merge$`Cell Name` <- as.factor(data_merge$`Cell Name`)
#data_merge$`Total length` <- data_merge$`Total length`/13
#mu  <- ddply(data_merge, "Condition", summarise, grp.mean=mean(`Track.Displacement.X.Reference.Frame`))
cbp <- cbp_gradient(nlevels(data_merge$Condition))

data_merge$Mobility <- "Immobile"
data_merge$Mobility[data_merge$Track.Displacement.X.Reference.Frame > 0.75] <- "Mobile" # CC includes borders into stationary
data_merge$Mobility[data_merge$Track.Displacement.X.Reference.Frame < -0.75] <- "Mobile"
data_merge$Mobility <- as.factor(data_merge$Mobility)
data_merge$Mobility <- factor(data_merge$Mobility, levels=c("Mobile", "Immobile"))
cols <- col_gradient(nlevels(data_merge$Mobility))

p_percent_mobility <- ggplot(data_merge, aes(x = Condition)) +
  #facet_grid(~condition) +
  geom_bar(aes(color = Condition, fill = Mobility), position="fill", width = 0.5, size=1) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.ticks = element_line(colour="black")#,
    #axis.text.x = element_blank(),
    #legend.position = "none",
  ) +
  ylab("Percent counts") +
  xlab("") +
  scale_y_continuous(limits = c(0,1),breaks=seq(0, 1, 0.2))
p_percent_mobility

svg("mobility_percentage.svg", width = 2.5, height = 2.2)
p_percent_mobility
dev.off()


data_merge$Directionality <- "Stationary"
data_merge$Directionality[data_merge$Track.Displacement.X.Reference.Frame > 0.75] <- "Anterograde" 
data_merge$Directionality[data_merge$Track.Displacement.X.Reference.Frame < -0.75] <- "Retrograde" 
data_merge$Directionality <- factor(data_merge$Directionality, levels=c("Anterograde", "Stationary", "Retrograde"))
cols <- col_gradient(nlevels(data_merge$Directionality))

p_percent_counts <- ggplot(data_merge, aes(x = Condition)) +
  #facet_grid(~condition) +
  geom_bar(aes(color = Condition, fill = Directionality), position="fill", width = 0.5, size=1) +
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cols) +
  #dark_theme_gray() +
  theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.ticks = element_line(colour="black")#,
    #axis.text.x = element_blank(),
    #legend.position = "none",
  ) +
  ylab("Percent counts") +
  xlab("") +
  scale_y_continuous(limits = c(0,1),breaks=seq(0, 1, 0.2))
p_percent_counts

svg("mobility_percentage.svg", width = 2.5, height = 2.2)
p_percent_counts
dev.off()

# p_displ <- ggplot(data_merge, aes(x=Track.Displacement.X.Reference.Frame, y=Track.Number.of.Spots, colour = Condition)) +
#   geom_point(size=1.2, fill="white",  aes(colour = Condition)) + #, shape=shape_phosphosite
#   scale_colour_manual(values = cbp) +
#   theme_classic() +
#   geom_vline(xintercept = 0.75, colour="gray", linetype="dashed") +
#   geom_vline(xintercept = -0.75, colour="gray", linetype="dashed") +
#   scale_x_continuous(breaks=pretty_breaks(15)) + #breaks=seq(0, 300, 10)
#   scale_y_continuous(breaks=pretty_breaks(10))
# p_displ


write.table(data_merge, "mobility_table_all_conditions.csv", sep = ';', row.names = FALSE)

data_merge_mobile <- data_merge[data_merge$Mobility == "Mobile",]
data_merge_mobile$Track_quality <- 100*data_merge_mobile$Track.Number.of.Spots/((data_merge_mobile$Track.Duration+0.2)*5)
data_merge_mobile$Velocity <- data_merge_mobile$Track.Displacement.Length.Reference.Frame/data_merge_mobile$Track.Duration # velocity is given in s, does not need to be changed to frames
data_merge_mobile$Processivity <- data_merge_mobile$Track.Displacement.Length.Reference.Frame/data_merge_mobile$Track.Length.Reference.Frame
write.csv2(data_merge_mobile_means, paste("means_output.csv", sep=""), row.names = FALSE)

p_track_quality <- ggplot(data_merge_mobile, aes(x = Condition, y = Track_quality, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 0.7, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  scale_y_continuous(limits = c(0,101),breaks=pretty_breaks(15)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_track_quality

p_speed <- ggplot(data_merge_mobile, aes(x = Condition, y = `Track.Speed.Mean.Reference.Frame`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 0.7, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Speed") +
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  scale_y_continuous(breaks=pretty_breaks(15)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_speed

p_velocity <- ggplot(data_merge_mobile, aes(x = Condition, y = Velocity, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 0.7, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  scale_y_continuous(breaks=pretty_breaks(15)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_velocity

p_processivity <- ggplot(data_merge_mobile, aes(x = Condition, y = Processivity, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 0.7, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  scale_y_continuous(breaks=pretty_breaks(15)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_processivity

library(dplyr)

data_merge_to_average <- data_merge_mobile[c("Original.Image.Name", "Track.Displacement.Length.Reference.Frame",
                                             "Track.Duration", "Track_quality",
                                             "Track.Length.Reference.Frame", 
                                             "Track.Speed.Mean.Reference.Frame", "Track.Speed.Max.Reference.Frame",
                                             "Velocity", "Processivity")]
df_for_plot_mean <- data_merge_to_average %>% group_by(`Original.Image.Name`) %>% dplyr::summarise(across(everything(), list(mean))) 
colnames(df_for_plot_mean) <- c("Original.Image.Name", "Track.Displacement.Length.Reference.Frame",
                                "Track.Duration", "Track_quality",
                                "Track.Length.Reference.Frame", 
                                "Track.Speed.Mean.Reference.Frame", "Track.Speed.Max.Reference.Frame",
                                "Velocity", "Processivity")

#data_merge_mobile_means_agg <- aggregate(data_merge_mobile[, 10:18], list(data_merge_mobile$Original.Image.Name), mean)
# data_merge_mobile_means_agg <- select(data_merge_mobile_means_agg, -Condition)
# data_merge_mobile_means_agg <- select(data_merge_mobile_means_agg, -Mobility)
# data_merge_mobile_means_agg <- select(data_merge_mobile_means_agg, -Directionality)
# names(data_merge_mobile_means_agg)[names(data_merge_mobile_means_agg) == 'Group.1'] <- 'Original.Image.Name'
data_merge_mobile_means <- left_join(df_for_plot_mean, data_merge_mobile[c("Original.Image.Name", "Condition")], by = "Original.Image.Name")
data_merge_mobile_means <- data_merge_mobile_means[!duplicated(data_merge_mobile_means),]

p_mean_track_quality <- ggplot(data_merge_mobile_means, aes(x = Condition, y = `Track_quality`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Track displacement length [\u03BCm]") +
  xlab("") #+
#ggtitle("Speed") +
#scale_x_continuous() + #breaks=pretty_breaks(15)
#scale_y_continuous(breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_track_quality

p_mean_track_displacement_length <- ggplot(data_merge_mobile_means, aes(x = Condition, y = `Track.Displacement.Length.Reference.Frame`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Track displacement length [\u03BCm]") +
  xlab("") #+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  #scale_y_continuous(breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_track_displacement_length

p_mean_track_duration <- ggplot(data_merge_mobile_means, aes(x = Condition, y = `Track.Duration`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Track duration [s]") +
  xlab("") #+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  #scale_y_continuous(limits = c(0,60), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_track_duration

p_mean_track_length <- ggplot(data_merge_mobile_means, aes(x = Condition, y = `Track.Length.Reference.Frame`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Track length [\u03BCm]") +
  xlab("") #+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  #scale_y_continuous(limits = c(0,12), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_track_length

p_mean_speed <- ggplot(data_merge_mobile_means, aes(x = Condition, y = `Track.Speed.Mean.Reference.Frame`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Mean speed [\u03BCm/s]") +
  xlab("") #+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  #scale_y_continuous(limits = c(0,0.9), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_speed

p_max_speed <- ggplot(data_merge_mobile_means, aes(x = Condition, y = `Track.Speed.Max.Reference.Frame`, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Max speed [\u03BCm/s]") +
  xlab("") #+
#ggtitle("Speed") +
#scale_x_continuous() + #breaks=pretty_breaks(15)
#scale_y_continuous(limits = c(0,0.9), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_max_speed

p_mean_velocity <- ggplot(data_merge_mobile_means, aes(x = Condition, y = Velocity, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Velocity = displacement length / track duration") +
  xlab("") #+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  #scale_y_continuous(limits = c(0,0.9), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_velocity

p_mean_processivity <- ggplot(data_merge_mobile_means, aes(x = Condition, y = Processivity, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Processivity [ratio] = displacement length / track length") +
  xlab("") #+
  #ggtitle("Speed") +
  #scale_x_continuous() + #breaks=pretty_breaks(15)
  #scale_y_continuous(limits = c(0,0.9), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_mean_processivity

p_max_speed <- ggplot(data_merge_mobile_means, aes(x = Condition, y = Track.Speed.Max.Reference.Frame, color = Condition, fill = Condition)) + 
  geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
  #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5, 
  #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
  geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
  theme_classic() + expand_limits(y = 0)+
  scale_color_manual(values = cbp) +
  scale_fill_manual(values = cbp) +
  stat_compare_means(method = "t.test")+
  ylab("Maximum speed [\u03BCm/s]") +
  xlab("") #+
#ggtitle("Speed") +
#scale_x_continuous() + #breaks=pretty_breaks(15)
#scale_y_continuous(limits = c(0,0.9), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
p_max_speed

p_empty <- ggplot()

all_plot <- ggarrange(p_mean_track_duration, p_mean_track_length, p_mean_track_displacement_length, p_empty,
                      p_mean_velocity, p_mean_processivity, p_mean_speed, p_max_speed,
                      #labels = c("A", "B", "C", "D", "E", "F"),
                      ncol = 4, nrow = 2,
                      common.legend = TRUE, legend = "bottom")
all_plot


ggsave(all_plot, filename = "all_mean_plots_together.png", type = "cairo", width = 10, height = 10)

# ggsave(all_plot, filename = "all_mean_plots_together.svg", width = 10, height = 10)

# p_mean_straightness <- ggplot(data_merge_mobile_means, aes(x = Condition, y = Track.Straightness.Reference.Frame, color = Condition, fill = Condition)) +
#   geom_boxplot(width=0.5, aes(fill=Condition), alpha=0.2) +
#   #geom_violin(aes(color=Condition, fill=Condition), alpha = 0.2) + #width=0.5,
#   #geom_point(aes(color=condition), position = position_jitterdodge(), size = 0.7) + #, position = position_jitterdodge()
#   geom_jitter(aes(color=Condition), size = 2, width = 0.2) +
#   theme_classic() +
#   scale_color_manual(values = cbp) +
#   scale_fill_manual(values = cbp) +
#   stat_compare_means(method = "t.test")+
#   ylab("Processivity [ratio] = displacement length / track length") +
#   xlab("") #+
# #ggtitle("Speed") +
# #scale_x_continuous() + #breaks=pretty_breaks(15)
# #scale_y_continuous(limits = c(0,0.9), breaks=pretty_breaks(10)) #limits = c(-0.1,1.1),breaks=seq(-0.1, 1.1, 0.1)
# p_mean_straightness
