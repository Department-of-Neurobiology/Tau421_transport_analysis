###############################################
##Nataliya Trushina, 2020##
##Kymograph-like analysis of trajectories##

#Requirements: xls table with Displacement X Reference Frame and list of ids for mobile vesicles

#When working with Excel files from different OneDrive account sometimes they can not be opened with read_excel()
#open the file and save it again or use linux subsystem to automatically write them into new files to be able to open them
#unoconv -f xlsx *.xls
#for dir in *; do [ -d "$dir" ] && unoconv -f xlsx "$dir"/*.xls; done

#v3
#nrow(df_list[[i]])-1 > 24/49/99/149 instead of 1 - check again the lengths!! Should it be 10 less i.e. 14,39...? 
#check that the lists to write out are empty (if there are no long trajectories for the cell) if (n != 0)  and if(length(changes_datalist) != 0) 
#added
###############################################
#Libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(stringr)
library(plotly)
library(zoo)
library(ggpubr)
library(rlist)
library(ggdark)

#Set colors
#cols <- c("retrograde" = "#118ab2", "stationary" = "#ef476f", "anterograde" = "#06d6a0") #normal blue, red, and green
cols <- c("retrograde" = "#ff9955", "stationary" = "#000000", "anterograde" = "#87decd") #for 421 paper orange, black, and cyan
col_gradient <- colorRampPalette(c("#390099", "#9E0059", "#FF0054", "#FF5400", "#FFBD00")) #choose multiple colors for gradient
#col_gradient <- colorRampPalette(c("#FF0054")) #choose multiple colors for gradient

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#test with 
#condition <- "Tau441_001percentDMSO"
condition_folders <- Sys.glob(file.path("Tau*")) #check that only required folders start with Tau*

######## displacement plots only for two conditions were shown

#condition_folders <- c("Tau441") #, "Tau441"   "Tau421", 

for (condition in condition_folders) {
  print(condition)
  #condition <- "Tau421"
  setwd(paste("C:/Users/NITru/Downloads/CellSpecificData_ReExport/",condition,sep=""))
  create_empty_table <- function(num_rows, num_cols) {
    frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
    return(frame)
  }
  
  #for separate files
  #df <-  read_excel("Tau441_APP-GFP_0-01percentDMSO_cell07_czi_5e8aea3ee4065_hrm.xls", sheet = "Displacement Reference Frame", skip = 1)
  #df_ID_table <- read_excel("Tau441_APP-GFP_0-01percentDMSO_cell07_TibcoExport_Copy.xlsx", sheet = "Tabelle1")
  
  #for folders
  #make a vector with IDs for mobile, the table contains only trajectories longer than 10!
  full_ID_table <- read_excel("../mCherryTau441_Tau421_APP-eGFP_merged_table_TibcoSpotfireExport.xlsx", sheet = "Tabelle1")
  mobile_ID_table <- full_ID_table[which(full_ID_table$Directionality!='Stationary'), ]
  mobile_ID_table$Original.ID <- as.factor(mobile_ID_table$`Original ID`)
  
  #renaming of rogue files
  old_filenames <- Sys.glob(file.path("*hrm*"))
  old_filenames
  for (i in old_filenames){  
    better_name <- gsub("hrm_1","hrm",i)
    better_name
    file.rename(i, better_name)
  }
  
  #read in all files that can be opened
  filenames <- Sys.glob(file.path("*_hrm.xlsx"))
  #for 421
  #filenames <- "Tau421_APP-GFPcell16_czi_5d9241565fbe6_hrm.xlsx"
  #filenames <- c("Tau421_APP-GFPcell02_czi_5d924155ac8f6_hrm.xlsx", "Tau421_APP-GFPcell05_czi_5d924155d071b_hrm.xlsx", "Tau421_APP-GFPcell09_czi_5d9241560c14b_hrm.xlsx", "Tau421_APP-GFPcell11_czi_5d92415623f97_hrm.xlsx", "Tau421_APP-GFPcell18_czi_5d92415677dd2_hrm.xlsx", "Tau421_APP-GFPcell20_czi_5d9241568fd3e_hrm.xlsx")
  #for 441
  #filenames <- "Tau441_APP-GFPcell08_czi_5d92f22054be3_hrm.xlsx"
  #filenames <- c("Tau441_APP-GFPcell01_czi_5d92f2200182d_hrm.xlsx", "Tau441_APP-GFPcell07_czi_5d92f22048d4e_hrm.xlsx", "Tau441_APP-GFPcell08_czi_5d92f22054be3_hrm.xlsx", "Tau441_APP-GFPcell11_czi_5d92f220785b8_hrm.xlsx", "Tau441_APP-GFPcell13_czi_5d92f22090077_hrm.xlsx", "Tau441_APP-GFPcell17_czi_5d92f220bf1e8_hrm.xlsx", "Tau441_APP-GFPcell26_czi_5e21eb5f5b3ce_hrm.xlsx", "Tau441_APP-GFPcell27_czi_5e21eb5f67642_hrm.xlsx", "Tau441_APP-GFPcell28_czi_5e21eb5f73539_hrm.xlsx")
  
  fractions_all_cells_datalist <- list()
  changes_all_cells_datalist <- list()
  
  for (x in filenames){  
    #print(x)
    #x <-"Tau441_APP-GFPcell01_czi_5d92f2200182d_hrm.xlsx"
    #x <-"Tau421_APP-GFPcell11_czi_5d92415623f97_hrm.xlsx"
    df <- read_excel(x, sheet = "Displacement Reference Frame", skip = 1)
    df$TrackID <- as.factor(df$TrackID)
    df_subset <- df[df$TrackID %in% mobile_ID_table$Original.ID,]
    
    #df_subset <- df_subset[which(df_subset$TrackID == "1000037660"),] #to plot one trajectory 1000037660 Tau421_001percentDMSO, Tau421_APP-GFP_0-01percentDMSO_cell16_czi_5ecbdd0f6adf5_hrm.xlsx
    df_subset <- droplevels(df_subset)
    nlevels(df_subset$TrackID)
    cbp <- col_gradient(nlevels(df_subset$TrackID))
    
    # ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = `Displacement Y Reference Frame`)) + 
    #   geom_path(aes(color = TrackID)) +
    #   #theme_classic() +
    #   ggtitle("Trajectories mobile fraction only") +
    #   xlab("Displacement X Reference Frame") + 
    #   ylab("Displacement Y Reference Frame") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_x_continuous(breaks = seq(-5, 5, by = 0.5))
    # #scale_x_reverse() +
    
    
    #ONLY WORKS FOR ONE TRAJECTORY WELL
    #time_gradient <- colorRampPalette(c("#390099", "#9E0059", "#FF0054", "#FF5400", "#FFBD00")) #choose multiple colors for gradient
    #cbp_time <- time_gradient(nlevels(as.factor(df_subset$Time)))
    #mid <- max(df_subset$Time)/2
    # ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = `Displacement Y Reference Frame`)) + 
    #   geom_path(aes(color = Time)) +
    #   #geom_point(aes(fill = as.factor(Time)), shape=21, colour="black") +
    #   #geom_point(aes(color = Time)) +
    #   #scale_color_manual(values = cbp) +
    #   #scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
    #                         #high = "red", space = "Lab" ) +
    #   scale_color_gradientn(colours = rainbow(7),limits=c(0,300), breaks=c(0,300,150)) +
    #   guides(colour = guide_colourbar(reverse=T)) +
    #   dark_theme_gray() +
    #   #theme(legend.position = "none") +
    #   #ggtitle("Trajectories mobile fraction only") +
    #   xlab("Displacement X Reference Frame") + 
    #   ylab("Displacement Y Reference Frame") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_y_continuous(breaks = seq(-15, 15, by = 0.5)) + #limits = c(-1,1), 
    #   scale_x_continuous(breaks = seq(-15, 15, by = 0.5)) #limits = c(-1,1), 
    #   
    # ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = `Displacement Y Reference Frame`)) + 
    #   geom_path(aes(color = TrackID)) +
    #   scale_color_manual(values = cbp) +
    #   dark_theme_gray() +
    #   theme(legend.position = "none") +
    #   #theme_classic() +
    #   xlab("Displacement X Reference Frame") + 
    #   ylab("Displacement Y Reference Frame") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_x_continuous(breaks = seq(-5, 5, by = 0.5))
    
    #To avoid uninitialized column warning try:
    df_subset$num <- NA
    df_subset$smoothed_x_displacement <- NA
    
    df_subset$num <- ave(df_subset$`Displacement X Reference Frame`, df_subset$TrackID, FUN = seq_along)
    
    ######NO FILTERING
    #df_subset$smoothed_x_displacement <- ave(df_subset$`Displacement X Reference Frame`, df_subset$TrackID, 
                                             #FUN= function(x) rollmean(x, k=10, na.pad=T) )
    df_subset$smoothed_x_displacement <- df_subset$`Displacement X Reference Frame`
                                             
    #df_subset <- na.omit(df_subset, cols="smoothed_x_displacement")
    
    
    # ggplot(df_subset, aes(x = smoothed_x_displacement, y = Time)) + 
    #   geom_path(aes(color = TrackID)) +
    #   scale_color_manual(values = cbp) +
    #   dark_theme_gray() +
    #   geom_path(aes(x = `Displacement X Reference Frame`, y = Time, color = TrackID), linetype = "dashed") +
    #   #theme_classic() +
    #   ggtitle("Trajectories, mobile fraction only, cut values out smoothing range") +
    #   xlab("Displacement smoothed by window = 5") + 
    #   ylab("Time") +
    #   scale_y_reverse()  +
    #   scale_x_continuous(breaks = seq(-5, 5, by = 0.5)) #+
    # #coord_flip()
    # 
    # ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = Time)) + 
    #   geom_path(aes(color = Time), size=1) +
    #   #geom_point(aes(fill = as.factor(Time)), shape=21, colour="black") +
    #   #geom_point(aes(color = Time)) +
    #   #scale_color_manual(values = cbp) +
    #   #scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
    #   #high = "red", space = "Lab" ) +
    #   dark_theme_gray() +
    #   scale_color_gradientn(colours = rainbow(7),limits=c(0,300), breaks=c(0,300,150)) +
    #   guides(colour = guide_colourbar(reverse=T)) +
    #   #theme(legend.position = "none") +
    #   xlab("Displacement X Reference Frame") + 
    #   ylab("Time") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_y_reverse() +
    #   scale_x_continuous(limits = c(-1,1), breaks = seq(-15, 15, by = 0.5))
    # 
    # ggplot(df_subset, aes(x = smoothed_x_displacement, y = Time)) + 
    #   geom_path(aes(color = Time), size=1) +
    #   #geom_point(aes(fill = as.factor(Time)), shape=21, colour="black") +
    #   #geom_point(aes(color = Time)) +
    #   #scale_color_manual(values = cbp) +
    #   #scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
    #   #high = "red", space = "Lab" ) +
    #   scale_color_gradientn(colours = rainbow(7),limits=c(0,300), breaks=c(0,300,150)) +
    #   dark_theme_gray() +
    #   #theme(legend.position = "none") +
    #   xlab("Displacement smoothed by window = 5") + 
    #   ylab("Time") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_y_reverse() +
    #   scale_x_continuous(limits = c(-1,1), breaks = seq(-15, 15, by = 0.5))
    # 
    # 
    #Kymograph analysis
    df_subset <- df_subset[order(df_subset$TrackID),]
    #To avoid uninitialized column warning try:
    df_subset$delta_x <- NA
    df_list <- list()
    df_list <- split(df_subset, df_subset$TrackID)
    for(i in 1:length(df_list)){
      #print(df_list[[i]]$TrackID[1])
      if (nrow(df_list[[i]]) > 1) {
        for (j in 2:nrow(df_list[[i]])) {
          #IMPORTANT time is not always 1 apart, delta_x = delta_disp/delta_t
          df_list[[i]]$delta_x[[j]] <- (df_list[[i]]$smoothed_x_displacement[[j]]-df_list[[i]]$smoothed_x_displacement[[j-1]])/(df_list[[i]]$Time[[j]]-df_list[[i]]$Time[[j-1]])
          #df_subset$angle[[i]] <- atan(1/(df_subset$smoothed_x_displacement[[i]]-df_subset$smoothed_x_displacement[[i-1]]))*180/pi #atan(1/(0.01))*180/pi #1/tan(89.5*pi/180)
          #df_subset$velocity[[i]] <- sign(df_subset$smoothed_x_displacement[[i]]-df_subset$smoothed_x_displacement[[i-1]])*(df_subset$smoothed_x_displacement[[i]]-df_subset$smoothed_x_displacement[[i-1]])
        }
      }  
    }
    df_subset <- do.call(rbind, df_list)
    
    #the lower border of mean minimum is 0.8 um per second if we divide by 5 (5 frames per second, 200ms each frame) then it was close to 0.02 which we chose as cut-off
    df_subset$motion <- ifelse(df_subset$delta_x < -0.02, "retrograde", ifelse(df_subset$delta_x > 0.02, "anterograde", "stationary"))
    
    # ggplot(na.omit(df_subset), aes(x = smoothed_x_displacement, y = Time)) + 
    #   #geom_path() + #aes(color = TrackID)
    #   scale_color_manual(values = cols) +
    #   dark_theme_gray() +
    #   geom_point(data = na.omit(df_subset), aes(color=motion), size=1) +
    #   #theme_classic() +
    #   ggtitle("No filtering") +
    #   xlab("Displacement smoothed by window = 5") + 
    #   ylab("Time") +
    #   scale_y_reverse() +
    #   scale_x_continuous(limits = c(-1,1),breaks = seq(-5, 5, by = 0.5))
    # 
    df_subset_naomit <- na.omit(df_subset, cols="delta_x")
    # #Filtering: if previous and next are same, change the middle one for next
    # #make for TrackID
    # count=0
    # df_list_2 <- list()
    # df_list_2 <- split(df_subset_naomit, df_subset_naomit$TrackID)
    # for(i in 1:length(df_list_2)){
    #   if (nrow(df_list_2[[i]])-1 > 1) { #24...
    #     for (j in 2:(nrow(df_list_2[[i]])-1)) {
    #       state <- ifelse(df_list_2[[i]]$motion[[j]]==df_list_2[[i]]$motion[[j-1]],"same","different")
    #       if (state=="different" ) {
    #         state_next <- ifelse(df_list_2[[i]]$motion[[j+1]]==df_list_2[[i]]$motion[[j-1]],"next_same","next_different")
    #         if (state_next=="next_same") {
    #           df_list_2[[i]]$motion[[j]]=df_list_2[[i]]$motion[[j-1]]
    #           count=count+1
    #         }
    #       }
    #     }
    #   }  
    # }
    # df_subset_naomit <- do.call(rbind, df_list_2)
    # #print(paste("Reset state", count, "times"))
    
    df_subset_naomit$motion <- as.factor(df_subset_naomit$motion)
    # summary(df_subset_naomit$motion)
    # summary(df_subset_naomit$TrackID)

    ##### FILTER TRAJECTORIES BY LENGTH
    # df_list_long_traj <- list()
    # df_list_long_traj <- split(df_subset_naomit, df_subset_naomit$TrackID)
    # df_for_plot <- data.frame()
    # length_list <- list()
    # for(i in 1:length(df_list_long_traj)){
    #   if (nrow(df_list_long_traj[[i]])-1 > 0) { #####HERE PUT 200 for long ones
    #     if (dim(df_for_plot)[1] == 0) {
    #       df_for_plot <- df_list_long_traj[[i]]
    #       length_list <- nrow(df_list_long_traj[[i]])
    #     }
    #     else {
    #       #print(nrow(df_list_long_traj[[i]]))
    #       df_for_plot <- rbind(df_for_plot,df_list_long_traj[[i]])
    #       length_list <- rbind(length_list, nrow(df_list_long_traj[[i]]))
    #     }
    #   }
    # }
    # 
    # 
    # 
    # 
    # length_list <- unlist(length_list)
    # checking_list <- tail(sort(length_list),5)
    # df_for_plot <- data.frame()
    # for(i in 1:length(df_list_long_traj)){
    #   if (nrow(df_list_long_traj[[i]]) %in% checking_list) { #####HERE PUT 200 for long ones
    #     if (dim(df_for_plot)[1] == 0) {
    #       print(nrow(df_list_long_traj[[i]]))
    #       df_for_plot <- df_list_long_traj[[i]]
    #       length_list <- nrow(df_list_long_traj[[i]])
    #     }
    #     else {
    #       print(nrow(df_list_long_traj[[i]]))
    #       df_for_plot <- rbind(df_for_plot,df_list_long_traj[[i]])
    #       length_list <- rbind(length_list, nrow(df_list_long_traj[[i]]))
    #     }
    #   }
    # }
    # 
    # 
    # if (dim(df_for_plot)[1] != 0) {
    #   df_for_plot_dataframe <- as.data.frame(df_for_plot)
    #   df_for_plot_dataframe$condition <- condition
    #   write.table(df_for_plot_dataframe, paste(x, "_df_for_plot.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
    # 
    #   # p <- ggplot(df_for_plot, aes(x = smoothed_x_displacement, y = Time)) + #, text = paste("TrackID:",TrackID)
    #   #   #geom_path(data = df_for_plot, aes(x = `Displacement X Reference Frame`, y = Time, color = TrackID), linetype = "dashed") + #has to have color = TrackID to be shown as separate
    #   #   geom_point(aes(color=motion), size=0.7) + #, shape=21
    #   #   scale_color_manual(values = cols) +
    #   #   #dark_theme_gray() +
    #   #   theme_classic() +
    #   #   ggtitle("Filtered points by direction") +
    #   #   xlab("Displacement smoothed by window = 5") +
    #   #   ylab("Time") +
    #   #   #theme(legend.position = "none") +
    #   #   scale_y_reverse() +
    #   #   #scale_y_continuous() +
    #   #   scale_x_continuous(limits=c(-1,1),breaks = seq(-10, 10, by = 0.5)) #limits = c(-5, 5), breaks = seq(-5, 5, by = 1)
    #   # p
    # 
    #   df_for_plot <- df_for_plot[which(df_for_plot$TrackID != "1000000350"),]
    #   df_for_plot$`Time (s)` <- df_for_plot$Time/5
    #   #We have chosen for representation 441 - cell 26, 421 - cell 10
    #   p <- ggplot(df_for_plot, aes(x = smoothed_x_displacement, y = `Time (s)`)) + #, text = paste("TrackID:",TrackID)
    #     #geom_path(data = df_for_plot, aes(x = `Displacement X Reference Frame`, y = Time, color = TrackID), linetype = "dashed") + #has to have color = TrackID to be shown as separate
    #     #facet_grid(~TrackID) +
    #     geom_point(aes(color=motion), size=0.4) + #, shape=21
    #     scale_color_manual(values = cols) +
    #     #dark_theme_gray() +
    #     theme_classic() +
    #     ggtitle(paste(condition,"displacement", sep=": ")) +
    #     #xlab("Displacement") +
    #     #ylab("Time") +
    #     theme(plot.title = element_text(hjust = 0.5),
    #           legend.position = "none",
    #           legend.title = element_blank(),
    #           axis.title.x=element_blank(),
    #           axis.text.x=element_blank(),
    #           axis.ticks.x=element_blank(),
    #           axis.ticks = element_line(colour="black"),
    #           axis.text.y = element_text(color = "black", size = 10)) +
    #     scale_y_reverse(limits = c(60, 0), breaks = seq(0, 60, by = 10)) +
    #     #scale_y_continuous() +
    #     scale_x_continuous(limits = c(-9, 9), breaks = seq(-8, 8, by = 2)) #limits = c(-2, 4), breaks = seq(-2, 4, by = 2)
    #   print(p)
    #   # #example output of filtered trajectories
    #   pdf(paste(paste("../final_displacement_plots_441_421/final_trajectories_example_", x, sep=""), ".pdf", sep=""),  width=3, height=3)
    #   print(p)
    #   dev.off()
    #   svg(paste(paste("../final_displacement_plots_441_421/final_trajectories_example_", x, sep=""), ".svg", sep=""),  width=3, height=3)
    #   print(p)
    #   dev.off()
    # }
    #####
    
    #count state fractions
    df_list_3 <- list()
    df_list_3 <- split(df_subset_naomit, df_subset_naomit$TrackID)
    
    fractions_datalist <- list()
    fractions_df <- create_empty_table(3,1)
    changes_datalist <- list()
    state_changes_df <- create_empty_table(11,1)
    #for each trajectory creates data lists for further assembly into tables
    for(i in 1:length(df_list_3)){
      #set row names later as trajectory IDs
      name <- df_list_3[[i]]$TrackID[1]

      #state changes for each trajectory
      retr_stat=0
      ant_stat=0
      retr_ant=0
      ant_retr=0
      stat_retr=0
      stat_ant=0
      
      #additional columns to count all changes to and from stops
      all_stat=0
      stat_all=0
      
      if (nrow(df_list_3[[i]])-1 > 1) { #24...
        #state percentage for each trajectory - IS IT REASONABLE? then further on they will be taken as mean for each cell
        fractions_df[1,] <- 100*summary(df_list_3[[i]]$motion)[1]/sum(summary(df_list_3[[i]]$motion))
        fractions_df[2,] <- 100*summary(df_list_3[[i]]$motion)[2]/sum(summary(df_list_3[[i]]$motion))
        fractions_df[3,] <- 100*summary(df_list_3[[i]]$motion)[3]/sum(summary(df_list_3[[i]]$motion))
        fractions_datalist[name] <-  as.list(fractions_df)
        for (j in 2:(nrow(df_list_3[[i]])-1)) {
          state_change <- ifelse(df_list_3[[i]]$motion[[j]]==df_list_3[[i]]$motion[[j-1]],"same","different")
          if (state_change=="different") {
            if (df_list_3[[i]]$motion[[j]]=="stationary" & df_list_3[[i]]$motion[[j-1]]=="retrograde") {
              retr_stat=retr_stat+1
            }
            if (df_list_3[[i]]$motion[[j]]=="stationary" & df_list_3[[i]]$motion[[j-1]]=="anterograde") {
              ant_stat=ant_stat+1
            }
            if (df_list_3[[i]]$motion[[j]]=="anterograde" & df_list_3[[i]]$motion[[j-1]]=="retrograde") {
              retr_ant=retr_ant+1
            }
            if (df_list_3[[i]]$motion[[j]]=="retrograde" & df_list_3[[i]]$motion[[j-1]]=="anterograde") {
              ant_retr=ant_retr+1
            }
            if (df_list_3[[i]]$motion[[j]]=="retrograde" & df_list_3[[i]]$motion[[j-1]]=="stationary") {
              stat_retr=stat_retr+1
            }
            if (df_list_3[[i]]$motion[[j]]=="anterograde" & df_list_3[[i]]$motion[[j-1]]=="stationary") {
              stat_ant=stat_ant+1
            }
            #optional: to count all changes to and from stops
            #if ((df_list_3[[i]]$motion[[j]]=="anterograde" | df_list_3[[i]]$motion[[j]]=="retrograde") & df_list_3[[i]]$motion[[j-1]]=="stationary") {
              #stat_all=stat_all+1
            #}
          }  
        }
        #relative to trajectory length, before was not divided, beware of the gaps time!=tracked_spots*0.2, can be higher, better take from Tibco table
        n <- (nrow(df_list_3[[i]]))
        if (n != 0) {
          track_duration <- (df_list_3[[i]]$Time[[n]]-df_list_3[[i]]$Time[[1]]+10)*0.2
          state_changes_df[1,] <- retr_stat/track_duration
          state_changes_df[2,] <- ant_stat/track_duration
          state_changes_df[3,] <- retr_ant/track_duration
          state_changes_df[4,] <- ant_retr/track_duration
          state_changes_df[5,] <- stat_retr/track_duration
          state_changes_df[6,] <- stat_ant/track_duration
          state_changes_df[7,] <- (retr_stat+ant_stat)/track_duration
          state_changes_df[8,] <- (stat_retr+stat_ant)/track_duration
          state_changes_df[9,] <- (retr_stat+retr_ant)/track_duration
          state_changes_df[10,] <- (ant_stat+ant_retr)/track_duration
          state_changes_df[11,] <- (stat_retr+stat_ant+retr_stat+retr_ant+ant_stat+ant_retr)/track_duration
          changes_datalist[name] <-  as.list(state_changes_df)
        }
      }
    }

    if(length(fractions_datalist) != 0) {
    assembled_state_fractions <- as.data.frame(do.call(rbind, fractions_datalist))
    assembled_state_fractions <- setNames(assembled_state_fractions,c("Anterograde","Retrograde","Stationary"))
    #barplot(t(assembled_state_fractions), legend = colnames(assembled_state_fractions))
    assembled_state_fractions_out <- assembled_state_fractions
    assembled_state_fractions_out$condition <- condition
    write.table(assembled_state_fractions_out, paste(x, "_assembled_state_fractions.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
    }
    
    if(length(changes_datalist) != 0) {
      assembled_state_changes <- as.data.frame(do.call(rbind, changes_datalist))
      assembled_state_changes <- setNames(assembled_state_changes,c("RS","AS","RA","AR","SR","SA","all_to_stop","stop_to_all","retr_to_stop_ant","ant_to_stop_retr","all_changes"))
      #barplot(t(assembled_state_changes), legend = colnames(assembled_state_changes))
      assembled_state_changes_out <- assembled_state_changes
      assembled_state_changes_out$condition <- condition
      write.table(assembled_state_changes_out, paste(x, "_assembled_state_changes.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
    }
    
    #CHECK for multiple files in folder - what if less 
    as.data.frame(colMeans(assembled_state_fractions))
    
    fractions_all_cells_datalist[x] <- as.data.frame(colMeans(assembled_state_fractions))
    changes_all_cells_datalist[x] <- as.data.frame(colMeans(assembled_state_changes))
    
    # Density plot
    # ggplot(df_subset_naomit, aes(x = delta_x)) + 
    #   geom_density(aes(fill = motion, color = motion), alpha = 0.4) +
    #   #geom_histogram(aes(fill = motion), alpha = 0.4,bins=500)+
    #   #theme_classic() +
    #   theme_classic() + 
    #   scale_color_manual(values=cols) +
    #   scale_fill_manual(values=cols) +
    #   ggtitle("Trajectories, mobile fraction only") +
    #   xlab("Delta_x") + 
    #   ylab("Density") 
    # 
    # # Basic density plot with mean line and marginal rug
    # ggdensity(df_subset_naomit, x = "delta_x", 
    #           fill = "motion", color = "motion", palette = cols,
    #           add = "mean", rug = TRUE)
    
    # Violin plots
    #View(df_subset_naomit)
    # ggplot(df_subset_naomit, aes(x = motion,y = delta_x, color = motion)) +
    #   geom_violin(trim = FALSE,draw_quantiles = c(0.25, 0.5, 0.75)) +
    #   geom_jitter(aes(color=as.factor(motion))) +
    #   theme_classic() + 
    #   scale_color_manual(values=cols) +
    #   coord_flip() 
      
    
    ####### velocity calculations
    #count state fractions
    # df_list_4 <- list()
    # df_list_4 <- split(df_subset_naomit, df_subset_naomit$TrackID)
    # 
    # velocity_datalist <- list()
    # velocity_df <- create_empty_table(4,1)
    # 
    #for each trajectory creates data lists for further assembly into tables
    # for(i in 1:length(df_list_4)){
    #   #for checking
    #   #i=1
    #   #View(df_list_4[[i]])
    #   
    #   #set row names later as trajectory IDs
    #   name <- as.character(df_list_4[[i]]$TrackID[1])
    #   
    #   #velocity for each directional part of trajectory
    #   retr_vel=0
    #   ant_vel=0
    #   retr_vel_list <- list()
    #   ant_vel_list <- list()
    #   
    #   if (nrow(df_list_4[[i]])-1 > 1) { #24...
    #     for (j in 2:(nrow(df_list_4[[i]]))) {
    #       first=j-1
    #       state_change <- ifelse(df_list_4[[i]]$motion[[j]]==df_list_4[[i]]$motion[[j-1]],"same","different")
    #       traj_end <- ifelse(j==nrow(df_list_4[[i]]),"end","middle")
    #       #print(state_change)
    #       if (first==1){
    #         if (state_change=="different" && traj_end=="middle") {
    #           #print(df_list_4[[i]]$motion[[first]])
    #           if (df_list_4[[i]]$motion[[first]]=="retrograde" ) {
    #             retr_vel = (df_list_4[[i]]$smoothed_x_displacement[[j-1]] - 0)/(df_list_4[[i]]$Time[[j-1]] - df_list_4[[i]]$Time[[first]] + 1)
    #             retr_vel_list <- list.append(retr_vel_list,retr_vel)
    #             #print(paste("retr",retr_vel,sep=" "))
    #             first=j
    #             next #is needed to avoid going from e.g. "SRAS" changes mistakes 
    #           }
    #           if (df_list_4[[i]]$motion[[first]]=="anterograde") {
    #             ant_vel = (df_list_4[[i]]$smoothed_x_displacement[[j-1]] - 0)/(df_list_4[[i]]$Time[[j-1]] - df_list_4[[i]]$Time[[first]] + 1)
    #             ant_vel_list <- list.append(ant_vel_list,ant_vel)
    #             #print(paste("ant",ant_vel,sep=" "))
    #             first=j
    #             next
    #           } 
    #           if (df_list_4[[i]]$motion[[first]]=="stationary") {
    #             first=j
    #             next
    #           }
    #         }
    #         if (traj_end=="end") {
    #           if (df_list_4[[i]]$motion[[j]]=="retrograde") {
    #             retr_vel = (df_list_4[[i]]$smoothed_x_displacement[[j]] - 0)/(df_list_4[[i]]$Time[[j]] - df_list_4[[i]]$Time[[first]] + 1)
    #             retr_vel_list <- list.append(retr_vel_list,retr_vel)
    #             #print(paste("retr",retr_vel,sep=" "))
    #             break
    #           }
    #           if (df_list_4[[i]]$motion[[j]]=="anterograde") {
    #             ant_vel = (df_list_4[[i]]$smoothed_x_displacement[[j]] - 0)/(df_list_4[[i]]$Time[[j]] - df_list_4[[i]]$Time[[first]] + 1)
    #             ant_vel_list <- list.append(ant_vel_list,ant_vel)
    #             #print(paste("ant",ant_vel,sep=" "))
    #             break
    #           }
    #         }
    #         next
    #       }
    #       if (first!=1){
    #         if (state_change=="different" && traj_end=="middle") {
    #           #print(df_list_4[[i]]$motion[[first]])
    #           if (df_list_4[[i]]$motion[[first]]=="retrograde" ) {
    #             retr_vel = (df_list_4[[i]]$smoothed_x_displacement[[j-1]] - df_list_4[[i]]$smoothed_x_displacement[[first-1]])/(df_list_4[[i]]$Time[[j-1]] - df_list_4[[i]]$Time[[first-1]])
    #             retr_vel_list <- list.append(retr_vel_list,retr_vel)
    #             #print(paste("retr",retr_vel,sep=" "))
    #             first=j
    #             next #is needed to avoid going from e.g. "SRAS" changes mistakes 
    #           }
    #           if (df_list_4[[i]]$motion[[first]]=="anterograde") {
    #             ant_vel = (df_list_4[[i]]$smoothed_x_displacement[[j-1]] - df_list_4[[i]]$smoothed_x_displacement[[first-1]])/(df_list_4[[i]]$Time[[j-1]] - df_list_4[[i]]$Time[[first-1]])
    #             ant_vel_list <- list.append(ant_vel_list,ant_vel)
    #             #print(paste("ant",ant_vel,sep=" "))
    #             first=j
    #             next
    #           } 
    #           if (df_list_4[[i]]$motion[[first]]=="stationary") {
    #             first=j
    #             next
    #           }
    #         }
    #         if (traj_end=="end") {
    #           if (df_list_4[[i]]$motion[[j]]=="retrograde") {
    #             retr_vel = (df_list_4[[i]]$smoothed_x_displacement[[j]] - df_list_4[[i]]$smoothed_x_displacement[[first-1]])/(df_list_4[[i]]$Time[[j]] - df_list_4[[i]]$Time[[first-1]])
    #             retr_vel_list <- list.append(retr_vel_list,retr_vel)
    #             #print(paste("retr",retr_vel,sep=" "))
    #             break
    #           }
    #           if (df_list_4[[i]]$motion[[j]]=="anterograde") {
    #             ant_vel = (df_list_4[[i]]$smoothed_x_displacement[[j]] - df_list_4[[i]]$smoothed_x_displacement[[first-1]])/(df_list_4[[i]]$Time[[j]] - df_list_4[[i]]$Time[[first-1]])
    #             ant_vel_list <- list.append(ant_vel_list,ant_vel)
    #             #print(paste("ant",ant_vel,sep=" "))
    #             break
    #           }
    #         }
    #       }
    #     } 
    #   } 
    #   if(length(retr_vel_list) != 0) {
    #     print(unlist(retr_vel_list))
    #     velocity_df[1,] <- mean(unlist(retr_vel_list))
    #     velocity_df[2,] <- min(unlist(retr_vel_list))
    #   }
    #   if(length(ant_vel_list) != 0) {
    #     velocity_df[3,] <- mean(unlist(ant_vel_list))
    #     velocity_df[4,] <- max(unlist(ant_vel_list))
    #   }
    #   velocity_datalist[name] <-  as.list(velocity_df)
    # } 
  }
  
  #assembled_state_fractions_all_cells <- as.data.frame(do.call(rbind, fractions_all_cells_datalist))
  #write.table(df_for_plot, paste(condition, "_df_for_plot.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
  
  assembled_state_fractions_all_cells <- as.data.frame(do.call(rbind, fractions_all_cells_datalist))
  assembled_state_fractions_all_cells <- setNames(assembled_state_fractions_all_cells,c("Anterograde","Retrograde","Stationary"))
  assembled_state_fractions_all_cells$condition <- condition
  write.table(assembled_state_fractions_all_cells, paste(condition, "_fractions_final.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
  
  assembled_state_changes_all_cells <- as.data.frame(do.call(rbind, changes_all_cells_datalist))
  assembled_state_changes_all_cells <- setNames(assembled_state_changes_all_cells,c("RS","AS","RA","AR","SR","SA","all_to_stop","stop_to_all","retr_to_stop_ant","ant_to_stop_retr","all_changes"))
  assembled_state_changes_all_cells$condition <- condition
  write.table(assembled_state_changes_all_cells, paste(condition, "_changes_final.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)

  # assembled_velocities_all_cells <- as.data.frame(do.call(rbind, velocity_datalist))
  # assembled_velocities_all_cells <- tibble::rownames_to_column(assembled_velocities_all_cells, "Traj_number")
  # assembled_velocities_all_cells <- setNames(assembled_velocities_all_cells,c("Traj_number", "Mean_retrograde", "Min_retrograde", "Mean_anterograde", "Max_anterograde"))
  # assembled_velocities_all_cells$condition <- condition
  # assembled_velocities_all_cells_long <- gather(assembled_velocities_all_cells, value, measurement, Mean_retrograde:Max_anterograde, factor_key=TRUE)
  # png(paste(condition, "_velocities_final.png", sep=""), width = 1000, height = 600) 
  # ggplot(assembled_velocities_all_cells_long,aes(x=value,y=measurement, color=value))+
  #   geom_boxplot() +
  #   geom_point(position = position_jitterdodge())
  # dev.off()
  # write.table(assembled_velocities_all_cells, paste(condition, "_velocities_final.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
}


#to save the last plot as plotly object
#p <- ggplotly(
#  p = ggplot2::last_plot()
#)
#save as an interactive html widget
#htmlwidgets::saveWidget(as_widget(p), paste(condition, "_densities.html", sep=))


#for(i in seq_along(levels(df_subset$TrackID))){
#  if(i != length(levels(df_subset$TrackID))){
#    print(levels(df_subset$TrackID)[i])
#    df_subset_by_TrackID <- data.frame()
#    df_subset %>% filter(TrackID %in% c(levels(TrackID)[i], levels(TrackID)[i+1])) %>%  as.data.frame() -> df_subset_by_TrackID
#    print(nrow(df_subset_by_TrackID))
#    for (j in 2:nrow(df_subset_by_TrackID)) {
#      df_subset_by_TrackID$delta_x[[j]] <- df_subset_by_TrackID$smoothed_x_displacement[[j]]-df_subset_by_TrackID$smoothed_x_displacement[[j-1]]
#      #df_subset$angle[[i]] <- atan(1/(df_subset$smoothed_x_displacement[[i]]-df_subset$smoothed_x_displacement[[i-1]]))*180/pi #atan(1/(0.01))*180/pi #1/tan(89.5*pi/180)
#      #df_subset$velocity[[i]] <- sign(df_subset$smoothed_x_displacement[[i]]-df_subset$smoothed_x_displacement[[i-1]])*(df_subset$smoothed_x_displacement[[i]]-df_subset$smoothed_x_displacement[[i-1]])
#    }
#  }
#  df_list[[i]] <- df_subset_by_TrackID
#}
#df_list

#rle(as.vector(df_subset_naomit$motion))
#v <- rle(df_subset_naomit$motion)$values
#v
#state_table <- table(v[-length(v)],v[-1])
#View(state_table)
#library(igraph)
#routes_igraph <- graph.adjacency(state_table, mode="directed", weighted=TRUE)
#plot(routes_igraph, edge.label=round(E(routes_igraph)$weight, 3), edge.arrow.size = 0.2)

#print changes of states
#print(paste("Stopped after moving retrograde", retr_stat, "times"))
#print(paste("Stopped after moving anterograde", ant_stat, "times"))
#print(paste("Changed movement from retrograde to anterograde", retr_ant, "times"))
#print(paste("Changed movement from anterograde to retrograde", ant_retr, "times"))
#print(paste("Started moving retrograde after stationary phase", stat_retr, "times"))
#print(paste("Started moving anterograde after stationary phase", stat_ant, "times"))
