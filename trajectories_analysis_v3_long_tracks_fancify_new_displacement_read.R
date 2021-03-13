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
library(ggExtra)
library(gganimate)
library("gganimate") 
library("gifski") # gif renderer
library("transformr")
library(magick)

#Set colors
cols <- c("retrograde" = "#118ab2", "stationary" = "#ef476f", "anterograde" = "#06d6a0")
col_gradient <- colorRampPalette(c("#390099", "#9E0059", "#FF0054", "#FF5400", "#FFBD00")) #choose multiple colors for gradient
#col_gradient <- colorRampPalette(c("#FF0054")) #choose multiple colors for gradient



#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
#test with 
#condition <- "Tau441_001percentDMSO"
condition_folders <- Sys.glob(file.path("Tau*")) #check that only required folders start with Tau*
for (condition in condition_folders) {
  print(condition)
  #condition <- "Tau421"
  setwd(paste("C:/Users/NITru/Downloads/CellSpecificData_ReExport/",condition,sep=""))
  
  
  fn <- Sys.glob(file.path("*_df_for_plot_position.csv"))
  for (file in fn) {
    #Delete file if it exists
    file.remove(file)
  }
  
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
  filenames
  
  fractions_all_cells_datalist <- list()
  changes_all_cells_datalist <- list()
  
  for (x in filenames){  
    #print(x)
    #x <-"Tau441_APP-GFPcell03_czi_5d92f2201950d_hrm.xlsx"
    #x <-"Tau421_APP-GFPcell11_czi_5d92415623f97_hrm.xlsx"
    df <- read_excel(x, sheet = "Position", skip = 1)
    df$TrackID <- as.factor(df$TrackID)
    df_subset <- df[df$TrackID %in% mobile_ID_table$Original.ID,]
    
    #df_subset <- df_subset[which(df_subset$TrackID == "1000037660"),] #to plot one trajectory 1000037660 Tau421_001percentDMSO, Tau421_APP-GFP_0-01percentDMSO_cell16_czi_5ecbdd0f6adf5_hrm.xlsx
    df_subset <- droplevels(df_subset)
    nlevels(df_subset$TrackID)
    cbp <- col_gradient(nlevels(df_subset$TrackID))
    
    # p <- ggplot(df_subset, aes(x = `Position X`, y = `Time`)) + 
    #   geom_point(aes(color = TrackID)) +
    #   #theme_classic() +
    #   ggtitle("Trajectories mobile fraction only") +
    #   xlab("Position X") + 
    #   ylab("Time") +
    #   scale_y_reverse() +
    #   theme_classic() +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_x_continuous() #breaks = seq(-5, 5, by = 0.5)
    #p
    #ggExtra::ggMarginal(p, type = "histogram")
    
    # ggplot(df_subset, aes(x = `Position X`)) + 
    #   geom_histogram() +
    #   #theme_classic() +
    #   ggtitle("Trajectories mobile fraction only") +
    #   xlab("Position X") + 
    #   ylab("Time") +
    #   #scale_y_reverse() +
    #   dark_theme_gray() +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_x_continuous()
    # 
    

 
    
    #######Kymograph analysis
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
          df_list[[i]]$delta_x[[j]] <- (df_list[[i]]$`Position X`[[j]]-df_list[[i]]$`Position X`[[j-1]])/(df_list[[i]]$Time[[j]]-df_list[[i]]$Time[[j-1]])
        }
      }  
    }
    df_subset <- do.call(rbind, df_list)
    
    df_subset$motion <- ifelse(df_subset$delta_x < -0.01, "retrograde", ifelse(df_subset$delta_x > 0.01, "anterograde", "stationary"))
    
    #######FILTER TRAJECTORIES BY LENGTH
    df_list_long_traj <- list()
    df_list_long_traj <- split(df_subset, df_subset$TrackID)
    df_for_plot <- data.frame()
    for(i in 1:length(df_list_long_traj)){
      if (nrow(df_list_long_traj[[i]])-1 > 0) { #####HERE PUT 200 for long ones
        if (dim(df_for_plot)[1] == 0) {
          df_for_plot <- df_list_long_traj[[i]]
        }
        else {
          print(nrow(df_list_long_traj[[i]]))
          df_for_plot <- rbind(df_for_plot,df_list_long_traj[[i]])
        }
      }
    }
    if (dim(df_for_plot)[1] != 0) {
      df_for_plot_dataframe <- as.data.frame(df_for_plot)
      df_for_plot_dataframe$condition <- condition
      #write.table(df_for_plot_dataframe, paste(x, "_df_for_plot_position.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
      
      df_for_plot_dataframe <- na.omit(df_for_plot_dataframe, cols="smoothed_x_displacement")
    
    p2 <- ggplot(df_for_plot_dataframe, aes(x = `Position X`, y = Time)) +
      geom_path(aes(group = TrackID)) + #aes(color = TrackID)
      #geom_count() +
      scale_color_manual(values = cols) + #aes(group = TrackID), 
      #dark_theme_gray() +
      geom_point(aes(group = TrackID,color=motion), size=3) +
      theme_classic() +
      ggtitle("No filtering") +
      xlab("Displacement smoothed by window = 5") +
      ylab("Time") +
      scale_y_reverse() +
      scale_x_continuous() #limits = c(-1,1),breaks = seq(-5, 5, by = 0.5)
    p2
    
    
    # animate static plot
    anim2 <- p2 + transition_reveal(Time) + ease_aes('cubic-in-out') # set reveal dimesion to year
    a_gif <- animate(anim2, width = 480, height = 300)
    #animate(anim, nframes = 100, end_pause = 20, renderer = gifski_renderer("gganim.gif")) # set parameters for gif and render to file
    
    #ggExtra::ggMarginal(p2, type = "histogram")
    # 
    # ggscatterhist(
    #   df_for_plot_dataframe, x = "Position X", y = "Time",
    #   color = "motion",
    #   palette = cols,
    #   margin.plot = "histogram",
    #   margin.params = list(fill = "motion", color = "black", size = 0.2))
    #   
    # 
    # 
    p3 <- ggplot(df_for_plot_dataframe, aes(x = `Position X`, y = `Position Y`)) +
      geom_path(aes(group = TrackID)) + #aes(color = TrackID)
      geom_point(aes(group = TrackID, color=motion), size=3) +
      scale_color_manual(values = cols) +
      ggtitle("Trajectories mobile fraction only") +
      xlab("Position X") +
      ylab("Position Y") +
      scale_y_reverse() +
      theme_classic() +
      scale_x_continuous() #breaks = seq(-5, 5, by = 0.5)
    p3
    #animate static plot
    anim3 <- p3 + transition_reveal(Time) + ease_aes('cubic-in-out') # set reveal dimesion to year
    b_gif <- animate(anim3, width = 480, height = 240)
    #animate(anim3, nframes = 100, end_pause = 20, renderer = gifski_renderer("gganim2.gif")) # set parameters for gif and render to file

    new_gif <- image_append(c(a_gif[1], b_gif[1]))
    for(i in 2:100){
      combined <- image_append(c(a_gif[i], b_gif[i]))
      new_gif <- c(new_gif, combined)
    }
    
    new_gif
    animation <- image_animate(new_gif, fps = 3)
    image_write(animation, "animation.gif")
    
    df_subset_stops <- df_for_plot_dataframe[which(df_for_plot_dataframe$motion=='stationary'),]
    ggplot(df_subset_stops, aes(x = `Position X`, y = `Position Y`)) +
      #geom_path(aes(group = TrackID)) + #aes(color = TrackID)
      geom_point(aes(color=motion), size=1) +
      scale_color_manual(values = cols) +
      ggtitle("Trajectories mobile fraction only") +
      xlab("Position X") +
      ylab("Position Y") +
      scale_y_reverse() +
      theme_classic() +
      scale_x_continuous()

    ggplot(df_subset_stops, aes(x = `Position X`, y = `Position Y`)) +
      #geom_path(aes(group = TrackID)) + #aes(color = TrackID)
      geom_point(aes(color=TrackID), size=1) +
      stat_density_2d() +
      #scale_color_manual(values = cols) +
      ggtitle("Trajectories mobile fraction only") +
      xlab("Position X") +
      ylab("Position Y") +
      scale_y_reverse() +
      theme_classic() +
      theme(legend.position = "none") +
      scale_x_continuous()

    ggscatterhist(
      df_subset_stops, x = "Position X", y = "Position Y",
      color = "TrackID",
      #palette = cols,
      margin.plot = "histogram",
      legend = "none",
      margin.params = list(fill = "TrackID", color = "black", size = 0.2))

    }
  }    
} 



