# Kymograph-like analysis of trajectories
# Author: Nataliya Trushina	
# Start date: 2020-01-01
# Last modified: 2024-01-14
# Description: Postprocess IMARIS collection output for axonal transport analysis
# Requirements: xls tables with Displacement X Reference Frame 
# and "mobility_table_all_conditions.csv" - the output of "01b_axonal_transport_cell_means.R"

# Note: If Excel files from different OneDrive accounts cannot be opened with read_excel(),
# open the file and save it again or use the Linux subsystem to automatically write them into new files.
# Convert xls files to xlsx:
# In one folder:
# unoconv -f xlsx *.xls
# When ran from an upper directory with all condition directories:
# for dir in *; do [ -d "$dir" ] && unoconv -f xlsx "$dir"/*.xls; done

# ---------------------------------------------------------------------------- #

# Load libraries

library(tidyverse) # Includes ggplot2, dplyr, tidyr, readr, purrr, tibble, and stringr
library(readxl)
library(Rmisc)
library(zoo)
library(ggpubr)
library(rlist)

# ---------------------------------------------------------------------------- #

# Settings

# Set working directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_path <- "path/to/single_cell_trajectories"
setwd(working_path)

cols <- c("retrograde" = "#ff9955", "stationary" = "#000000", "anterograde" = "#87decd")
col_gradient <- colorRampPalette(c("#390099", "#9E0059", "#FF0054", "#FF5400", "#FFBD00"))

# ---------------------------------------------------------------------------- #

# Functions

# Create an empty table with a specified number of rows and columns
create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}

# # Merge input tables
# merge_input_tables <- function(filenames) {
#   data_merge <- data.frame()
#   
#   # Iterate through all files
#   for (i in filenames){  
#     x <- read_excel(i, sheet = "Sheet1")
#     
#     x$`Track Displacement X Reference Frame` <- as.numeric(x$`Track Displacement X Reference Frame`)
#     
#     # Define mobility and directionality
#     x$Mobility <- "Immobile"
#     x$Mobility[x$`Track Displacement X Reference Frame` > 0.75] <- "Mobile"
#     x$Mobility[x$`Track Displacement X Reference Frame` < -0.75] <- "Mobile"
#     
#     x$Directionality <- "Stationary"
#     x$Directionality[x$`Track Displacement X Reference Frame` > 0.75] <- "Anterograde" 
#     x$Directionality[x$`Track Displacement X Reference Frame` < -0.75] <- "Retrograde" 
#     
#     data_merge <- rbind(data_merge, x)
#   }
#   return(data_merge)
# }

save_plot <- function(plot, file_name, width, height) {
  ggsave(paste0(file_name, ".svg"), plot = plot, width = width, height = height)
  ggsave(paste0(file_name, ".png"), plot = plot, width = width, height = height)
}

# ---------------------------------------------------------------------------- #

# Create full annotation table from condition export tables
# filenames <- Sys.glob(file.path("*xlsx"))
# full_ID_table <- merge_input_tables(filenames)

full_ID_table <- read.csv("mobility_table_all_conditions.csv", sep = ";", dec = ",")

# Filter mobile vesicles - if using another input for mobility calculation, is not required for "mobility_table_all_conditions.csv"
mobile_ID_table <- full_ID_table[which(full_ID_table$Mobility=='Mobile'), ]
mobile_IDs <- as.factor(mobile_ID_table$Original.ID)

# Select only folders
all_files <- list.files(rec = F)
condition_folders <- all_files[file.info(all_files)$isdir]

# Iterate through all condition folders
for (condition in condition_folders) {
  setwd(working_path) 
  setwd(paste0(working_path, "/", condition))
  print(getwd())

  # Get filenames with a specific pattern
  filenames <- Sys.glob(file.path("*_hrm.xlsx"))
  
  # Initialize data lists for fractions and changes
  fractions_all_cells_datalist <- list()
  changes_all_cells_datalist <- list()
  
  # Iterate through all filenames
  for (x in filenames){  
    # Read and subset data
    df <- read_excel(x, sheet = "Displacement Reference Frame", skip = 1)
    df$TrackID <- as.factor(df$TrackID)
    df_subset <- df[df$TrackID %in% mobile_IDs,]
    
    # Plot one trajectory
    # df_subset <- df_subset[which(df_subset$TrackID == "1000120338"),]
    
    # Drop unused levels
    df_subset <- droplevels(df_subset)
    cbp <- col_gradient(nlevels(df_subset$TrackID))
    
    # --------------------------------------------------
    # Check original data by plotting input:
    
    # ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = `Displacement Y Reference Frame`)) +
    #   geom_path(aes(color = TrackID)) +
    #   theme_classic() +
    #   ggtitle("Trajectories mobile fraction only") +
    #   xlab("Displacement X Reference Frame") +
    #   ylab("Displacement Y Reference Frame") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_x_continuous(breaks = seq(-5, 5, by = 0.5))
    
    plot_all_IDcolored <- ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = Time)) +
      geom_path(aes(color = TrackID)) +
      scale_color_manual(values = cbp) +
      geom_path(aes(x = `Displacement X Reference Frame`, y = Time, color = TrackID), linetype = "dashed") +
      theme_classic() +
      theme(legend.position = "none") +
      ggtitle("Trajectories, mobile fraction only") +
      xlab("Displacement X Reference Frame") +
      ylab("Time") +
      scale_y_reverse(breaks = seq(300, 0, by = -20))  +
      scale_x_continuous(breaks = seq(-100, 100, by = 1))
    
    # ggsave(plot_all_IDcolored, filename = paste(x, "_trajectories.png", sep = ""), width = 8, height = 10)
    save_plot(plot = plot_all_IDcolored, file_name = paste0(x, "_trajectories"), 8, 10)
    
    # ggplot(df_subset, aes(x = `Displacement X Reference Frame`, y = Time)) +
    #   geom_path(aes(color = Time), size=1) +
    #   scale_color_gradientn(colours = rainbow(7),limits=c(0,300), breaks=c(0,300,150)) +
    #   guides(colour = guide_colourbar(reverse=T)) +
    #   xlab("Displacement X Reference Frame") +
    #   ylab("Time") +
    #   #geom_smooth(se=FALSE, linetype="dashed", size=0.1, aes(color = TrackID)) +
    #   scale_y_reverse() +
    #   scale_x_continuous(breaks = seq(-15, 15, by = 0.5))
    
    
    # --------------------------------------------------
    # Kymograph-like analysis
    df_subset <- df_subset[order(df_subset$TrackID),]
    
    # Initialize required columns
    df_subset$delta_x <- NA
    df_list <- list()
    df_list <- split(df_subset, df_subset$TrackID)
    
    # Calculate delta_x values
    for(i in 1:length(df_list)){
      if (nrow(df_list[[i]]) > 1) {
        for (j in 2:nrow(df_list[[i]])) {
          # Calculate delta_x as delta_disp/delta_t, allowing for 2 (or more) empty frames
          df_list[[i]]$delta_x[[j]] <- 
            (df_list[[i]]$`Displacement X Reference Frame`[[j]] -
               df_list[[i]]$`Displacement X Reference Frame`[[j-1]])/
            (df_list[[i]]$Time[[j]]-df_list[[i]]$Time[[j-1]])
        }
      }  
    }
    
    # Combine data frames back into one
    df_subset <- do.call(rbind, df_list)
    
    # Define motion type based on delta_x values
    # The lower border of mean minimum is 0.8 um per second if we divide by 5 (5 frames per second, 200ms each frame) then it was close to 0.02 which we chose as cut-off
    df_subset$motion <- ifelse(df_subset$delta_x < -0.02, "retrograde", ifelse(df_subset$delta_x > 0.02, "anterograde", "stationary"))
    
    plot_all_motioncolored <- ggplot(na.omit(df_subset), aes(x = `Displacement X Reference Frame`, y = Time)) +
      geom_path(aes(group = TrackID)) +
      geom_point(data = na.omit(df_subset), aes(color=motion), size=1) +
      scale_color_manual(values = cols) +
      ggtitle("Trajectories, mobile fraction only") +
      xlab("Displacement X Reference Frame") +
      ylab("Time") +
      scale_y_reverse(breaks = seq(300, 0, by = -20)) +
      scale_x_continuous(breaks = seq(-100, 100, by = 1)) +
      theme_classic() +
      theme(
        legend.position = "bottom"
        )
    
    save_plot(plot = plot_all_motioncolored, file_name = paste0(x, "_motion"), 8, 10)
     
    df_subset_naomit <- na.omit(df_subset, cols="delta_x")
    df_subset_naomit$motion <- as.factor(df_subset_naomit$motion)
    
    # --------------------------------------------------
    # # Filtering: if previous and next are same, change the middle one for next
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
    
    
    # --------------------------------------------------
    # Count state fractions
    df_list_3 <- list()
    df_list_3 <- split(df_subset_naomit, df_subset_naomit$TrackID)
    
    fractions_datalist <- list()
    fractions_df <- create_empty_table(3,1)
    changes_datalist <- list()
    state_changes_df <- create_empty_table(11,1)
    
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
    
    fractions_all_cells_datalist[x] <- as.data.frame(colMeans(assembled_state_fractions))
    changes_all_cells_datalist[x] <- as.data.frame(colMeans(assembled_state_changes))
  }
  
  assembled_state_fractions_all_cells <- as.data.frame(do.call(rbind, fractions_all_cells_datalist))
  assembled_state_fractions_all_cells <- setNames(assembled_state_fractions_all_cells,c("Anterograde","Retrograde","Stationary"))
  assembled_state_fractions_all_cells$condition <- condition
  write.table(assembled_state_fractions_all_cells, paste("../", condition, "_fractions_final.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
  
  assembled_state_changes_all_cells <- as.data.frame(do.call(rbind, changes_all_cells_datalist))
  assembled_state_changes_all_cells <- setNames(assembled_state_changes_all_cells,c("RS","AS","RA","AR","SR","SA","all_to_stop","stop_to_all","retr_to_stop_ant","ant_to_stop_retr","all_changes"))
  assembled_state_changes_all_cells$condition <- condition
  write.table(assembled_state_changes_all_cells, paste("../", condition, "_changes_final.csv", sep=""), sep = ";",dec = '.', row.names = FALSE, col.names = TRUE)
}
