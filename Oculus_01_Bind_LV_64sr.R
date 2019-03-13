

library(tidyverse); library(rio); library(signal)

rm(list=ls())

{
  # Update directory for new runs
  curr_directory <- "./Data/participant06"
  
  sample_rate <- 2048
  
  pre_baseline_time <- 0   # how many seconds was the baseline period?
  post_baseline_time <- 0
  
  # Select columns/channels w/ data in LabView output 
  channels <- c(1:6, 8, 9, 15)  
  # 1:6 Force Plate
  # 08 RMG
  # 09 LMG
  # 15 flags from Oculus
  
  curr_direction <- "z"
}

#-------------------------------
{
  
  pre_blt <- pre_baseline_time * sample_rate 
  post_blt <- post_baseline_time * sample_rate
  
  # Create directory for cleaned data
  dir.create(paste0(curr_directory, "/cleaned_LV"))
  
  # List phys files
  files_lv <- list.files(paste0(curr_directory, "/labview"))

  
  # List stim files
  files_stim <- list.files("./Scaled") %>% 
    str_to_lower()
  
  # Import key to decode current stim type
  stim_key <- import("Stim_Key.csv") 
}

# Step through each stim type
for(i in seq_along(stim_key$Stim)){
  
  # Create empty variable where trial data will go
  assign(stim_key$Stim[i], NULL)
  
  # Grab stim file that matches i
  vis2load <- files_stim[which(str_detect(files_stim, str_split(stim_key$Stim[i], "_")[[1]][1]) &
                                 str_detect(files_stim, str_split(stim_key$Stim[i], "_")[[1]][2]) == TRUE)]
  
  # Import stim
  vis_stim <- import(paste0("./Scaled/", vis2load))
  
  
  # Add column names to stim
  colnames(vis_stim) <- c("x", "y", "z", "yaw", "pitch", "roll", "flags")
  
  # Only need 2 columns
  vis_stim <- vis_stim[, c(curr_direction, "flags")]
  
  # Save data
  assign(vis2load, vis_stim)
  
}




for(i in seq_along(files_lv)){
  
  # Load current data set
  data_lv <- import(paste0(curr_directory, "/labview/", files_lv[i]))[channels]
  
  # Which stim types were in this run?
  sub_key <- stim_key$Stim_Code[which(stim_key$Stim_Code %in% unique(data_lv$V15) == TRUE)]
  
  # Step through each stim type in run
  for(j in seq_along(sub_key)){

    # Decode stim type using look up table
    current_stim <- stim_key$Stim[which(stim_key$Stim_Code == sub_key[j])]
    
    # Find approx. location in sequence where stim delivered
    stim_index <- min(which(data_lv$V15 == sub_key[j]))
    
    # Because 90 Hz stim sampled at 2048 Hz, flags repeat 22 or 23 times unpredictably
    #  making it difficult to pinpoint real stim onset/offset
    #  Solution: index beginning and end of run lengths
    markers_rle <- rle(data_lv$V15)
    flag_ends <- cumsum(markers_rle$lengths)
    flag_starts <- c(1, lag(flag_ends)[-1] + 1)
    
    # Create look up table for runs
    markers_rle2 <- data.frame(lengths = markers_rle$lengths, 
                               values = markers_rle$values,
                               start = flag_starts,
                               end = flag_ends)
    
    # Find approximate stim start and end times (flag 13 is a trigger)
    trial_markers <- which(data_lv$V15 == 13)
    
    # Approx onset of current stim
    stim_start0 <- trial_markers[max(which(trial_markers < stim_index))]
    
    # Look up # of trigger repetitions 
    start_offset <- markers_rle2$lengths[which(markers_rle2$end == stim_start0)]
    
    # Account for repeitition length
    stim_start <- stim_start0 - (start_offset - 1)
    
    # Same process for approx offset of current stim
    stim_end0 <- trial_markers[min(which(trial_markers > stim_index))]
    end_offset <- markers_rle2$lengths[which(markers_rle2$start == stim_end0)]
    stim_end <- stim_end0 + (end_offset - 1)
    
    # subset data from start to end of current stim
    data_phys <- data_lv[stim_start:stim_end,]
    data_phys$trial_timeLV <- seq(from = 0, to = 90, length.out = nrow(data_phys))
    
    
    # Find appropriate stim file for current stim
    vis2bind <- files_stim[which(str_detect(files_stim, str_split(current_stim, "_")[[1]][1]) &
                                   str_detect(files_stim, str_split(current_stim, "_")[[1]][2]) == TRUE)]
    
    
    tmp_vis <- get(vis2bind)

    
    # Sanity check, length should be 3
    print(length(sort(unique(data_phys$V15)) == sort(unique(tmp_vis$flags))))
    
    # Rename 
    colnames(tmp_vis)[colnames(tmp_vis)=="z"] <- paste0("stim_", curr_direction)
    
    tmp_vis <- tmp_vis[-c(1:min(which(tmp_vis$flags == 13) -1 )),]
    tmp_vis$trial_timeOc <- seq(from = 0, to = 90, length.out = nrow(tmp_vis))
    tmp_vis <- tmp_vis[,-2]
    
    # up sample the visual stimulus to match length of phys recording
    upSampled <- as.data.frame(
      apply(tmp_vis, 2, function(x) 
        resample(x, nrow(data_phys), nrow(tmp_vis))
      )
    )
    
    # Bind phys and stim data
    bound2048 <- cbind(data_phys, upSampled)
    
    # Concatenate with previous trials of stim type
    assign(paste(current_stim), rbind(get(current_stim), bound2048))
    
  }
  
}

# Save output
for(i in seq_along(stim_key$Stim)){
  
  write.table(get(stim_key$Stim[i]),
              paste0(curr_directory, "/cleaned_LV/", stim_key$Stim[i], ".txt"),
              sep = "\t",
              row.names = FALSE)
  
}

