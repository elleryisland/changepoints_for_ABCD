

# STEPS: 
# 0. libraries, directories, common data 
# 1. get lowest motion subject list from Kelly's folder (see code in changepoints_abcd)
#### subject for-loop start ####
# 2. for each subject, get length of the runs by loading in run specific tsv files
# 3. load in combined run data (the time)
# 4. separate combined run data into individual runs by using the length of each run from step 2
#### run for-loop start ####
# 5. set up for change points (merge with parcelation data, prewhiten)
# 6. run change point analyses on each run for each individual for each run
# 7. store subject id, run number, and change points in a good way 
#### run for-loop end ####
#### subject for-loop end ####

# LIBRARIES 
rm(list = ls()) # Clear env

if(!require(tidyverse)){
  install.packages("tidyverse")}

if(!require(cifti)){
  install.packages("cifti")
}

if(!require(devtools)){
  install.packages("devtools")}

if(!require(mvtnorm)){
  install.packages("mvtnorm")}

if(!require(mixedcpt)){
  devtools::install_github("mfiecas/mixedcpt", auth_token = "")}

# SET UP DIRECTORIES
lowest_motion_subs_dir <- '/spaces/ngdr/workspaces/cullenkr/abcd_ws/dcc/Glasser_timeseries_LowestRun-AllSubs_NoInt/' # for step 1
data_dir <- '/spaces/ngdr/ref-data/abcd/nda-3165-2020-09/derivatives/abcd-hcp-pipeline/'
out_dir <- '/home/cullenkr/shared/islan007/default_mode_11_12_24/'

# COMMON DATA (shared across subjects)
glasser_parcels <- read_csv("/home/cullenkr/islan007/GlasserParcels_Cole_Laird_Networks.csv") # import parcelation data

# 1. LOWEST MOTION SUBJECT LIST
sub_list <- dir(lowest_motion_subs_dir)
for (i in 1:length(sub_list)) {
  sub_list[i] <- str_remove_all(sub_list[i], "_ses-baselineYear1Arm1_task-rest_bold_atlas-HCP2016FreeSurferSubcortical_desc-filtered_timeseries_run-[:digit:].txt")
}

# SUBSET THE SUBJECT LIST USING SLURM_TASK_ID
args = commandArgs(trailingOnly=TRUE) # takes in the input from the shell script (SLURM_TASK_ID in this case)
print(args[1]) # print out the job number for troubleshooting
job_number <- as.numeric(args[1])

if (job_number == 807){
  sub_list <- sub_list[(4*job_number - 2):(4*job_number)] # SKIPPING SUBJECT NDARINVL24LAKCM due to poor data quality (index 3225)
} else{sub_list <- sub_list[(4*job_number - 3):(4*job_number)]} # 4 subjects per job (5016/4 = 1254 jobs --> + 1 = 1255 jobs)


# print network for troubleshooting
print("default mode")

##### START SUBJECT FOR LOOP #####

cpt_df <- tibble() 

for (sub_index in 1:length(sub_list)) {
  
  sub <- sub_list[sub_index]
  
  # Print subject id for troubleshooting
  cat('Subject:', sub, '\n \n')
  
  # 2. RUN LENGTHS
  run_length_vec0 <- c() # initialize storage vector
  for (run_num in 1:4) {
    if (sub != "sub-NDARINVE5K3E4XA") {
      file_path <- paste0(data_dir, sub, '/ses-baselineYear1Arm1/func/', sub, '_ses-baselineYear1Arm1_task-rest_run-', run_num, '_desc-filtered_motion.tsv')
    } else {file_path <- paste0(data_dir, sub, '/ses-baselineYear1Arm1/func/', sub, '_ses-baselineYear1Arm1_task-rest_run-', run_num, '_desc-filteredincludingFD_motion.tsv')}
    if (file.exists(file_path) == T){ # only save dimension if the file exists (aka the run exists)
      run_tsv <- read_tsv(file_path, show_col_types = FALSE)
      print(dim(run_tsv)[1])
      run_length_vec0[run_num] <- dim(run_tsv)[1]} # capture the number of rows of this tsv which = the length of the run
  }
  
  print(run_length_vec0)
  for (i in 1:length(run_length_vec0)){ # remove runs of 0 length
    if (run_length_vec0[i] == 0){run_length_vec <- run_length_vec0[-i]
    } else{run_length_vec <- run_length_vec0}
  } 
  
  print(run_length_vec0)
  print(run_length_vec)
  # 3. TIMESERIES DATA
  file_path_ts <- paste0(data_dir, sub, '/ses-baselineYear1Arm1/func/', sub, "_ses-baselineYear1Arm1_task-rest_bold_atlas-HCP2016FreeSurferSubcortical_desc-filtered_timeseries.ptseries.nii")
  ts <- cifti_data(file_path_ts)[,,1]
  
  print(dim(ts))
  # 4. SPLIT DATA INTO SEPERATE RUNS
  run_length_index <- cumsum(run_length_vec) # convert the individual lengths to index values for the whole series
  run_data_list <- list() # initialize list to store the run data frames
  print(run_length_index)
  if (length(run_length_index) > 3){ # account for the situation where time series has 3 runs but it's reported as 4 in tsv (e.g., ts = 1149  379, tsv cum sum: 383  766 1149 1156)
    if (dim(ts)[1] == run_length_index[3]){run_length_index <- run_length_index[0:3]}}
  
  if (length(run_length_index) > 2){ # account for the situation where time series has 2 runs but it's reported as 4 in tsv 
    if (dim(ts)[1] == run_length_index[2]){run_length_index <- run_length_index[0:2]}}
  
  for (i in 1:length(run_length_index)){
    if (i == 1){
      print(i)
      run_data_list[[i]] <- ts[1:run_length_index[i],]}
    else {
      if (dim(ts)[1] < run_length_index[i]){run_data_list[[i]] <- ts[(run_length_index[i-1] + 1):dim(ts)[1],]} # account for the case when the time series is shorter than reported
      else{
        run_data_list[[i]] <- ts[(run_length_index[i-1] + 1):run_length_index[i],]
      }
    }
  }
  if (sub == "sub-NDARINVE8PCBGJ0") {run_data_list <- run_data_list[1:3]} # remove the zero variance series (run 4 for this subject) -- field of view issue
  if (sub == "sub-NDARINVL24LAKCM") {run_data_list <- run_data_list[2:4]} # remove the zero variance series (run 1 for this subject)  -- field of view issue
  if (sub == "sub-NDARINVZZ05KY5J") {run_data_list <- run_data_list[-3]} # remove the zero variance series (run 3 for this subject)  -- field of view issue
  if (sub == "sub-NDARINV8UF58XPK") {run_data_list <- run_data_list[-2]} # remove the zero variance series (run 2 for this subject)  -- field of view issue
  if (sub == "sub-NDARINVAWBNZMY0") {run_data_list <- run_data_list[-2]} # remove the zero variance series (run 2 for this subject)  -- field of view issue
  
  ##### START RUN FOR LOOP ######
  
  for (i in 1: length(run_data_list)){
    ts <- run_data_list[[i]]
    ts <- ts[-1:-10, ] # remove first 10 time points (inaccurate signal)
    
    # 5. SET UP FOR CHANGEPOINTS
    combined <- cbind(t(as.data.frame(ts)), glasser_parcels) # merge parcelation with time series (time is columns)
    
    # rois (default mode)
    acc_mpfc <- c('33pr', 'p24pr', 'a24pr', 'p24', 'a24', 'p32pr', 'a32pr', 'd32', 'p32', 's32', '8BM', '9m', '10v', '10r', '25')
    posterior_cingulate <- c('DVT', 'ProS', 'POS1', 'POS2', 'RSC', 'v23ab', 'd23ab', '31pv', '31pd', '31a', '23d', '23c', 'PCV', '7m')
    angular_gyrus <- c("PGi", "PGs")
    middle_temporal <- c("TE1a", "TE1m", "TE1p")
    
    acc_mpfc_rois_L <- c(paste('L_', c(acc_mpfc), '_ROI', sep=''))
    acc_mpfc_rois_R <- c(paste('R_', c(acc_mpfc), '_ROI', sep=''))
    posterior_cingulate_rois_L <- c(paste('L_', c(posterior_cingulate), '_ROI', sep=''))
    posterior_cingulate_rois_R <- c(paste('R_', c(posterior_cingulate), '_ROI', sep=''))
    angular_gyrus_L <- c(paste('L_', c(angular_gyrus), '_ROI', sep=''))
    angular_gyrus_R <- c(paste('R_', c(angular_gyrus), '_ROI', sep=''))
    middle_temporal_L <- c(paste('L_', c(middle_temporal), '_ROI', sep=''))
    middle_temporal_R <- c(paste('R_', c(middle_temporal), '_ROI', sep=''))
    
    # make group variable
    combined <- combined %>%
      filter(orig_name %in% c(acc_mpfc_rois_L, acc_mpfc_rois_R, posterior_cingulate_rois_L, posterior_cingulate_rois_R, angular_gyrus_L, angular_gyrus_R, middle_temporal_L, middle_temporal_R)) %>% 
      mutate(group = case_when(orig_name %in% acc_mpfc_rois_L ~ "1", # create group id variable
                               orig_name %in% acc_mpfc_rois_R ~ "2", 
                               orig_name %in% posterior_cingulate_rois_L ~ "3",
                               orig_name %in% posterior_cingulate_rois_R ~ "4",
                               orig_name %in% angular_gyrus_L ~ "5",
                               orig_name %in% angular_gyrus_R ~ "6",
                               orig_name %in% middle_temporal_L ~ "7",
                               orig_name %in% middle_temporal_R ~ "8"),
             group = as.factor(group)) %>% 
      arrange(group)
    
    group <- combined %>%
      select(group)
    
    
    # subset data 
    for_ar <- combined %>%
      arrange(group) %>%
      filter(orig_name %in% c(acc_mpfc_rois_L, acc_mpfc_rois_R, posterior_cingulate_rois_L, posterior_cingulate_rois_R, angular_gyrus_L, angular_gyrus_R, middle_temporal_L, middle_temporal_R)) %>%
      select(-c("orig_name", "LairdNetwork", "ColeNetNum", "ColeNetwork", "group")) 
    
    length_ts <- dim(for_ar)[2]
    
    if(length_ts >= 300){ # if dimensions make it possible, carry out the prewhitening and changepoint analysis
      # prewhiten
      n_rois <- dim(for_ar)[1]
      data_pw <- array(NA, dim = c(n_rois, length_ts - 8))
      
      for (p in 1:n_rois) {
        data_pw[p,] <- (t(for_ar[p,]) %>%
                          scale(center = TRUE, scale=FALSE) %>%
                          ar(aic=FALSE, order.max=8))$resid[9:length_ts] %>%
          scale()
      }
      X <- t(data_pw) # make sure that t1 = row1, t2 = row2, ... 
      
      # parameters for changepoints
      P <- dim(X)[2]
      LENGTH <- dim(X)[1] # time = rows
      minseglen <- 2*P # minimum segment length
      cluster.size <- nlevels(group$group)  # number of groups
      pen <- (choose(cluster.size + 1, 2) + cluster.size)*log(LENGTH) # penalty 
      
      # changepoints
      cpts <- PELT.lmec(data = X,
                        group.id = group,
                        pen = pen,
                        minseglen = minseglen,
                        equal.variance=TRUE)
      
      # storage
      new_row <- data.frame(t(c(sub, i, cpts))) # this is an inelegant way of doing this (bind_rows requires dfs as inputs) -- improve later
      cpt_df <- bind_rows(cpt_df, new_row) # using bind_rows instead of rbind because it doesn't care about the number of cols
      print(paste("finished run", i, "for", sub))
    } else{print(paste("run", i, "for", sub, "is too short"))}
  }
}

write.csv(cpt_df, file = paste0(out_dir, "cpt_default_mode_df_", job_number, ".csv")) # save! 

print(paste("job complete"))
