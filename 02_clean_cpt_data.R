library(dplyr) # data manipulation
library(stringr) # dealing with strings
library(readr) # for read_csv
library(tidyr) # data manipulation
library(broom) # for the tidy() function

# ======================================= read in subject data and clean =================================================
subject_data <- read_csv('abcd_subject_data_2_21_2024.csv')

subject_data_clean <- subject_data %>%
  mutate(src_subject_id = str_remove(string = src_subject_id, pattern = "_")) %>%
  mutate(across(c(White_race, Black_race, Asian_race, Other_race, Missing_race, Indigenous_race, demo_ethn_v2, demo_gender_id_v2_comb, pubertal_status,
                  SI_ever_p_y, SA_ever_p_y, NSSI_ever_p_y, AnyDD_ever_p_y, imgincl_rsfmri_include, site_id_l, rel_family_id, demo_comb_income_v2_bl, demo_prnt_ed_v2_bl), ~as.factor(.x))) %>%
  mutate(AnyDD_ever_p_y_binary = factor(case_when(AnyDD_ever_p_y == "never" ~ "never",
                                                  AnyDD_ever_p_y == "past" ~ "ever",
                                                  AnyDD_ever_p_y == "present" ~ "ever",
                                                  AnyDD_ever_p_y == "partial remission" ~ "ever")),
         SA_ever_p_y_binary = factor(case_when( SA_ever_p_y == "never" ~ "never",
                                                SA_ever_p_y == "past" ~ "ever",
                                                SA_ever_p_y == "present" ~ "ever")),
         SI_ever_p_y_binary = factor(case_when(SI_ever_p_y == "never" ~ "never",
                                               SI_ever_p_y == "past" ~ "ever",
                                               SI_ever_p_y == "present" ~ "ever")),
         NSSI_ever_p_y_binary = factor(case_when(NSSI_ever_p_y == "never" ~ "never",
                                                 NSSI_ever_p_y == "past" ~ "ever",
                                                 NSSI_ever_p_y == "present" ~ "ever"))) %>%
  mutate(Black_race = if_else(is.na(Black_race) == T, "0", Black_race),
         White_race = if_else(is.na(White_race) == T, "0", White_race),
         Asian_race = if_else(is.na(Asian_race) == T, "0", Asian_race),
         Other_race = if_else(is.na(Other_race) == T, "0", Other_race),
         Missing_race = if_else(is.na(Missing_race) == T, "0", Missing_race),
         Indigenous_race = if_else(is.na(Indigenous_race) == T, "0", Indigenous_race),
         demo_ethn_v2 = if_else(is.na(demo_ethn_v2) == T, "0", demo_ethn_v2),
         Missing_race = if_else(is.na(Black_race) == T, "1", Missing_race),
         Missing_race = if_else(is.na(White_race) == T, "1", Missing_race),
         Missing_race = if_else(is.na(Asian_race) == T, "1", Missing_race),
         Missing_race = if_else(is.na(Other_race) == T, "1", Missing_race),
         Missing_race = if_else(is.na(Indigenous_race) == T, "1", Missing_race),
         Missing_race = if_else(is.na(demo_ethn_v2) == T, "1", Missing_race)) 

# ==================================================== cbcl and neurocog data ======================================================================

cbcl <- read_csv("cbcl_data") %>% mutate(src_subject_id = str_remove(src_subject_id, "_"))

neurocog <- read_csv("abcd_neurocognition_pcs_release_4_2021.csv") %>% 
  filter(eventname == "baseline_year_1_arm_1") %>% 
  select(-eventname) %>%
  mutate(src_subject_id = str_remove(src_subject_id, "_"))

subject_data_clean <- left_join(left_join(subject_data_clean, cbcl, by = "src_subject_id"), neurocog, by = "src_subject_id")

# ===================== read in all cpt task csv files, merge, and write combined csv ============================================
# only do this when you have new cpt csvs!!!
# default_mode
# setwd("/home/cullenkr/shared/islan007/default_mode_11_12_24")
# tbl_default_mode <- list.files(path = '/home/cullenkr/shared/islan007/default_mode_11_12_24', pattern = "\\.csv$") %>%
#   map_df(~read_csv(.))
# write_csv(tbl_default_mode, '/home/cullenkr/shared/islan007/cpt_default_mode_11.12.24')

 
# # reward
# setwd("/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/reward_CORRECT_GROUPS")
# tbl_reward <- list.files(path = '/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/reward_CORRECT_GROUPS', pattern = "\\.csv$") %>%
#   map_df(~read_csv(.))
# write_csv(tbl_reward, '/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/cpt_reward_7.23.24')
# 
# # fronto limbic
# setwd("/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/fronto_limbic_CORRECT_GROUPS")
# tbl_fronto_limbic <- list.files(path = '/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/fronto_limbic_CORRECT_GROUPS', pattern = "\\.csv$") %>%
#   map_df(~read_csv(.))
# write_csv(tbl_fronto_limbic, '/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/cpt_fronto_limbic_7.23.24')
# 
# # dorsal_cortico_striatal
# setwd("/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/dorsal_cortico_striatal_CORRECT_GROUPS")
# tbl_dorsal_cortico_striatal <- list.files(path = '/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/dorsal_cortico_striatal_CORRECT_GROUPS', pattern = "\\.csv$") %>%
#   map_df(~read_csv(.))
# write_csv(tbl_dorsal_cortico_striatal, '/spaces/ngdr/workspaces/cullenkr/abcd_ws/islan007/changepoints/cpt_all_runs_out/cpt_dorsal_cortico_striatal_7.23.24')

# ========================================= read in the combined csvs ================================================

tbl_default_mode <- read_csv('cpt_default_mode_11.12.24')
tbl_reward <- read_csv('cpt_reward_7.23.24')
tbl_fronto_limbic <- read_csv('cpt_fronto_limbic_7.23.24')
tbl_dorsal_cortico_striatal <- read_csv('cpt_dorsal_cortico_striatal_7.23.24')

# ======================================= clean cpt data =================================================

clean_cpt_df <- function(network, cpt_tbl){
  name <- paste0('n_cpts_', network)
  if (network == "r"){
    clean_data <- cpt_tbl %>%
      group_by(X1, .drop = F) %>%
      add_count(factor(X2)) %>%
      ungroup() %>%
      rename(src_subject_id = X1) %>%
      mutate(src_subject_id = str_remove(string = src_subject_id, pattern = "sub-")) %>%
      mutate(count_cpt1 = if_else(is.na(X3) == F, 1, 0)) %>%
      mutate(count_cpt2 = if_else(is.na(X4) == F, 1, 0)) %>%
      mutate(count_cpt3 = if_else(is.na(X5) == F, 1, 0)) %>%
      mutate(total_count = count_cpt3 + count_cpt2 + count_cpt1)  %>%
      group_by(src_subject_id) %>%
      summarise(n_runs = sum(n), n_cpts = sum(total_count)) %>%
      rename(!!name := n_cpts)
  } else if (network == "df"){
    clean_data <- cpt_tbl %>%
      group_by(X1, .drop = F) %>%
      add_count(factor(X2)) %>%
      ungroup() %>%
      rename(src_subject_id = X1) %>%
      mutate(src_subject_id = str_remove(string = src_subject_id, pattern = "sub-")) %>%
      mutate(count_cpt1 = if_else(is.na(X3) == F, 1, 0)) %>%
      mutate(total_count = count_cpt1)  %>%
      group_by(src_subject_id) %>%
      summarise(n_runs = sum(n), n_cpts = sum(total_count)) %>%
      rename(!!name := n_cpts)
  } else(
    clean_data <- cpt_tbl %>%
      group_by(X1, .drop = F) %>%
      add_count(factor(X2)) %>%
      ungroup() %>%
      rename(src_subject_id = X1) %>%
      mutate(src_subject_id = str_remove(string = src_subject_id, pattern = "sub-")) %>%
      mutate(count_cpt1 = if_else(is.na(X3) == F, 1, 0)) %>%
      mutate(count_cpt2 = if_else(is.na(X4) == F, 1, 0)) %>%
      mutate(total_count = count_cpt2 + count_cpt1)  %>%
      group_by(src_subject_id) %>%
      summarise(n_runs = sum(n), n_cpts = sum(total_count)) %>%
      rename(!!name := n_cpts)
  )
  
  return(clean_data)
}

clean_default_mode <- clean_cpt_df('df', tbl_default_mode)
clean_reward <- clean_cpt_df('r', tbl_reward)
clean_fronto_limbic <- clean_cpt_df('fl', tbl_fronto_limbic)
clean_dcs <- clean_cpt_df('dcs', tbl_dorsal_cortico_striatal)


# ============================================= read and clean motion data ==========================================================

#motion <- read_csv("/spaces/ngdr/workspaces/cullenkr/abcd_ws/dcc/subject_inclusion_processing_info/motion-data_AllSubs-AllRuns-Baseline_thresh-0.3_desc-filteredincludingFD_motion.csv")

motion2 <- read_csv("ellery_abcd_fd-filt.csv") %>% # note: motion 2 is very unclean -- below is a messy solution to cleaning it
  select(1:6) %>% 
  mutate(across(3:6, ~strsplit(unlist(.x), ","))) # turn each entry from a list into the list's entry (a character) then split the character into a list

run1_MOTION <- motion2 %>% 
  unnest(c(fd_run1), keep_empty = T) %>% # expand the list into rows (each element of fd_run1 becomes a row)
  mutate(across(fd_run1, ~str_remove(.x, "\\[")), # clean the entries
         across(fd_run1, ~str_remove(.x, "\\]")),
         across(fd_run1, ~as.numeric(.x))) 

run1_fd <- motion2 %>% 
  unnest(c(fd_run1), keep_empty = T) %>% # expand the list into rows (each element becomes a row)
  mutate(across(fd_run1, ~str_remove(.x, "\\[")), # clean the entries
         across(fd_run1, ~str_remove(.x, "\\]")),
         across(fd_run1, ~as.numeric(.x))) %>%
  group_by(subj) %>%
  summarize(n_above_thresh_run1 = sum(fd_run1 < 0.3)) # find the number of frames below 0.3 fd

run2_fd <- motion2 %>% 
  unnest(c(fd_run2), keep_empty = T) %>% # repeat process for other runs
  mutate(across(fd_run2, ~str_remove(.x, "\\[")),
         across(fd_run2, ~str_remove(.x, "\\]")),
         across(fd_run2, ~as.numeric(.x))) %>%
  group_by(subj) %>%
  summarize(n_above_thresh_run2 = sum(fd_run2 < 0.3)) # NOTE THIS IS NAMED WRONG --- REALLY BELOW THRESH

run3_fd <- motion2 %>% 
  unnest(c(fd_run3), keep_empty = T) %>%
  mutate(across(fd_run3, ~str_remove(.x, "\\[")),
         across(fd_run3, ~str_remove(.x, "\\]")),
         across(fd_run3, ~as.numeric(.x))) %>%
  group_by(subj) %>%
  summarize(n_above_thresh_run3 = sum(fd_run3 < 0.3))

run4_fd <- motion2 %>% 
  unnest(c(fd_run4), keep_empty = T) %>%
  mutate(across(fd_run4, ~str_remove(.x, "\\[")),
         across(fd_run4, ~str_remove(.x, "\\]")),
         across(fd_run4, ~as.numeric(.x))) %>%
  group_by(subj) %>%
  summarize(n_above_thresh_run4 = sum(fd_run4 < 0.3))

part1 <- left_join(run1_fd, run2_fd, by = "subj") # join together
part2 <- left_join(run3_fd, run4_fd, by = "subj")
new_motion_summarized <- left_join(part1, part2, by = "subj") %>%
  mutate(src_subject_id = str_remove(string = subj, pattern = "sub-")) %>%
  select(-subj) %>%
  pivot_longer(cols = 1:4, names_to = "run", values_to = "n_below_thresh") %>%
  mutate(run = str_remove(string = run, pattern = "n_above_thresh_run"),
         run = as.numeric(run))

# capturing only the runs that are in the cpt data
subject_run_key <- tbl_default_mode %>% # create a key that has which runs were included in cpt analysis by subject
  rename(src_subject_id = X1,
         run = X2) %>%
  select(src_subject_id, run) %>%
  mutate(src_subject_id = str_remove(src_subject_id, "sub-"))

motion_correct_runs <- left_join(subject_run_key, new_motion_summarized, by = c("src_subject_id", "run")) # left join keeps only the rows in subject_run_key as desired!

# sum together the run separated motion stuff for this data
new_motion_summarized_condensed <- motion_correct_runs %>% 
  group_by(src_subject_id) %>%
  summarize(summed_fd = sum(n_below_thresh, na.rm = T)) # there are NAs if run doesn't exist for a subject (safe to ignore these)!


# ======================================= merge all cpt data together, then merge with subject data, then make long data frame ============================

part1 <- left_join(clean_default_mode, clean_reward, by = "src_subject_id")
part2 <- left_join(clean_dcs, clean_fronto_limbic, by = "src_subject_id")
full_cpt_data <- left_join(part2, part1, by = "src_subject_id") %>% select(-n_runs.y.y, -n_runs.y.x, -n_runs.x.y) %>% rename(n_runs = n_runs.x.x)

merged1 <- left_join(full_cpt_data, subject_data_clean, by = "src_subject_id")
merged <- left_join(merged1, new_motion_summarized_condensed, by = "src_subject_id") %>%
  mutate(fd_per_run = summed_fd/n_runs)

mod_df_fd_per_run <- glm(n_cpts_df ~ fd_per_run + mri_info_manufacturer + n_runs, data = merged, family = "poisson")
mod_r_fd_per_run <- glm(n_cpts_r ~ fd_per_run + mri_info_manufacturer + n_runs, data = merged, family = "poisson")
mod_fl_fd_per_run <- glm(n_cpts_fl ~ fd_per_run + mri_info_manufacturer + n_runs, data = merged, family = "poisson")
mod_dcs_fd_per_run <- glm(n_cpts_dcs ~ fd_per_run + mri_info_manufacturer + n_runs, data = merged, family = "poisson")

merged <- merged %>%
  filter(is.na(mri_info_manufacturer) == F) %>%
  mutate(adj_n_cpts_df = residuals(mod_df_fd_per_run, type = "pearson"),
         adj_n_cpts_r = residuals(mod_r_fd_per_run, type = "pearson"),
         adj_n_cpts_fl = residuals(mod_fl_fd_per_run, type = "pearson"),
         adj_n_cpts_dcs = residuals(mod_dcs_fd_per_run, type = "pearson")) 

cpt_long <- merged %>% # create long data frame with raw cpts
  pivot_longer(cols = starts_with("n_cpts"), names_to = "network", values_to = "n_cpts") %>%
  mutate(network = recode(network,
                          n_cpts_dcs = "dorsal cortico striatal",
                          n_cpts_df = "default mode",
                          n_cpts_r = "reward",
                          n_cpts_fl = "fronto limbic"))

cpt_long_adj <- merged %>% # create long data frame with adjusted cpts
  pivot_longer(cols = starts_with("adj_n_"), names_to = "network", values_to = "n_cpts_adj") %>%
  mutate(network = recode(network, 
                          adj_n_cpts_dcs = "dorsal cortico striatal", 
                          adj_n_cpts_df = "default mode",
                          adj_n_cpts_r = "reward",
                          adj_n_cpts_fl = "fronto limbic"))

# ===================================================================== complete case data =====================================================================

print_filtered_rows <- function(dataframe, ...) {
  # Reference:
  # https://stackoverflow.com/questions/41021976/print-number-of-rows-filtered-out-by-dplyrs-filter-function
  df <- dataframe
  vars = as.list(substitute(list(...)))[-1L]
  for(arg in vars) {
    dataframe <- df
    dataframe_new <- dataframe %>% filter(!!arg)
    # rows_filtered <- nrow(df) - nrow(dataframe_new) # From original function
    rows_filtered = (df$src_subject_id %>% unique() %>% length()) - (dataframe_new$src_subject_id %>% unique() %>% length())
    cat(sprintf('Filtered out %s subjects using: %s\n', rows_filtered, deparse(arg)))
    df = dataframe_new
  }
  return(dataframe_new)
}

cpt_long_adj_COMPLETE_CASE <- cpt_long_adj %>%
  select(src_subject_id, AnyDD_ever_p_y_binary, SA_ever_p_y_binary, SI_ever_p_y_binary, NSSI_ever_p_y_binary, network, n_cpts_adj, Black_race, Asian_race, 
         Other_race, Missing_race, Indigenous_race, White_race, demo_comb_income_v2_bl, 
         demo_ethn_v2, demo_prnt_ed_v2_bl, demo_prnt_marital_v2_bl, demo_gender_id_v2_comb, pubertal_status, interview_age,
         cbcl_scr_syn_totprob_r.x, cbcl_scr_syn_external_r.x, cbcl_scr_syn_internal_r.x, cbcl_scr_dsm5_depress_r.x,
         cbcl_scr_dsm5_anxdisord_r.x,cbcl_scr_dsm5_adhd_r.x, summed_fd, n_runs, mri_info_manufacturer, site_id_l) %>%
  print_filtered_rows(!is.na(AnyDD_ever_p_y_binary)) %>%
  print_filtered_rows(!is.na(SA_ever_p_y_binary)) %>%
  print_filtered_rows(!is.na(SI_ever_p_y_binary)) %>%
  print_filtered_rows(!is.na(NSSI_ever_p_y_binary)) %>%
  print_filtered_rows(!is.na(network)) %>%
  print_filtered_rows(!is.na(n_cpts_adj)) %>%
  print_filtered_rows(!is.na(Black_race)) %>%
  print_filtered_rows(!is.na(Asian_race)) %>%
  print_filtered_rows(!is.na(Other_race)) %>%
  print_filtered_rows(!is.na(Missing_race)) %>%
  print_filtered_rows(!is.na(Indigenous_race)) %>%
  print_filtered_rows(!is.na(White_race)) %>%
  print_filtered_rows(!is.na(demo_comb_income_v2_bl)) %>%
  print_filtered_rows(!is.na(demo_ethn_v2)) %>%
  print_filtered_rows(!is.na(demo_prnt_ed_v2_bl)) %>%
  print_filtered_rows(!is.na(demo_prnt_marital_v2_bl)) %>%
  print_filtered_rows(!is.na(demo_gender_id_v2_comb)) %>%
  print_filtered_rows(!is.na(pubertal_status)) %>%
  print_filtered_rows(!is.na(interview_age)) %>%
  print_filtered_rows(!is.na(cbcl_scr_syn_totprob_r.x)) %>%
  print_filtered_rows(!is.na(cbcl_scr_syn_external_r.x)) %>%
  print_filtered_rows(!is.na(cbcl_scr_syn_internal_r.x)) %>%
  print_filtered_rows(!is.na(cbcl_scr_dsm5_depress_r.x)) %>%
  print_filtered_rows(!is.na(cbcl_scr_dsm5_anxdisord_r.x)) %>%
  print_filtered_rows(!is.na(cbcl_scr_dsm5_adhd_r.x)) %>%
  print_filtered_rows(!is.na(summed_fd)) %>%
  print_filtered_rows(!is.na(site_id_l))

subjects_COMPLETE_CASE <- (cpt_long_adj_COMPLETE_CASE %>% select(src_subject_id) %>% distinct())$src_subject_id # vector of subject ids

merged_COMPLETE_CASE <- merged %>% filter(src_subject_id %in% subjects_COMPLETE_CASE)
cpt_long_COMPLETE_CASE <- cpt_long %>% filter(src_subject_id %in% subjects_COMPLETE_CASE)

# ===================================================================== write csvs =====================================================================

write.csv(cpt_long_adj_COMPLETE_CASE, 'cpt_long_adj_COMPLETE_CASE_update_11.13.24.csv')
write.csv(cpt_long_COMPLETE_CASE, 'cpt_long_COMPLETE_CASE_update_11.13.24.csv')
write.csv(merged_COMPLETE_CASE, 'merged_COMPLETE_CASE_update_11.13.24.csv')