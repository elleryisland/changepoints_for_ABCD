library(tidyverse)
library(tidyr) # data manipulation
library(broom) # for the tidy() function
library(knitr) # for kable()
library(kableExtra) # for extra functions for making kables
library(purrr) # for map_df
library(modelr) # for add_residuals() function
library(lme4) # for mixed effect models
library(lmerTest) # for mixed effect models (gives p values)
library(table1) # for making table 1
library(tibble) # for making the row names into a column
library(effectsize) # for standardizing parameters
library(wesanderson) # for color palette
library(ggpubr) # for stat cor function
library(cowplot)

# ===================================================================== read in data =====================================================================

cpt_long_adj_COMPLETE_CASE <- read.csv("Desktop/final cpt analysis/data/cpt_long_adj_COMPLETE_CASE_update_11.13.24.csv")
merged_COMPLETE_CASE <- read.csv('Desktop/final cpt analysis/data/merged_COMPLETE_CASE_update_11.13.24.csv')
cpt_long_COMPLETE_CASE <-read.csv("Desktop/final cpt analysis/data/cpt_long_COMPLETE_CASE_update_11.13.24.csv")

# matched groups info
matched_groups <-read.csv("Desktop/final cpt analysis/data/matched_groups.csv") 

matched_groups <- matched_groups %>%
  filter(session_id == "ses-baselineYear1Arm1") %>%
  mutate(participant_id = str_remove(participant_id, "sub-")) %>%
  rename(src_subject_id = participant_id)

groups_long <- left_join(cpt_long_adj_COMPLETE_CASE, matched_groups, by = "src_subject_id")
groups <- left_join(merged_COMPLETE_CASE, matched_groups, by = "src_subject_id")


# ===================================================================== clinical models =====================================================================
# lmer CBCL
cpt_long_long_adj_cbcl <- cpt_long_adj_COMPLETE_CASE %>%
  pivot_longer(c(cbcl_scr_syn_totprob_r.x, cbcl_scr_syn_external_r.x, cbcl_scr_syn_internal_r.x, cbcl_scr_dsm5_depress_r.x,
                 cbcl_scr_dsm5_anxdisord_r.x,cbcl_scr_dsm5_adhd_r.x),
               names_to = "outcome",
               values_to = "value")

cbcl_results_adj <- cpt_long_long_adj_cbcl %>% 
  nest_by(network, outcome) %>%
  mutate(modDD = list(lmerTest::lmer(value ~ Black_race + Asian_race + Other_race + Missing_race + Indigenous_race + White_race + as.numeric(demo_comb_income_v2_bl) + factor(demo_ethn_v2) + as.numeric(demo_prnt_ed_v2_bl) + factor(demo_prnt_marital_v2_bl) + factor(demo_gender_id_v2_comb) + pubertal_status + n_cpts_adj + interview_age + (1|site_id_l), data = data))) %>%
  reframe(data.frame(coef(summary(modDD))) %>% rownames_to_column("term")) %>% 
  filter(term == "n_cpts_adj") %>% 
  select(-term) %>% 
  select(-df) %>%
  unnest() %>%
  group_by(network) %>%
  mutate(p.adj.network = p.adjust(Pr...t.., method = "BH")) %>%
  ungroup() %>%
  group_by(outcome) %>%
  mutate(p.adj.outcome = p.adjust(Pr...t.., method = "BH")) %>%
  mutate(across(c(Estimate, Std..Error, t.value, Pr...t.., p.adj.network, p.adj.outcome), ~round(.x, 3))) %>%
  mutate(outcome = str_remove(outcome, "cbcl_scr_"),
         outcome = str_remove(outcome, "_r.x")) %>%
  rename(Estimate = Estimate,
        `Standard Error` = Std..Error,
         Statistic = t.value,
         `p-value` = `Pr...t..`) %>%
  mutate(outcome = case_when(outcome == "dsm5_adhd" ~ "ADHD",
                             outcome == "syn_external" ~ "Externalizing",
                             outcome == "syn_totprob" ~ "Total Problems",
                             outcome == "dsm5_anxdisord" ~ "Anxiety",
                             outcome == "syn_internal" ~ "Internalizing",
                            outcome == "dsm5_depress" ~ "Depression"),
         network = case_when(network == "default mode" ~ "Default Mode",
                             network == "reward" ~ "Reward",
                             network == "dorsal cortico striatal" ~ "Dorsal Corticostriatal",
                             network == "fronto limbic" ~ "Frontolimbic")) %>%
  select(-p.adj.network, - Statistic) %>%
  rename(Outcome = outcome,
         Network = network,
         `Adjusted p-value` = p.adj.outcome)


kable(cbcl_results_adj, format = "html") %>% 
  kable_styling(full_width = F) %>%
  row_spec(which(cbcl_results_adj$`Adjusted p-value` <= 0.05), bold = F, color = "black", background = "grey95") 

# ======================================================================= standardized coefficients ======================================================================================================

cbcl_results_STANDARDIZED <- cpt_long_long_adj_cbcl %>% 
  nest_by(network, outcome) %>%
  mutate(modDD = list(lmerTest::lmer(value ~ Black_race + Asian_race + Other_race + Missing_race + Indigenous_race + White_race + as.numeric(demo_comb_income_v2_bl) + demo_ethn_v2 + as.numeric(demo_prnt_ed_v2_bl) + demo_prnt_marital_v2_bl + demo_gender_id_v2_comb + pubertal_status + n_cpts_adj + interview_age + (1|site_id_l), data = data))) %>%
  reframe(data.frame(standardize_parameters(modDD))) %>% 
  filter(Parameter == "n_cpts_adj") %>%
  select(-Parameter, -CI) %>%
  unnest() %>%
  mutate(outcome = str_remove(outcome, "cbcl_scr_"),
         outcome = str_remove(outcome, "_r.x")) %>%
  mutate(outcome = case_when(outcome == "dsm5_adhd" ~ "ADHD",
                             outcome == "syn_external" ~ "Externalizing",
                             outcome == "syn_totprob" ~ "Total Problems",
                             outcome == "dsm5_anxdisord" ~ "Anxiety",
                             outcome == "syn_internal" ~ "Internalizing",
                             outcome == "dsm5_depress" ~ "Depression"),
         network = case_when(network == "default mode" ~ "Default Mode",
                             network == "reward" ~ "Reward",
                             network == "dorsal cortico striatal" ~ "Dorsal Corticostriatal",
                             network == "fronto limbic" ~ "Frontolimbic")) %>%
  rename(Outcome = outcome,
         Network = network)

kable(cbcl_results_STANDARDIZED, format = "html") %>% 
  kable_styling(full_width = F)

# ==================================================================== changepoint outcome models ======================================================================================================

cpt_outcome_data_numeric_preds <- cpt_long_adj_COMPLETE_CASE %>%
  pivot_longer(cols = c(demo_comb_income_v2_bl, demo_prnt_ed_v2_bl, pubertal_status, interview_age),
               names_to = "pred_name", values_to ="pred") %>%
  select(src_subject_id, pred_name, pred, site_id_l, n_cpts_adj, network)

cpt_outcome_data_cat_preds <- cpt_long_adj_COMPLETE_CASE %>%
  mutate(demo_prnt_marital_v2_bl = case_when(demo_prnt_marital_v2_bl == 1 ~ "Married",
                                      demo_prnt_marital_v2_bl == 2 ~ "Widowed",
                                      demo_prnt_marital_v2_bl == 3 ~ "Divorced",
                                      demo_prnt_marital_v2_bl== 4 ~ "Separated",
                                      demo_prnt_marital_v2_bl == 5 ~ "Never married",
                                      demo_prnt_marital_v2_bl == 6 ~ "Living with partner"),
         demo_gender_id_v2_comb = case_when(demo_gender_id_v2_comb == 1 ~ "Male",
                            demo_gender_id_v2_comb == 2 ~ "Female",
                            demo_gender_id_v2_comb == 3 ~ "Trans male",
                            demo_gender_id_v2_comb == 4 ~ "Trans female",
                            demo_gender_id_v2_comb == 5 ~ "Gender queer",
                            demo_gender_id_v2_comb == 6 ~ "Different gender")) %>%
  pivot_longer(cols = c(demo_prnt_marital_v2_bl, demo_gender_id_v2_comb),
               names_to = "pred_name", values_to ="pred") %>%
  select(src_subject_id, pred_name, pred, site_id_l, n_cpts_adj, network) %>%
  mutate(pred = as.factor(pred))

cpt_outcome_mods_numeric_preds <- cpt_outcome_data_numeric_preds %>%
  nest_by(network, pred_name) %>%
  mutate(mod = list(glm(n_cpts_adj ~ pred, data = data, family = "gaussian"))) %>%
  reframe(tidy(mod)) %>%
  unnest() %>%
  filter(term == "pred") %>%
  select(-term) %>%
  mutate(pred_name = case_when(pred_name == "demo_comb_income_v2_bl" ~ "Parental Income", 
                               pred_name == "demo_prnt_ed_v2_bl" ~ "Parental Education", 
                               pred_name == "pubertal_status" ~ "Pubertal Status", 
                               pred_name == "interview_age" ~ "Age (months)"),
    network = case_when(network == "default mode" ~ "Default Mode",
                        network == "reward" ~ "Reward",
                        network == "dorsal cortico striatal" ~ "Dorsal Cortico Striatal",
                        network == "fronto limbic" ~ "Fronto Limbic")) %>%
  rename(Covariate = pred_name, 
         Estimate = estimate,
         `Standard Error` = std.error,
         Network = network) %>%
  mutate(Level = "")%>%
  select(-statistic)

cpt_outcome_mods_cat_preds <- cpt_outcome_data_cat_preds %>%
  nest_by(network, pred_name) %>%
  mutate(mod = list(glm(n_cpts_adj ~ pred, data = data, family = "gaussian"))) %>%
  reframe(tidy(mod)) %>%
  unnest() %>%
  filter(term != "(Intercept)") %>%
  select(-statistic) %>%
  mutate(pred_name = case_when(pred_name == "demo_prnt_marital_v2_bl" ~ "Marital Status",
                               pred_name == "demo_gender_id_v2_comb" ~ "Gender"),
    network = case_when(network == "default mode" ~ "Default Mode",
                        network == "reward" ~ "Reward",
                        network == "dorsal cortico striatal" ~ "Dorsal Cortico Striatal",
                        network == "fronto limbic" ~ "Fronto Limbic")) %>%
  mutate(term = str_remove(term, "pred")) %>%
  rename(Covariate = pred_name, 
         Level = term,
         Estimate = estimate,
         `Standard Error` = std.error,
         Network = network)  

race_mod <- cpt_long_adj_COMPLETE_CASE %>% 
  nest_by(network) %>%
  mutate(mod = list(glm(n_cpts_adj ~ Black_race + Asian_race + Other_race + Indigenous_race + White_race + factor(demo_ethn_v2), data = data, family = "gaussian"))) %>%
  reframe(tidy(mod)) %>%
  filter(term != "(Intercept)") %>%
  mutate(Level = "") %>%
  rename(Covariate = term, 
         Estimate = estimate,
         `Standard Error` = std.error,
         Network = network)  %>%
  select(-statistic)
  
univariate_correlates <- rbind(cpt_outcome_mods_cat_preds, cpt_outcome_mods_numeric_preds, race_mod) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

sig_correlates <- univariate_correlates %>%
  filter(p.adj <= 0.05) %>%
  mutate(across(c(Estimate, `Standard Error`, p.value, p.adj), ~round(.x, 3)))

  
# ======================================================================= split half analyses ======================================================================================================
# see table 1 section for info on demographic split

# adjusting changepoints for each group

groups_small <- groups %>% 
  select(src_subject_id, matched_group, n_runs, starts_with("n_cpts"), fd_per_run, mri_info_manufacturer, 
         cbcl_scr_syn_totprob_r.x, cbcl_scr_syn_external_r.x, cbcl_scr_syn_internal_r.x, cbcl_scr_dsm5_depress_r.x, cbcl_scr_dsm5_anxdisord_r.x,cbcl_scr_dsm5_adhd_r.x, 
         Black_race, Asian_race, Other_race, Missing_race, Indigenous_race, White_race, demo_comb_income_v2_bl, demo_ethn_v2, demo_prnt_ed_v2_bl, demo_prnt_marital_v2_bl, demo_gender_id_v2_comb, pubertal_status, interview_age, site_id_l)
  
  
groups1 <- groups_small %>% filter(matched_group == 1)
groups2 <- groups_small %>% filter(matched_group == 2)

mod_df_fd_per_run1 <- glm(n_cpts_df ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups1, family = "poisson")
mod_r_fd_per_run1 <- glm(n_cpts_r ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups1, family = "poisson")
mod_fl_fd_per_run1 <- glm(n_cpts_fl ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups1, family = "poisson")
mod_dcs_fd_per_run1 <- glm(n_cpts_dcs ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups1, family = "poisson")

mod_df_fd_per_run2 <- glm(n_cpts_df ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups2, family = "poisson")
mod_r_fd_per_run2 <- glm(n_cpts_r ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups2, family = "poisson")
mod_fl_fd_per_run2 <- glm(n_cpts_fl ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups2, family = "poisson")
mod_dcs_fd_per_run2 <- glm(n_cpts_dcs ~ fd_per_run + mri_info_manufacturer + n_runs, data = groups2, family = "poisson")

groups1 <- groups1 %>%
  select(-starts_with("adj_n_cpts")) %>%
  filter(is.na(mri_info_manufacturer) == F) %>%
  mutate(adj_n_cpts_df = residuals(mod_df_fd_per_run1, type = "pearson"),
         adj_n_cpts_r = residuals(mod_r_fd_per_run1, type = "pearson"),
         adj_n_cpts_fl = residuals(mod_fl_fd_per_run1, type = "pearson"),
         adj_n_cpts_dcs = residuals(mod_dcs_fd_per_run1, type = "pearson")) 

groups2 <- groups2 %>%
  filter(is.na(mri_info_manufacturer) == F) %>%
  mutate(adj_n_cpts_df = residuals(mod_df_fd_per_run2, type = "pearson"),
         adj_n_cpts_r = residuals(mod_r_fd_per_run2, type = "pearson"),
         adj_n_cpts_fl = residuals(mod_fl_fd_per_run2, type = "pearson"),
         adj_n_cpts_dcs = residuals(mod_dcs_fd_per_run2, type = "pearson"))

groups1_long <- groups1 %>%
  pivot_longer(cols = starts_with("adj_n_cpts"), names_to = "network", values_to = "n_cpts_adj_group1") 

groups1_long_long <- groups1_long %>%
  pivot_longer(c(cbcl_scr_syn_totprob_r.x, cbcl_scr_syn_external_r.x, cbcl_scr_syn_internal_r.x, cbcl_scr_dsm5_depress_r.x,
                 cbcl_scr_dsm5_anxdisord_r.x,cbcl_scr_dsm5_adhd_r.x),
               names_to = "outcome",
               values_to = "value")

groups2_long <- groups2 %>%
  pivot_longer(cols = starts_with("adj_n_cpts"), names_to = "network", values_to = "n_cpts_adj_group2")

groups2_long_long <- groups2_long %>%
  pivot_longer(c(cbcl_scr_syn_totprob_r.x, cbcl_scr_syn_external_r.x, cbcl_scr_syn_internal_r.x, cbcl_scr_dsm5_depress_r.x,
                 cbcl_scr_dsm5_anxdisord_r.x,cbcl_scr_dsm5_adhd_r.x),
               names_to = "outcome",
               values_to = "value")
# models (and test the difference between estimates in group 1 and 2)

g1 <- groups1_long_long %>% 
  nest_by(network, outcome) %>%
  mutate(modDD = list(lmerTest::lmer(value ~ Black_race + Asian_race + Other_race + Missing_race + Indigenous_race + White_race + as.numeric(demo_comb_income_v2_bl) + factor(demo_ethn_v2) + as.numeric(demo_prnt_ed_v2_bl) + factor(demo_prnt_marital_v2_bl) + factor(demo_gender_id_v2_comb) + pubertal_status + n_cpts_adj_group1 + interview_age + (1|site_id_l), data = data))) %>%
  reframe(data.frame(coef(summary(modDD))) %>% rownames_to_column("term")) %>%
  filter(term == "n_cpts_adj_group1") %>%
  mutate(group = "group1")

g2 <- groups2_long_long %>% 
  nest_by(network, outcome) %>%
  mutate(modDD = list(lmerTest::lmer(value ~ Black_race + Asian_race + Other_race + Missing_race + Indigenous_race + White_race + as.numeric(demo_comb_income_v2_bl) + factor(demo_ethn_v2) + as.numeric(demo_prnt_ed_v2_bl) + factor(demo_prnt_marital_v2_bl) + factor(demo_gender_id_v2_comb) + pubertal_status + n_cpts_adj_group2 + interview_age + (1|site_id_l), data = data))) %>%
  reframe(data.frame(coef(summary(modDD))) %>% rownames_to_column("term")) %>%
  filter(term == "n_cpts_adj_group2") %>%
  mutate(group = "group2")

split_half_output <- rbind(g1, g2)

thing <- split_half_output %>%
  group_by(network) %>%
  do(tidy(t.test(Estimate ~ group, data = .))) %>%
  select(network, statistic, p.value) %>%
  mutate(network = str_remove(network, "adj_n_cpts_"))

thing2 <- split_half_output %>%
  group_by(outcome) %>%
  do(tidy(t.test(Estimate ~ group, data = .))) %>%
  select(outcome, statistic, p.value)
  
# models (standardized coefs)

GROUP1_standardized <- groups1_long_long %>% 
  nest_by(network, outcome) %>%
  mutate(modDD = list(lmerTest::lmer(value ~ Black_race + Asian_race + Other_race + Missing_race + Indigenous_race + White_race + as.numeric(demo_comb_income_v2_bl) + factor(demo_ethn_v2) + as.numeric(demo_prnt_ed_v2_bl) + factor(demo_prnt_marital_v2_bl) + factor(demo_gender_id_v2_comb) + pubertal_status + n_cpts_adj_group1 + interview_age + (1|site_id_l), data = data))) %>%
  reframe(data.frame(standardize_parameters(modDD))) %>%
  filter(Parameter == "n_cpts_adj_group1")

GROUP1_standardized <- GROUP1_standardized %>%
  mutate(outcome = case_when(outcome == "cbcl_scr_dsm5_adhd_r.x" ~ "ADHD",
                             outcome == "cbcl_scr_syn_external_r.x" ~ "Externalizing",
                             outcome == "cbcl_scr_syn_totprob_r.x" ~ "Total Problems",
                             outcome == "cbcl_scr_dsm5_anxdisord_r.x" ~ "Anxiety",
                             outcome == "cbcl_scr_syn_internal_r.x" ~ "Internalizing",
                             outcome == "cbcl_scr_dsm5_depress_r.x" ~ "Depression"),
         network = case_when(network == "adj_n_cpts_df" ~ "Default Mode",
                             network == "adj_n_cpts_r" ~ "Reward",
                             network == "adj_n_cpts_dcs" ~ "Dorsal Corticostriatal",
                             network == "adj_n_cpts_fl" ~ "Frontolimbic"))

GROUP2_standardized <- groups2_long_long %>% 
  nest_by(network, outcome) %>%
  mutate(modDD = list(lmerTest::lmer(value ~ Black_race + Asian_race + Other_race + Missing_race + Indigenous_race + White_race + as.numeric(demo_comb_income_v2_bl) + factor(demo_ethn_v2) + as.numeric(demo_prnt_ed_v2_bl) + factor(demo_prnt_marital_v2_bl) + factor(demo_gender_id_v2_comb) + pubertal_status + n_cpts_adj_group2 + interview_age + (1|site_id_l), data = data))) %>%
  reframe(data.frame(standardize_parameters(modDD))) %>%
  filter(Parameter == "n_cpts_adj_group2") %>%
  mutate(outcome = case_when(outcome == "cbcl_scr_dsm5_adhd_r.x" ~ "ADHD",
                             outcome == "cbcl_scr_syn_external_r.x" ~ "Externalizing",
                             outcome == "cbcl_scr_syn_totprob_r.x" ~ "Total Problems",
                             outcome == "cbcl_scr_dsm5_anxdisord_r.x" ~ "Anxiety",
                             outcome == "cbcl_scr_syn_internal_r.x" ~ "Internalizing",
                             outcome == "cbcl_scr_dsm5_depress_r.x" ~ "Depression"),
         network = case_when(network == "adj_n_cpts_df" ~ "Default Mode",
                             network == "adj_n_cpts_r" ~ "Reward",
                             network == "adj_n_cpts_dcs" ~ "Dorsal Corticostriatal",
                             network == "adj_n_cpts_fl" ~ "Frontolimbic"))

# make standardized coef data table
GROUP1_standardized <- GROUP1_standardized %>%
  mutate(group = "1")

GROUP2_standardized <- GROUP2_standardized %>%
  mutate(group = "2")

plot_data <- rbind(GROUP1_standardized, GROUP2_standardized)

# plot

split_half_forest_plot <- plot_data %>%
ggplot() +
  geom_pointrange(aes(x = reorder(outcome, Std_Coefficient), 
                      y = Std_Coefficient, 
                      ymin = CI_low, 
                      ymax = CI_high,
                      color = group), position = position_nudge(x = (plot_data$group ==1)* .30), size = .25, stroke = 1, linewidth = 1) + 
  geom_hline(yintercept = 0, lty = 2) + 
  coord_flip() +  # flip coordinates (puts labels on y axis)
  theme_classic()+
  facet_wrap(~network) +
  labs(title = "B", x = NULL, y = "Adjusted Changepoint Standardized Coefficient Estimate", color = "ARM") +
  theme(text = element_text(family = "Times New Roman")) +
  scale_color_manual(values = c("grey", "grey6")) +
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(override.aes = list(shape = 15, size = 1.5) ) )

split_half_histogram <- groups %>%
  filter(matched_group < 3) %>%
  pivot_longer(names_to = "network", cols = starts_with("n_cpts"), values_to = "n_cpts") %>%
  mutate(network = case_when(network == "n_cpts_dcs" ~ "Dorsal Corticostrial", 
                             network == "n_cpts_r" ~ "Reward", 
                             network == "n_cpts_df" ~ "Default Mode", 
                             network == "n_cpts_fl" ~ "Frontolimbic")) %>%
  ggplot(aes(x = factor(n_cpts), fill = factor(matched_group))) +
  geom_histogram(stat = 'count', position = 'dodge', size = 0.4) +
  facet_wrap(~network) +
  theme_classic() +
  labs(title = "A", y = "Count", x = "Total Number of Changepoints", fill = "Group") + 
  scale_fill_manual(values = c("grey", "grey6")) +
  theme(legend.position = "none") + 
  theme(text = element_text(family = "Times New Roman"))


split_half_hist_forest <- plot_grid(split_half_histogram, split_half_forest_plot, ncol = 1, align = "v", axis = "l")
ggsave("Desktop/final cpt analysis/figures/split_half_hist_forest.png", split_half_hist_forest, width = 6, height = 8)



# ============================================================================ final plots ======================================================================================================

cbcl_forest_plot <- left_join(cbcl_results_STANDARDIZED, cbcl_results_adj, by = c("Network", "Outcome")) 

forest_plot <- cbcl_forest_plot %>%
  mutate(p_label = if_else(`Adjusted p-value` <= 0.05, "*", "")) %>%
  ggplot(aes(x = reorder(Outcome, Std_Coefficient), 
             y = Std_Coefficient, 
             ymin = CI_low, 
             ymax = CI_high,
             color = Network)) +
  geom_pointrange(aes(shape = p_label), fill = "white", size = 1, stroke = 1.5, linewidth = 1) + # fill white so you don't see the lines through the circles
  geom_hline(yintercept=0, lty=2) + 
  # geom_text(aes(x = outcome, y = 0.075, label = p_label), color = "black") +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(x = "CBCL Outcome", 
       y = "Adjusted Changepoint Standardized Coefficient Estimates") +
  theme_classic()+
  facet_wrap(~Network) + 
  scale_shape_manual(values = c(21, 8)) + # 21 = open circle, 8 = *
  scale_color_manual(values = wes_palette("Darjeeling1")[2:5]) +
  theme(legend.position="none")  + # remove legend
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  theme(panel.spacing = unit(20, "pt")) # add space between the panels so text doesn't overlap

# HISTOGRAM: DISTRIBUTION OF CHANGEPOINTS
cpt_hist <- cpt_long_COMPLETE_CASE %>%
  mutate(network = case_when(
    network == "default mode" ~ "Default Mode Network",
    network == "reward" ~ "Reward Network",
    network == "dorsal cortico striatal" ~ "Dorsal Corticostriatal Network",
    network == "fronto limbic" ~ "Frontolimbic Network")) %>%
  ggplot(aes(x = factor(n_cpts), fill = network, color = network)) +
  geom_histogram(stat = 'count') +
  facet_wrap(~network) +
  theme_classic() +
  labs(y = "Count", x = "Total Number of Changepoints") + 
  scale_fill_manual(values = wes_palette("Darjeeling1")[2:5]) +
  scale_color_manual(values = wes_palette("Darjeeling1")[2:5]) +
  theme(legend.position = "none") + 
  theme(text = element_text(family = "Times New Roman")) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) 

# =========================================================================================== table 1 ======================================================================================================

table1_data <- merged_COMPLETE_CASE %>%
  mutate(Race = case_when(Asian_race == 1 ~ "Asian",
            Black_race == 1 ~ "Black/African American",
            Indigenous_race == 1 ~ "Native American, Pacific Islander",
            Other_race== 1 ~ "Other",
            White_race == 1 ~ "White",
            factor(demo_ethn_v2) == 1 ~ "Hispanic/Latinx")) %>%
  mutate(demo_comb_income_v2_bl = as.numeric(demo_comb_income_v2_bl),
         demo_prnt_ed_v2_bl = as.numeric(demo_prnt_ed_v2_bl)) %>%
  mutate(`Marital Status` = case_when(demo_prnt_marital_v2_bl == 1 ~ "Married",
                                    demo_prnt_marital_v2_bl == 2 ~ "Widowed",
                                    demo_prnt_marital_v2_bl == 3 ~ "Divorced",
                                    demo_prnt_marital_v2_bl== 4 ~ "Separated",
                                    demo_prnt_marital_v2_bl == 5 ~ "Never married",
                                    demo_prnt_marital_v2_bl == 6 ~ "Living with partner"),
         Gender = case_when(demo_gender_id_v2_comb == 1 ~ "Male",
                            demo_gender_id_v2_comb == 2 ~ "Female",
                            demo_gender_id_v2_comb == 3 ~ "Trans male",
                            demo_gender_id_v2_comb == 4 ~ "Trans female",
                            demo_gender_id_v2_comb == 5 ~ "Gender queer",
                            demo_gender_id_v2_comb == 6 ~ "Different gender"),
         `Pubertal Status` = case_when(pubertal_status == 1 ~ "Pre-Puberty", 
                                     pubertal_status == 2 ~ "Early Puberty",
                                     pubertal_status == 3 ~ "Mid Puberty",
                                     pubertal_status == 4 ~ "Late Puberty",
                                     pubertal_status == 5 ~"Post Puberty"),
         Income = case_when(demo_comb_income_v2_bl >= 1 & demo_comb_income_v2_bl <= 6 ~ "$0 - $50,000",
                            demo_comb_income_v2_bl >= 7 & demo_comb_income_v2_bl <= 8 ~ "$50,000 - $99,999",
                            demo_comb_income_v2_bl == 9 ~ "$100,000 - $199,999",
                            demo_comb_income_v2_bl == 10 ~ "$200,000 + "),
         Education = case_when(demo_prnt_ed_v2_bl <= 12 ~ "Less than high school",
                               demo_prnt_ed_v2_bl == 13 | demo_prnt_ed_v2_bl == 14 ~ "High school diploma or GED",
                               demo_prnt_ed_v2_bl == 15  ~ "Some college",
                               demo_prnt_ed_v2_bl >= 16 & demo_prnt_ed_v2_bl <= 18 ~ "Finished college",
                               demo_prnt_ed_v2_bl >= 19 & demo_prnt_ed_v2_bl <= 21 ~ "Graduate Degree"),
         interview_age = interview_age/12) %>%
  rename(Depression = AnyDD_ever_p_y_binary,
         `Suicide Attempt` = SA_ever_p_y_binary,
         `Suicidal Ideation` = SI_ever_p_y_binary,
         `Non-Suicidal Self-Injury` = NSSI_ever_p_y_binary,
         Age = interview_age,
         `Number of Changepoints DCS` = n_cpts_dcs,
         `Number of Changepoints DF` = n_cpts_df,
         `Number of Changepoints FL` = n_cpts_fl,
         `Number of Changepoints R` = n_cpts_r,
         `CBCL: Total Problems` = cbcl_scr_syn_totprob_r.x ,
         `CBCL: Externalizing` = cbcl_scr_syn_external_r.x, 
         `CBCL: Internalizing` = cbcl_scr_syn_internal_r.x, 
         `CBCL: Depression` = cbcl_scr_dsm5_depress_r.x, 
         `CBCL: Anxiety` = cbcl_scr_dsm5_anxdisord_r.x,
         `CBCL: ADHD` = cbcl_scr_dsm5_adhd_r.x ) %>%
  mutate(across(c(Income, Education, `Marital Status`), ~factor(.x)))

table1_enar <- table1(~ factor(Asian_race) + factor(Black_race) + factor(Indigenous_race) + factor(Other_race) + factor(White_race) + factor(demo_ethn_v2) + Income + Education + `Marital Status` + Gender + `Pubertal Status` + Age + 
         `CBCL: Total Problems` + `CBCL: Externalizing` + `CBCL: Internalizing` + `CBCL: Depression` + `CBCL: Anxiety` + `CBCL: ADHD` +
         `Number of Changepoints DCS` + `Number of Changepoints DF` + `Number of Changepoints FL` + `Number of Changepoints R`, 
       data = table1_data)

kable(as.data.frame(table1_enar), format = "html") %>% 
  kable_styling(full_width = F)

IQR(merged_COMPLETE_CASE$n_cpts_dcs)
median(merged_COMPLETE_CASE$n_cpts_df)
IQR(merged_COMPLETE_CASE$n_cpts_fl)
IQR(merged_COMPLETE_CASE$n_cpts_r)

# demographic check

table1_data_split <- groups %>%
  mutate(Race = case_when(Asian_race == 1 ~ "Asian",
                          Black_race == 1 ~ "Black/African American",
                          Indigenous_race == 1 ~ "Native American, Pacific Islander",
                          Other_race== 1 ~ "Other",
                          White_race == 1 ~ "White",
                          factor(demo_ethn_v2) == 1 ~ "Hispanic/Latinx")) %>%
  mutate(demo_comb_income_v2_bl = as.numeric(demo_comb_income_v2_bl),
         demo_prnt_ed_v2_bl = as.numeric(demo_prnt_ed_v2_bl)) %>%
  mutate(`Marital Status` = case_when(demo_prnt_marital_v2_bl == 1 ~ "Married",
                                      demo_prnt_marital_v2_bl == 2 ~ "Widowed",
                                      demo_prnt_marital_v2_bl == 3 ~ "Divorced",
                                      demo_prnt_marital_v2_bl== 4 ~ "Separated",
                                      demo_prnt_marital_v2_bl == 5 ~ "Never married",
                                      demo_prnt_marital_v2_bl == 6 ~ "Living with partner"),
         Gender = case_when(demo_gender_id_v2_comb == 1 ~ "Male",
                            demo_gender_id_v2_comb == 2 ~ "Female",
                            demo_gender_id_v2_comb == 3 ~ "Trans male",
                            demo_gender_id_v2_comb == 4 ~ "Trans female",
                            demo_gender_id_v2_comb == 5 ~ "Gender queer",
                            demo_gender_id_v2_comb == 6 ~ "Different gender"),
         `Pubertal Status` = case_when(pubertal_status == 1 ~ "Pre-Puberty", 
                                       pubertal_status == 2 ~ "Early Puberty",
                                       pubertal_status == 3 ~ "Mid Puberty",
                                       pubertal_status == 4 ~ "Late Puberty",
                                       pubertal_status == 5 ~"Post Puberty"),
         Income = case_when(demo_comb_income_v2_bl >= 1 & demo_comb_income_v2_bl <= 6 ~ "$0 - $50,000",
                            demo_comb_income_v2_bl >= 7 & demo_comb_income_v2_bl <= 8 ~ "$50,000 - $99,999",
                            demo_comb_income_v2_bl == 9 ~ "$100,000 - $199,999",
                            demo_comb_income_v2_bl == 10 ~ "$200,000 + "),
         Education = case_when(demo_prnt_ed_v2_bl <= 12 ~ "Less than high school",
                               demo_prnt_ed_v2_bl == 13 | demo_prnt_ed_v2_bl == 14 ~ "High school diploma or GED",
                               demo_prnt_ed_v2_bl == 15  ~ "Some college",
                               demo_prnt_ed_v2_bl >= 16 & demo_prnt_ed_v2_bl <= 18 ~ "Finished college",
                               demo_prnt_ed_v2_bl >= 19 & demo_prnt_ed_v2_bl <= 21 ~ "Graduate Degree"),
         interview_age = interview_age/12) %>%
  rename(Depression = AnyDD_ever_p_y_binary,
         `Suicide Attempt` = SA_ever_p_y_binary,
         `Suicidal Ideation` = SI_ever_p_y_binary,
         `Non-Suicidal Self-Injury` = NSSI_ever_p_y_binary,
         Age = interview_age,
         `Number of Changepoints DCS` = n_cpts_dcs,
         `Number of Changepoints DF` = n_cpts_df,
         `Number of Changepoints FL` = n_cpts_fl,
         `Number of Changepoints R` = n_cpts_r) %>%
  mutate(across(c(Income, Education, `Marital Status`), ~factor(.x)))

table1(~ factor(Asian_race) + factor(Black_race) + factor(Indigenous_race) + factor(Other_race) + factor(White_race) + factor(demo_ethn_v2) + Income + Education + `Marital Status` + Gender + `Pubertal Status` + Age + 
         Depression + `Suicide Attempt` + `Suicidal Ideation` + `Non-Suicidal Self-Injury` + 
         `Number of Changepoints DCS` + `Number of Changepoints DF` + `Number of Changepoints FL` + `Number of Changepoints R` | matched_group, 
       data = table1_data_split)