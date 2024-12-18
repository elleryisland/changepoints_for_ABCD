# changepoints_for_ABCD

1: find_changepoints.R
This file reads in fMRI data for subjects and detects changepoints in dFC in the default mode network (the procedure is identical for the other networks except that different regions are selected). 

2: clean_cpt_data.R
This file pulls the output created by running find_changepoints.R for all subjects and merges them into one data set. It also summarizes the number of changepoints detected for each subject and merges this data with subject demographic / mental health data. It writes csv files (in various formats). 

3: post_hoc_analysis.R
This file takes the csv files written by clean_cpt_data.R and performs the post-hoc analysis and descriptive statistics described in the paper. 
