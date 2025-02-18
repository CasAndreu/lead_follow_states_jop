################################################################################
# AppendixI.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate the tables in  Appendix I of the paper.
# Data In:  
#           1. Dataset with information about how connected state legislators
#              are on Twitter to: MCs, National and State media, and Partisans
#              - ./data/state-legislators-network-connections-data.csv
#
#           2. Dataset with information about whether partisans mention state
#              legislators in their tweets
#              - ./data/state-legislators-mentions-by-partisans-summary.csv
#
# Data Out:
#           1. Figure I1
#           2. Figure I2
################################################################################

#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(boot)
library(ggplot2)

#===============================================================================
# DATA
#===============================================================================
# - load dataset with info about how state legislators are connected to other
#   groups under analysis
db <- read.csv("./data/state-legislators-network-connections-data.csv",
               colClasses = "character") %>%
  # ... one additional stats that I forgot to add and can be calculated from
  #.    existing variables in the dataset
  mutate(followed_mcs_n = as.numeric(followed_mcs_dem_n) + 
           as.numeric(followed_mcs_rep_n))

# - load dataset with info about how often partisans mention state legislators
#   in their tweets
db02 <- read.csv("./data/state-legislators-mentions-by-partisans-summary.csv",
                 colClasses = "character")

# MAIN
#===============================================================================

#-------------------------------------------------------------------------------
# [ A ] TABLE J1
#-------------------------------------------------------------------------------
# - calculate aggregate statistics
mc_stats <- data.frame(
  Group = "Members of Congress",
  Sample = c("All", "Same party", "Same state", "Same state and party"),
  Average = c(
    round(mean(as.numeric(db$followed_mcs_n)), 2),
    round(mean(as.numeric(db$followed_mcs_sameparty_n)), 2),
    round(mean(as.numeric(db$followed_mcs_samestate_n)), 2),
    round(mean(as.numeric(db$followed_mcs_samestate_sameparty_n)), 2)),
  Median = c(
    median(as.numeric(db$followed_mcs_n)),
    median(as.numeric(db$followed_mcs_sameparty_n)),
    median(as.numeric(db$followed_mcs_samestate_n)),
    median(as.numeric(db$followed_mcs_samestate_sameparty_n))),
  StdDev = c(
    round(sd(as.numeric(db$followed_mcs_n)), 2),
    round(sd(as.numeric(db$followed_mcs_sameparty_n)), 2),
    round(sd(as.numeric(db$followed_mcs_samestate_n)), 2),
    round(sd(as.numeric(db$followed_mcs_samestate_sameparty_n)), 2))
)

natmedia_stats <- data.frame(
  Group = "National Media",
  Sample = c("All", "Same party", "Same state", "Same state and party"),
  Average = c(
    round(mean(as.numeric(db$followed_natmedia_n)), 2),
    "",
    "",
    ""),
  Median = c(
    round(median(as.numeric(db$followed_natmedia_n)), 2),
    "",
    "",
    ""),
  StdDev = c(
    round(sd(as.numeric(db$followed_natmedia_n)), 2),
    "",
    "",
    "")
)

statemedia_stats <- data.frame(
  Group = "State Media",
  Sample = c("All", "Same party", "Same state", "Same state and party"),
  Average = c(
    round(mean(as.numeric(db$followed_statemedia_n)), 2),
    "",
    round(mean(as.numeric(db$followed_statemedia_samestate_n)), 2),
    ""),
  Median = c(
    round(median(as.numeric(db$followed_statemedia_n)), 2),
    "",
    round(median(as.numeric(db$followed_statemedia_samestate_n)), 2),
    ""),
  StdDev = c(
    round(sd(as.numeric(db$followed_statemedia_n)), 2),
    "",
    round(sd(as.numeric(db$followed_statemedia_samestate_n)), 2),
    "")
)

partisan_stats <- data.frame(
  Group = "State Partisans",
  Sample = c("All", "Same party", "Same state", "Same state and party"),
  Average = c(
    "",
    "",
    "",
    round(mean(as.numeric(db$followed_mcs_samestate_sameparty_n)), 2)),
  Median = c(
    "",
    "",
    "",
    round(median(as.numeric(db$followed_mcs_samestate_sameparty_n)), 2)),
  StdDev = c(
    "",
    "",
    "",
    round(sd(as.numeric(db$followed_mcs_samestate_sameparty_n)), 2))
)

# - combine the stats for all groups together
all_stats <- rbind(mc_stats, natmedia_stats, statemedia_stats, partisan_stats)
#----------
# TABLE I1
#----------
print(all_stats)
#                                            Group               Sample Average Median StdDev
#                              Members of Congress                  All   11.89      8  14.69
#                              Members of Congress           Same party   10.44      7  11.55
#                              Members of Congress           Same state    6.92      5   6.23
#                              Members of Congress Same state and party    6.02      5   5.28
#                                   National Media                  All     1.2      1   1.27
#                                   National Media           Same party                      
#                                   National Media           Same state                      
#                                   National Media Same state and party                      
#                                      State Media                  All    7.54      6   8.26
#                                      State Media           Same party                      
#                                      State Media           Same state    6.74      5   7.73
#                                      State Media Same state and party                      
#                                  State Partisans                  All                      
#                                  State Partisans           Same party                      
#                                  State Partisans           Same state                      
#                                  State Partisans Same state and party    6.02      5   5.28

# - LaTeX code for the table
library(xtable)
print(xtable(all_stats), include.rownames = FALSE)


#-------------------------------------------------------------------------------
# [ B ] TABLE I2
#-------------------------------------------------------------------------------
# - some summary statistics by state and by party
sum_stats <- db02 %>%
  # - only the 13 state analyzed in the paper
  filter(state %in% c('AZ',
                      'CA',
                      'FL',
                      'IL',
                      'MA',
                      'MT',
                      'NV',
                      'NJ',
                      'NY',
                      'OH',
                      'TX',
                      'UT',
                      'VA')) %>%
  filter(!(state == "MT" & party == "republican")) %>%
  group_by(state, party) %>%
  summarize(
    all_num = round(mean(as.numeric(stateleg_mentions)), 2),
    all = paste0(
      round(mean(as.numeric(stateleg_mentions)), 2),
      " [",
      round(t.test(as.numeric(stateleg_mentions))$conf.int[1], 2),
      "-",
      round(t.test(as.numeric(stateleg_mentions))$conf.int[2], 2),
      "]"
    ),
    same_state = paste0(
      round(mean(as.numeric(stateleg_mentions_samestate)), 2),
      " [",
      round(t.test(as.numeric(stateleg_mentions_samestate))$conf.int[1], 2),
      "-",
      round(t.test(as.numeric(stateleg_mentions_samestate))$conf.int[2], 2),
      "]"
    ),
    same_state_same_party = paste0(
      round(mean(as.numeric(stateleg_mentions_samestate_sameparty)), 2),
      " [",
      round(t.test(as.numeric(stateleg_mentions_samestate_sameparty))$conf.int[1], 2),
      "-",
      round(t.test(as.numeric(stateleg_mentions_samestate_sameparty))$conf.int[2], 2),
      "]"
    )
  ) %>%
  arrange(party, desc(all_num)) %>%
  dplyr::select(-all_num) %>%
  as.data.frame()

#----------
# TABLE I2
#----------
print(sum_stats)
 # state      party                 all          same_state same_state_same_party
 #    NY   democrat 17.16 [15.79-18.54]  13.73 [12.5-14.95]    12.63 [11.5-13.77]
 #    MA   democrat 15.61 [14.38-16.85]  13.37 [12.2-14.55]   13.31 [12.14-14.48]
 #    CA   democrat 14.69 [13.55-15.82] 12.41 [11.33-13.48]    12.33 [11.26-13.4]
 #    VA   democrat  11.46 [10.5-12.42]     5.29 [4.7-5.88]      4.89 [4.35-5.44]
 #    NJ   democrat   8.75 [7.41-10.09]       6.18 [5-7.36]      2.75 [2.18-3.32]
 #    AZ   democrat    7.76 [6.75-8.76]     4.79 [3.9-5.69]       4.52 [3.64-5.4]
 #    IL   democrat    7.31 [5.64-8.98]    6.01 [4.36-7.66]       5.66 [4.02-7.3]
 #    OH   democrat    5.19 [4.49-5.89]     4.05 [3.4-4.71]      3.07 [2.49-3.64]
 #    TX   democrat    5.16 [4.65-5.67]    2.18 [1.86-2.51]      1.74 [1.47-2.02]
 #    NV   democrat    4.19 [3.77-4.61]    2.08 [1.82-2.35]      2.01 [1.75-2.26]
 #    FL   democrat    2.12 [1.75-2.48]     1.6 [1.24-1.96]       0.9 [0.72-1.09]
 #    UT   democrat     1.14 [0.97-1.3]    0.35 [0.26-0.44]      0.12 [0.07-0.16]
 #    MT   democrat     0.3 [0.22-0.38]    0.08 [0.04-0.12]       0.06 [0.03-0.1]
 #    FL republican   9.58 [8.73-10.43]    7.98 [7.19-8.78]        7.65 [6.9-8.4]
 #    TX republican     5.9 [5.22-6.59]    3.26 [2.83-3.68]      3.13 [2.71-3.54]
 #    CA republican    2.83 [2.41-3.25]    2.35 [1.94-2.75]      0.19 [0.13-0.25]
 #    NY republican     2.44 [1.99-2.9]    1.73 [1.32-2.13]       1.51 [1.12-1.9]
 #    VA republican    1.97 [1.73-2.21]    0.81 [0.66-0.96]       0.7 [0.56-0.84]
 #    IL republican     1.8 [1.54-2.06]    1.67 [1.42-1.92]      1.57 [1.33-1.81]
 #    OH republican       1.79 [1.58-2]    1.16 [1.01-1.32]       1.06 [0.92-1.2]
 #    AZ republican    1.76 [1.52-2.01]    1.21 [0.99-1.42]      0.69 [0.56-0.81]
 #    NJ republican    1.55 [1.33-1.77]    1.11 [0.94-1.28]      1.06 [0.89-1.23]
 #    UT republican    0.62 [0.49-0.76]     0.39 [0.29-0.5]      0.37 [0.27-0.47]
 #    MA republican    0.49 [0.15-0.84]    0.35 [0.02-0.69]      0.09 [0.05-0.12]
 #    NV republican    0.43 [0.33-0.53]    0.24 [0.16-0.32]       0.17 [0.1-0.24]

# - LaTeX code
library(xtable)
print(xtable(sum_stats), include.rownames = FALSE)

