################################################################################
# AppendixH.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Figure H1 from Appendix H of the paper.
# Data In:  
#           1. Topic predictions for weets from all groups, but partisans
#              - ./data/tweet-level-topic-preds-all-tweets-state-legislators-2018-2021.csv
#
#           2. Bill-level metadata for bills introduced to state legislatures
#              - ./data/extended_bills_df_ALL-SIMPLIFIED.csv
#
#           3. Topic predictions for these bills (titles)
#              - ./data/bills-with-topic-predictions.csv
#
# Data Out:
#           1. Figure H1
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
# - dataset with tweets from all groups but partisans (w. topic preds) 
#   N = 1,227,446
twitter_db <- read.csv("./data/tweet-level-topic-preds-all-tweets-state-legislators-2018-2021.csv",
                       colClasses = "character")

# - dataset with bill-level metadata for bills introduced to state legislatures
#   N = 90,009
bills_db <- read.csv("./data/extended_bills_df_ALL-SIMPLIFIED.csv",
                     colClasses = "character")

# - dataset with the topic predictions for these bills (titles)
#   N = 90,009
bills_preds <- read.csv("./data/bills-with-topic-predictions.csv",
                        colClasses = "character")

# - merge the two
bills_db02 <- left_join(bills_db, bills_preds)

#===============================================================================
# DATA WRANGLING
#===============================================================================
# - create "year" variable, so that we can easily keep only 2018 and 2021 data
bills_db02$year_only <- as.character(sapply(bills_db02$first_date, function(x)
  strsplit(x, split = "-")[[1]][1]))

# - only keep 2018 and 2021 data. N = 70,502
bills_db03 <- bills_db02 %>%
  filter(year_only %in% c("2018", "2021"))

#   [BILLS]
# - calculate average topic prevalence by year and state (~ prop. of bills on 
#   each topic, by state and by year)
cols_touse <- c("bill_id", "state", "year_only",
                colnames(bills_db03)[which(grepl("X", colnames(bills_db03)))])
bills_avgatt <- bills_db03[,cols_touse] %>%
  gather(topic, att, -bill_id, -state, -year_only) %>%
  mutate(att = as.numeric(as.character(att))) %>%
  group_by(state, year_only, topic) %>%
  summarise(bills_att = round(sum(att)/n(), 4))

# - provide human-readable topic labels to the dataset
bills_avgatt02 <- bills_avgatt %>%
  mutate(topic = recode(topic,
                        `X0` = "No Policy Issue",
                        `X1` = "Economy",
                        `X2` = "Civil Rights",
                        `X3` = "Healthcare",
                        `X4` = "Agriculture",
                        `X5` = "Labor",
                        `X6` = "Education",
                        `X7` = "Environment", 
                        `X8` = "Energy",
                        `X9` = "Immigration",
                        `X10` = "Transportation", 
                        `X11` = "Law and Crime", 
                        `X12` = "Social Welfare", 
                        `X13` = "Housing", 
                        `X14` = "Domestic Commerce", 
                        `X15` = "Defense", 
                        `X16` = "Technology", 
                        `X17` = "Foreign Trade", 
                        `X18` = "Intl. Affairs", 
                        `X19` = "Gov. Operations", 
                        `X20` = "Public Lands",
                        `X21` = "Gun Control"))


#   [TWITTER]
# - create a year-only variable
twitter_2018 <- twitter_db %>% # N = 595,471
  filter(grepl("-2018", day)) %>%
  mutate(year_only = "2018") %>%
  dplyr::select(-year)

twitter_2021 <- twitter_db %>% # N = 631,975
  filter(grepl("2021-", day)) %>%
  mutate(year_only = "2021") %>%
  dplyr::select(-year)

twitter_db02 <- rbind(twitter_2018, twitter_2021)

# - calculate average topic prevalence by year and state
twitter_avgatt <- twitter_db02 %>%
  dplyr::select(-party, -day, -user_id) %>%
  gather(topic, att, -tweet_id, -state, -year_only) %>%
  mutate(att = as.numeric(as.character(att))) %>%
  group_by(state, year_only, topic) %>%
  summarise(twitter_att = round(sum(att)/n(), 4)) %>%
  as.data.frame() %>%
  mutate(topic = recode(topic,
                        `X0` = "No Policy Issue",
                        `X1` = "Economy",
                        `X2` = "Civil Rights",
                        `X3` = "Healthcare",
                        `X4` = "Agriculture",
                        `X5` = "Labor",
                        `X6` = "Education",
                        `X7` = "Environment", 
                        `X8` = "Energy",
                        `X9` = "Immigration",
                        `X10` = "Transportation", 
                        `X11` = "Law and Crime", 
                        `X12` = "Social Welfare", 
                        `X13` = "Housing", 
                        `X14` = "Domestic Commerce", 
                        `X15` = "Defense", 
                        `X16` = "Technology", 
                        `X17` = "Foreign Trade", 
                        `X18` = "Intl. Affairs", 
                        `X19` = "Gov. Operations", 
                        `X20` = "Public Lands",
                        `X21` = "Gun Control"))

# - MERGE the TWITTER and BILL data
# ... 
main <- left_join(bills_avgatt02, twitter_avgatt)

# MAIN: Plot showing correlation bill v. twitter attention to each CAP
#===============================================================================
outplot <- ggplot(main,
       aes(x = bills_att, y = twitter_att, color = state)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(inherit.aes = FALSE, 
              aes(x = bills_att, y = twitter_att), se = FALSE, method = "lm",
              color = "black", size = 0.4) +
  scale_y_continuous("Prop. of TWEETS on each topic\n", expand = c(0,0.005)) +
  scale_x_continuous("\nProp. of BILLS on each topic", expand = c(0,0.005)) +
  facet_wrap(~year_only) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        #axis.line = element_line(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave("./figures/figureH1.png", outplot, width = 10, height = 5)
