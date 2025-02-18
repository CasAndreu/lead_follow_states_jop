################################################################################
# 01-table02.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Table 2 of the paper, showing the performance of the
#           three BERT text classifiers.
# Data In:  
#           1. Datasets with information about the performance of the different
#              BERT text classifiers
#              - ./data/ml_performance/media-bert-2022-02-16_06-39-57.csv"
#              - ./data/ml_performance/pol-bert-2022-02-15_05-26-26.csv"
#              - ./data/ml_performance/legfol-bert-2022-02-15_05-10-20.csv
# Data Out:
#           LaTeX code for the following table:
#           1. Table 2
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)

#===============================================================================
# DATA
#===============================================================================
# - load the datasets with info about performance of the BERT model used to 
#   predict the topics in tweets from media, politicians, and members of the 
#   public.
media_perf <- read.csv("./data/ml_performance/media-bert-2022-02-16_06-39-57.csv")
pol_perf <- read.csv("./data/ml_performance/pol-bert-2022-02-15_05-26-26.csv")
part_perf <- read.csv("./data/ml_performance/legfol-bert-2022-02-15_05-10-20.csv")

# - load dataset
train_db <- read.csv("./data/annotated-tweets-no-text-no-author.csv",
                     colClasses = "character")

#===============================================================================
# MAIN
#===============================================================================
# - calculate 3-fold cross-validation for each model and training set up, and pull
#   the data for the best performing model
pol_cv <- pol_perf %>%
  filter(!grepl("smaller", dataset)) %>%
  filter(dataset != "") %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(`Acc.` = round(mean(macro_val), 2),
            `Policy F1` = round(mean(policy_val_f1weighted), 2)) %>%
  arrange(desc(`Policy F1`)) %>%
  head(1) %>%
  rename(Model = dataset) %>%
  mutate(Model = "Politicians BERT")

media_cv <- media_perf %>%
  filter(!grepl("smaller", dataset)) %>%
  filter(dataset != "") %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(`Acc.` = round(mean(macro_val), 2),
            `Policy F1` = round(mean(policy_val_f1weighted), 2)) %>%
  arrange(desc(`Policy F1`)) %>%
  head(1) %>%
  rename(Model = dataset) %>%
  mutate(Model = "Media BERT")

part_cv <- part_perf %>%
  filter(!grepl("smaller", dataset)) %>%
  filter(dataset != "") %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(`Acc.` = round(mean(macro_val), 2),
            `Policy F1` = round(mean(policy_val_f1weighted), 2)) %>%
  arrange(desc(`Policy F1`)) %>%
  head(1) %>%
  rename(Model = dataset) %>%
  mutate(Model = "Partisans BERT")
  

# - calculate max class proportion in our own annotated tweets from media,
#   politicians, and partisans.
max_props <- train_db %>%
  group_by(dataset) %>%
  mutate(total = n()) %>%
  filter(major != "0") %>%
  as.data.frame() %>%
  group_by(dataset, major) %>%
  summarise(topic_n = n(), total = total[1]) %>%
  mutate(topic_prop = round(topic_n / total, 2)) %>%
  arrange(desc(topic_prop)) %>%
  do(head(., n=1)) %>%
  dplyr::select(dataset, topic_prop) %>%
  rename(Model = dataset, `Max. Class Prop.` = topic_prop) %>%
  mutate(Model = recode(Model,
                        `legislators` = "Politicians BERT",
                        `media` = "Media BERT",
                        `leg_follower` = "Partisans BERT"))

# - merge information about performance of the 3 classifiers, and the max policy
#   class for each of them.
table2 <- left_join(rbind(pol_cv, media_cv, part_cv), max_props) %>%
  dplyr::select(Model, `Max. Class Prop.`, `Acc.`, `Policy F1`)


#===============================================================================
# OUTPUT
#===============================================================================
# Table 2 of the paper
print(table2)
# Model            `Max. Class Prop.`  Acc. `Policy F1`
# Politicians BERT               0.13  0.65        0.62
# Media BERT                     0.06  0.77        0.67
# Partisans BERT                 0.06  0.8         0.65
