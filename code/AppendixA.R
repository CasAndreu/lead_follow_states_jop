################################################################################
# AppendixA.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate the analytical tables in Appendix A, showing the 
#           performance of the BERT text CAP-topic classifiers.
# Data In:  
#           1. Datasets with information about the performance of the different
#              BERT text classifiers
#              - ./data/ml_performance/final-LEG-model-acc.csv
# Data Out:
#           Latex code for the following tables:
#           1. Table A2
#           2. Table A3
#           3. Table A4
#           4. Table A5
#           5. Table A6
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

# - load the performance datasets for the comparison RoBERTa models
media_roberta <- read.csv("./data/ml_performance/media-roberta-2024-04-03_06-08-26.csv")
pol_roberta <- read.csv("./data/ml_performance/pol-roberta-2024-04-04_06-01-42.csv")
part_roberta <- read.csv("./data/ml_performance/legfol-roberta-2024-04-04_06-32-39.csv")

# - load the performance dataset for the Politician BERT model trained on balanced data
pol_bal <- read.csv("./data/ml_performance/pol-bert-2024-07-05_10-00-02.csv")

# - load the performance dataset for the Politician SVM model
pol_svm <- read.csv("./data/ml_performance/pol-svm-2025-02-07_08-24-24.csv")


#===============================================================================
# MAIN
#===============================================================================

# [ A ] Tables A2, A3, and A4.
#-------------------------------------------------------------------------------
# - calculate 3-fold cross-validated performance for each set of BERT models 
#   (media, politician, and partisans) and combination of training data
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
  mutate(dataset = recode(
    dataset, 
    `coded_tweets-AND-ar` = "(5) set C.3 & B",
    `coded_tweets-AND-smallCAP` = "(4) Set C.3 and small A",
    `coded_tweets-AND-other-coded-tweets` = "(7) set C.3 and C.1&C.2",
    `coded_tweets-AND-fullCAP` = "(3) set C.3 and A",
    `coded_tweets-AND-small-ar` = "(6) set C.3 and small B",
    `cap` = "(1) set A",
    `leg_codedtweets` = "(2) set C.3")) %>%
  as.data.frame() %>%
  rename(`Model version` = dataset)


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
  mutate(dataset = recode(
    dataset, 
    `coded_tweets-AND-ar` = "(5) set C.1 & B",
    `coded_tweets-AND-smallCAP` = "(4) Set C.1 and small A",
    `coded_tweets-AND-other-coded-tweets` = "(7) set C.1 and C.2&C.3",
    `coded_tweets-AND-fullCAP` = "(3) set C.1 and A",
    `coded_tweets-AND-small-ar` = "(6) set C.1 and small B",
    `cap` = "(1) set A",
    `media_codedtweets` = "(2) set C.1")) %>%
  as.data.frame() %>%
  rename(`Model version` = dataset)

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
  mutate(dataset = recode(
    dataset, 
    `coded_tweets-AND-ar` = "(5) set C.2 & B",
    `coded_tweets-AND-smallCAP` = "(4) Set C.2 and small A",
    `coded_tweets-AND-other-coded-tweets` = "(7) set C.2 and C.1&C.3",
    `coded_tweets-AND-fullCAP` = "(3) set C.2 and A",
    `coded_tweets-AND-small-ar` = "(6) set C.2 and small B",
    `cap` = "(1) set A",
    `legfol_codedtweets` = "(2) set C.2")) %>%
  as.data.frame() %>%
  rename(`Model version` = dataset)

# - calculate 3-fold cross-validated performance for baseline SVM Politician model
pol_svm_cv <- pol_svm %>%
  filter(!grepl("smaller", dataset)) %>%
  filter(dataset != "") %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(`Acc. (SVM)` = round(mean(macro_val), 2),
            `Policy F1 (SVM)` = round(mean(policy_val_f1weighted), 2)) %>%
  mutate(dataset = recode(
    dataset, 
    `coded_tweets-AND-ar` = "(5) set C.3 & B",
    `coded_tweets-AND-smallCAP` = "(4) Set C.3 and small A",
    `coded_tweets-AND-other-coded-tweets` = "(7) set C.3 and C.1&C.2",
    `coded_tweets-AND-fullCAP` = "(3) set C.3 and A",
    `coded_tweets-AND-small-ar` = "(6) set C.3 and small B",
    `cap` = "(1) set A",
    `leg_codedtweets` = "(2) set C.3")) %>%
  as.data.frame() %>%
  rename(`Model version` = dataset)

# - merge the results for the Politician BERT and SVM models
pol_final <- left_join(pol_cv, pol_svm_cv)


# - print the tables

# TABLE A2
print(pol_final)
 #           Model version Acc. Policy F1 Acc. (SVM) Policy F1 (SVM)
 #         (5) set C.3 & B 0.65      0.62       0.50            0.49
 # (7) set C.3 and C.1&C.2 0.64      0.62       0.60            0.46
 # (4) Set C.3 and small A 0.66      0.62       0.56            0.30
 #       (3) set C.3 and A 0.64      0.61       0.43            0.42
 # (6) set C.3 and small B 0.66      0.61       0.58            0.33
 #               (1) set A 0.30      0.57       0.23            0.45
 #             (2) set C.3 0.64      0.54       0.54            0.23


# TABLE A3
media_cv
 #           Model version Acc. Policy F1
 #       (3) set C.1 and A 0.77      0.67
 # (7) set C.1 and C.2&C.3 0.77      0.67
 #         (5) set C.1 & B 0.77      0.64
 #               (1) set A 0.22      0.63
 # (6) set C.1 and small B 0.77      0.62
 # (4) Set C.1 and small A 0.75      0.62
 #             (2) set C.1 0.76      0.58


# TABLE A4
part_cv
 #           Model version Acc. Policy F1
 # (4) Set C.2 and small A 0.80      0.65
 # (6) set C.2 and small B 0.80      0.63
 #       (3) set C.2 and A 0.73      0.62
 #         (5) set C.2 & B 0.78      0.61
 # (7) set C.2 and C.1&C.3 0.79      0.61
 #             (2) set C.2 0.79      0.60
 #               (1) set A 0.13      0.49


# [ B ] Table A5
#-------------------------------------------------------------------------------
# - calculate 3-fold cross-validation performance for RoBERTa models
pol_cv_new <- pol_roberta %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(new_accuracy = round(mean(macro_val), 2),
            new_accuracy_policy = round(mean(policy_val), 2),
            new_f1_policy_weighted = round(mean(policy_val_f1weighted), 2)) %>%
  arrange(desc(new_accuracy_policy)) %>%
  dplyr::select(-new_accuracy_policy, -dataset) %>%
  head(1) %>%
  rename(`Accuracy` = new_accuracy,
         `PolicyF1` = new_f1_policy_weighted) %>%
  mutate(model = "RoBERTa",
         Group = "Politician")

media_cv_new <- media_roberta %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(new_accuracy = round(mean(macro_val), 2),
            new_accuracy_policy = round(mean(policy_val), 2),
            new_f1_policy_weighted = round(mean(policy_val_f1weighted), 2)) %>%
  arrange(desc(new_accuracy_policy)) %>%
  dplyr::select(-new_accuracy_policy, -dataset) %>%
  head(1) %>%
  rename(`Accuracy` = new_accuracy,
         `PolicyF1` = new_f1_policy_weighted) %>%
  mutate(model = "RoBERTa",
         Group = "Media")

part_cv_new <- part_roberta %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(new_accuracy = round(mean(macro_val), 2),
            new_accuracy_policy = round(mean(policy_val), 2),
            new_f1_policy_weighted = round(mean(policy_val_f1weighted), 2)) %>%
  arrange(desc(new_accuracy_policy)) %>%
  dplyr::select(-new_accuracy_policy, -dataset) %>%
  head(1) %>%
  rename(`Accuracy` = new_accuracy,
         `PolicyF1` = new_f1_policy_weighted) %>%
  mutate(model = "RoBERTa",
         Group = "Partisans")

roberta_summary <- rbind(pol_cv_new, media_cv_new, part_cv_new)

# - prepare BERT info to be merged
bert_summary <- rbind(
  pol_cv %>% 
    head(1) %>%
    dplyr::select(-`Model version`) %>%
    rename(Accuracy = `Acc.`,
           PolicyF1 = `Policy F1`) %>%
    mutate(model = "BERT", 
           Group = "Politician"),
  media_cv %>% 
    head(1) %>%
    dplyr::select(-`Model version`) %>%
    rename(Accuracy = `Acc.`,
           PolicyF1 = `Policy F1`) %>%
    mutate(model = "BERT", 
           Group = "Media"),
  part_cv %>% 
    head(1) %>%
    dplyr::select(-`Model version`) %>%
    rename(Accuracy = `Acc.`,
           PolicyF1 = `Policy F1`) %>%
    mutate(model = "BERT", 
           Group = "Partisans")
)

# - merge performance of BERT and RoBERTa models
both_perf <- rbind(bert_summary, roberta_summary) %>%
  gather(Statistic, value, -model, -Group) %>%
  spread(model, value) %>%
  as.data.frame() %>%
  dplyr::select(Statistic, Group, BERT, RoBERTa) %>%
  mutate(Group = factor(as.character(Group), levels = c(
    "Politician", "Media", "Partisans"
  ))) %>%
  arrange(Group)
  
# TABLE A5
both_perf
 # Statistic      Group BERT RoBERTa
 #  Accuracy Politician 0.64    0.70
 #  PolicyF1 Politician 0.62    0.66
 #  Accuracy      Media 0.77    0.77
 #  PolicyF1      Media 0.67    0.70
 #  Accuracy  Partisans 0.80    0.81
 #  PolicyF1  Partisans 0.65    0.69


# [ C ] Table A6
#-------------------------------------------------------------------------------
# - calculate 3-fold cross-validation performance of a balanced BERT Politician
#   classifier
pol_cv_bal <- pol_bal %>%
  filter(!grepl("smaller", dataset)) %>%
  filter(dataset != "") %>%
  group_by(fold, dataset) %>%
  arrange(desc(policy_val)) %>%
  do(head(., n=1)) %>%
  as.data.frame() %>%
  group_by(dataset) %>%
  summarise(`Acc. (Balanced)` = round(mean(macro_val), 2),
            `Policy F1 (Balanced)` = round(mean(policy_val_f1weighted), 2)) %>%
  as.data.frame() %>%
  mutate(dataset = recode(
  dataset, 
  `coded_tweets-AND-ar` = "(5) set C.3 & B",
  `coded_tweets-AND-smallCAP` = "(4) Set C.3 and small A",
  `coded_tweets-AND-other-coded-tweets` = "(7) set C.3 and C.1&C.2",
  `coded_tweets-AND-fullCAP` = "(3) set C.3 and A",
  `coded_tweets-AND-small-ar` = "(6) set C.3 and small B",
  `cap` = "(1) set A",
  `leg_codedtweets` = "(2) set C.3")) %>%
  rename(`Model version` = dataset)

# - merge with the info about performance of original Politician BERT 
pol_cv_tomerge <- pol_cv %>%
  rename(`Acc. (Original)` = `Acc.`,
         `Policy F1 (Original)` = `Policy F1`)

tab_a6 <- left_join(pol_cv_tomerge, pol_cv_bal)

# TABLE 6
print(tab_a6)
 #            Model version Acc. (Original) Policy F1 (Original) Acc. (Balanced) Policy F1 (Balanced)
 #          (5) set C.3 & B            0.65                 0.62            0.32                 0.57
 #  (7) set C.3 and C.1&C.2            0.64                 0.62            0.52                 0.53
 #  (4) Set C.3 and small A            0.66                 0.62            0.57                 0.53
 #        (3) set C.3 and A            0.64                 0.61            0.38                 0.58
 #  (6) set C.3 and small B            0.66                 0.61            0.47                 0.46
 #                (1) set A            0.30                 0.57            0.30                 0.56
 #              (2) set C.3            0.64                 0.54            0.57                 0.37  
