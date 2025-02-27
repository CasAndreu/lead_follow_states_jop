################################################################################
# AppendixA_02.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate tables A7-A11 in Appendix A. Table A7 is also Table 3
#           in the main paper. We show how to produce one of these (e.g. Table A7).
#           We follow the same code for producing the remaining ones, by simply
#           changing the path to where the .csv with the tweets for that group
#           of analysis is located. 
# Data In:  
#           REPLICATION DATA NOT PROVIDED. The input data for these scripts are
#           large number of csv files with the original text of millions of tweets.
#           Due to Twitter's Terms of Serivce, we are not allowed to publicly
#           share the text of these tweets. 
# Data Out:
#           - Tables A7-A11 (A7 is also Table 3 in the main paper)
################################################################################


# PACAKGES
#===============================================================================
library(dplyr)
library(xtable)

# DATA PATH & CONSTANTS
#===============================================================================
# - replace with the data path where the .csv with the raw text from the tweets
#   for each group of analysis are located.
data_path <- "<REPLACE-WITH-CORRECT-DATA-PATH>"
data_files <- paste0(data_path, list.files(data_path))

# - a list of stopwords to ignore. Either uninformative, or they appear across
#   topics
stopw <- c(
  "", "-", "the", "a", "…", "in", "to", "of", "and", "for", "is", "on",
  "at", "that", "with", "was", "has", "his", "an", "after", "this", "he",
  "rt", "as", "from", "who", "by", "it", "her", "are", "have", "i", "you", 
  "be", "nt", "s", "its", "she", "not", "they", "what", "been", "about",
  "when", "will", "”", "do", "their", "alief", "were", "but", "just", "out", 
  "two", "years", "we", "had", "up", "us", "via", "more", "said", "there", 
  "into", "how", "being", "no", "off", "than", "can", "him", "if", "our",
  "my", "your",  "__year__", "m", "tx", "so", "thehill", "am",  "great", "w",
  "today", "de", "\U0001f1f2", "\U0001f1fd","right", "now", "ve", "get",
  "all", "one", "thank", "day", "or", "make", "should",  "here", "__link__",
  "__num__", "trump", "biden", "president"
)

# MAIN
#===============================================================================
# - a list of unique topics to analyze
first_file <- read.csv(data_files[1], colClasses = "character")
topics <- unique(as.character(first_file$top_topic))

# - initialize main empty output dataframe
main_out <- list()

# - iterate through csv files for this group
for (dfile in data_files) {
  # - initialize empty output dataframe for this file
  outdb <- NULL
  
  # - load the csv with tweet text
  db <- read.csv(dfile, colClasses = "character")
  
  # - calculate in general how often they use all words
  main_wordfreq <- data.frame(as.data.frame(table(strsplit(
    paste0(db$clean_text_cnn, collapse = " "), 
    split = " "))))
  
  names(main_wordfreq) <- c("feature", "freq_main")
  main_wordfreq <- main_wordfreq %>%
    # - remove empty spaces and stopwords
    filter(!(as.character(feature) %in% as.character(stopw)))
  # - save the info into the main data object keeping track of term frequencies
  if (length(main_out) == 0) {
    main_out[["main"]] <- main_wordfreq
  } else {
    main_wordfreq <- main_wordfreq %>%
      rename(freq_main_NEW = freq_main)
    main_out[["main"]] <- full_join(main_out[["main"]], main_wordfreq) %>%
      mutate(freq_main = ifelse(is.na(freq_main), 0, freq_main),
             freq_main_NEW = ifelse(is.na(freq_main_NEW), 0 , freq_main_NEW),
             freq_main = freq_main + freq_main_NEW) %>%
      dplyr::select(-freq_main_NEW)
  }
  
  # - iterate through topics
  for (topic in topics) {
    group_topic <- db %>%
      filter(top_topic == topic)
    if (nrow(group_topic) > 0) {
      topic_wordfreq <- data.frame(as.data.frame(table(strsplit(
        paste0(group_topic$clean_text_cnn, collapse = " "), 
        split = " "))))
      names(topic_wordfreq) <- c("feature", "freq_topic")
      if (!(topic %in% names(main_out))) {
        main_out[[topic]] <- topic_wordfreq
      } else {
        topic_wordfreq <- topic_wordfreq %>%
          rename(freq_topic_NEW = freq_topic)
        main_out[[topic]] <- full_join(main_out[[topic]], topic_wordfreq) %>%
          mutate(freq_topic = ifelse(is.na(freq_topic), 0, freq_topic),
                 freq_topic_NEW = ifelse(is.na(freq_topic_NEW), 0 , freq_topic_NEW),
                 freq_topic = freq_topic + freq_topic_NEW) %>%
          dplyr::select(-freq_topic_NEW)
      }
    }
  }
}

# - pull main word frequencies
main_wordfreq <- main_out[["main"]]

# - iterate through topics, merge topic word frequencies with main/overall word
#   frequencies, calculate overall and topic prevalence for each word, and pull
#   the top 15 most characteristic of each topic
outdb <- NULL
for (topic in topics) {
  topic_wordfreq <- main_out[[topic]] %>%
    # - remove empty spaces and stopwords
    filter(!(as.character(feature) %in% as.character(stopw)))
  # - merge feature frequencies for all tweets and only tweets on this topic
  main_wordfreq$feature <- as.character(main_wordfreq$feature)
  topic_wordfreq$feature <- as.character(topic_wordfreq$feature)
  both_freq <- left_join(topic_wordfreq, main_wordfreq)
  top_features <-  both_freq %>%
    mutate(topic_prop = round(freq_topic / sum(both_freq$freq_topic), 4),
           main_prop = round(freq_main / sum(both_freq$freq_main), 4),
           # - take the difference to compare
           topic_diff = topic_prop - main_prop) %>%
    arrange(desc(topic_diff)) %>%
    head(15)
  top_features_str <- paste0(top_features$feature, collapse = ", ")
  # - save info to out-of-the-loop db
  new_row <- data.frame(
    topic = topic,
    top_features = top_features_str
  )
  outdb <- rbind(outdb, new_row)
}

# - sort the topics 
outdb <- outdb %>%
  mutate(topic = factor(as.character(topic),
                        levels = c(
                          "No policy issue", 
                          "Economy",
                          "Civil Rights",
                          "Healthcare",
                          "Agriculture",
                          "Labor",
                          "Education",
                          "Environment", 
                          "Energy",
                          "Immigration",
                          "Transportation",
                          "Law and Crime", 
                          "Social Welfare",
                          "Housing",
                          "Domestic Commerce",
                          "Defense",
                          "Technology",
                          "Foreign Trade", 
                          "Intl. Affairs",
                          "Gov. Operations",
                          "Public Lands", 
                          "Gun Control"
                        ))) %>%
  arrange(topic)

# OUTPUT
#===============================================================================
print(xtable(outdb), include.rownames = FALSE)
