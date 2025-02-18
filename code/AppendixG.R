################################################################################
# AppendixG.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Figure G1 from Appendix G of the paper.
# Data In:  
#           1. The 2018 and 2021 time series used to estimate main model
#              - ./data/group-day-issue-level-dataset-01-2018.csv
#              - ./data/group-day-issue-level-dataset-01-2021.csv
#
# Data Out:
#           Prints the following table in the console
#           1. Table G1
################################################################################

#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)
library(pals)
library(RColorBrewer)

#===============================================================================
# DATA
#===============================================================================
# - load the time series for the main model
db2018 <- read.csv("./data/group-day-issue-level-dataset-01-2018.csv")
db2021 <- read.csv("./data/group-day-issue-level-dataset-01-2021.csv")

#===============================================================================
# DATA WRANGLING
#===============================================================================
# - a list of the groups in the data
groups <- colnames(db2018)[2:6]

# de-logit the data
for (group in groups) {
  db2018[,group] <- inv.logit(db2018[,group])
  db2021[,group] <- inv.logit(db2021[,group])
}

# - separate the "IssueState" variable into 2 different ones: Issue and State
db2018$State <- as.character(sapply(db2018$IssueState, function(x)
  substring(x, nchar(x)-1, nchar(x))))
db2021$State <- as.character(sapply(db2021$IssueState, function(x)
  substring(x, nchar(x)-1, nchar(x))))

db2018$Issue <- as.character(sapply(1:nrow(db2018), function(i)
  gsub(db2018$State[i], "", db2018$IssueState[i])))
db2021$Issue <- as.character(sapply(1:nrow(db2021), function(i)
  gsub(db2021$State[i], "", db2021$IssueState[i])))


#===============================================================================
# MAIN
#===============================================================================
# - choose size of min. STEP (or STEP threshold): aournd +10-15% in attention
thresholds <- c(0.1, 0.15)

# - choose window size for checking if pre/post steps by other groups
windows <- c(1, 3, 5)

#-------------------------------------------------------------------------------
# [ A ] STEP/SPIKE DETECTION
#-------------------------------------------------------------------------------
out_a <- NULL
utopics <- unique(db2018$Issue)

# - iterate through groups and topics
for (group in groups) {
  for (topic in utopics) {
    for (thres in thresholds) {
      if (!(group %in% c("state_legislators", "state_media"))) {
        # - pull time series for this topic-group
        ts18 <- db2018[which(db2018$Issue == topic & db2018$State == "AZ"), group]
        ts21 <- db2021[which(db2021$Issue == topic & db2021$State == "AZ"), group]
      } else {
        ts18 <- db2018[which(db2018$Issue == topic), c("Date", group, "State")]
        ts21 <- db2021[which(db2021$Issue == topic), c("Date", group, "State")]
        if (group == "state_media") {
          ts18 <- ts18 %>%
            spread(State, state_media)
          ts21 <- ts21 %>%
            spread(State, state_media)
        } else {
          ts18 <- ts18 %>%
            spread(State, state_legislators)
          ts21 <- ts21 %>%
            spread(State, state_legislators)
        }
        ts18 <- rowMeans(ts18[,2:ncol(ts18)])  
        ts21 <- rowMeans(ts21[,2:ncol(ts21)])  
      }
      # - differentiate it
      ts18diff <- diff(ts18)
      ts21diff <- diff(ts21)
      # - identify steps
      steps18_inds <- which(ts18diff > thres)
      steps21_inds <- which(ts21diff > thres)
      # - count them
      steps18_n <- length(steps18_inds)
      steps21_n <- length(steps21_inds)
      new_data <- data.frame(
        group,
        topic,
        thres,
        steps18_n,
        steps21_n
      )
      # - check if any step from another group before/after: 1-day and 3-day windows
      if (steps18_n > 0) {
        for (group02 in groups) {
          if (group != group02) {
            if (!(group02 %in% c("state_legislators", "state_media"))) {
              # - pull time series for this topic-group
              ts18_02 <- db2018[which(db2018$Issue == topic & db2018$State == "AZ"), group02]
            } else {
              ts18_02 <- db2018[which(db2018$Issue == topic), c("Date", group02, "State")]
              if (group02 == "state_media") {
                ts18_02 <- ts18_02 %>%
                  spread(State, state_media)
              } else {
                ts18_02 <- ts18_02 %>%
                  spread(State, state_legislators)
              }
              ts18_02 <- rowMeans(ts18_02[,2:ncol(ts18_02)])  
            }
            # - differentiate time series for this group as well
            ts18diff_02 <- diff(ts18_02)
            for (window in windows) {
              pre18 <- 0
              same18 <- 0
              post18 <- 0
              for (step18_i in steps18_inds) {
                step18_i_prewindow <- seq(step18_i - window, step18_i - 1)
                step18_i_prewindow <- step18_i_prewindow[which(step18_i_prewindow > 0)]
                step18_i_postwindow <- seq(step18_i + 1, step18_i + window)
                step18_i_postwindow <- step18_i_postwindow[which(step18_i_postwindow < 364)]
                ts18diff_02pre <- ts18diff_02[step18_i_prewindow]
                ts18diff_02post <- ts18diff_02[step18_i_postwindow]
                ts18diff_02same <- ts18diff_02[step18_i]
                ts18diff_02pre_bin <- ifelse(
                  length(which(ts18diff_02pre > thres)), 1, 0
                )
                ts18diff_02post_bin <- ifelse(
                  length(which(ts18diff_02post > thres)), 1, 0
                )
                ts18diff_02same_bin <- ifelse(ts18diff_02same > thres, 1, 0)
                pre18 <- pre18 + ts18diff_02pre_bin
                same18 <- same18 + ts18diff_02same_bin
                post18 <- post18 + ts18diff_02post_bin
              }
              new_data[,paste0(group02, "_pre18_w", window)] <- pre18
              new_data[,paste0(group02, "_post18_w", window)] <- post18
              new_data[,paste0(group02, "_same18_w", window)] <- same18
            }
          }
        }
      }
      if (steps21_n > 0) {
        for (group02 in groups) {
          if (group != group02) {
            if (!(group02 %in% c("state_legislators", "state_media"))) {
              # - pull time series for this topic-group
              ts21_02 <- db2021[which(db2021$Issue == topic & db2021$State == "AZ"), group02]
            } else {
              ts21_02 <- db2021[which(db2021$Issue == topic), c("Date", group02, "State")]
              if (group02 == "state_media") {
                ts21_02 <- ts21_02 %>%
                  spread(State, state_media)
              } else {
                ts21_02 <- ts21_02 %>%
                  spread(State, state_legislators)
              }
              ts21_02 <- rowMeans(ts21_02[,2:ncol(ts21_02)])  
            }
            # - differentiate time series for this group as well
            ts21diff_02 <- diff(ts21_02)
            for (window in windows) {
              pre21 <- 0
              same21 <- 0
              post21 <- 0
              for (step21_i in steps21_inds) {
                step21_i_prewindow <- seq(step21_i - window, step21_i - 1)
                step21_i_prewindow <- step21_i_prewindow[which(step21_i_prewindow > 0)]
                step21_i_postwindow <- seq(step21_i + 1, step21_i + window)
                step21_i_postwindow <- step21_i_postwindow[which(step21_i_postwindow < 364)]
                ts21diff_02pre <- ts21diff_02[step21_i_prewindow]
                ts21diff_02post <- ts21diff_02[step21_i_postwindow]
                ts21diff_02same <- ts21diff_02[step21_i]
                ts21diff_02pre_bin <- ifelse(
                  length(which(ts21diff_02pre > thres)), 1, 0
                )
                ts21diff_02post_bin <- ifelse(
                  length(which(ts21diff_02post > thres)), 1, 0
                )
                ts21diff_02same_bin <- ifelse(ts21diff_02same > thres, 1, 0)
                pre21 <- pre21 + ts21diff_02pre_bin
                same21 <- same21 + ts21diff_02same_bin
                post21 <- post21 + ts21diff_02post_bin
              }
              new_data[,paste0(group02, "_pre21_w", window)] <- pre21
              new_data[,paste0(group02, "_post21_w", window)] <- post21
              new_data[,paste0(group02, "_same21_w", window)] <- same21
            }
          }
        }
      }
      out_a <- bind_rows(out_a, new_data)
    }
  }
}


#-------------------------------------------------------------------------------
# [ B ] VISUALIZE AMOUNT OF STEPS/SPIKES (by group, topic, threshold, year)
#-------------------------------------------------------------------------------
plotdb_a <- out_a %>%
  dplyr::select(group, topic, thres, steps18_n, steps21_n) %>%
  filter(thres == 0.1) %>%
  gather(year, steps, -group, -topic) %>% #, -thres) %>%
  mutate(year = ifelse(grepl("18", year), "2018", "2021")) %>%
  mutate(thres = factor(as.character(thres), levels = unique(as.character(thres))),
         year = factor(as.character(year), levels = unique(as.character(year))),
         group = recode(group,
                        `national_legislators` = "Members of Congress",
                        `national_media` = "National Media",
                        `state_legislators` = "State Legislators",
                        `state_media` = "State Media",
                        `state_partisans` = "State Partisans"),
         group = factor(group, levels = unique(group)),
         year = factor(year, levels = c("2018", "2021"))) %>%
  arrange(year, group, desc(steps)) %>%
  mutate(topic = factor(topic, levels = rev(unique(topic))))

plot_a <- ggplot(plotdb_a,
       aes(x = topic, y = steps, color = group)) +
  geom_jitter(size = 4, alpha = 0.7, width = 0.15, height = 0.15) +
  coord_flip() +
  facet_grid(~ year) +
  scale_color_manual("", values = brewer.pal(5, "Set1")) +
  scale_x_discrete("") +
  scale_y_continuous("\nNumber of spikes in issue attention (+10pp)",
                     breaks = seq(0, 100, 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "gray80"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))

#-----------
# FIGURE H1
#-----------
ggsave("./figures/figureG1.png", plot_a, width = 10, height = 6)

#-------------------------------------------------------------------------------
# [ C ] VISUALIZE "FIRST MOVER": only focus on MC --> State Legislators
#-------------------------------------------------------------------------------
plotdb_b <- out_a %>%
  filter(thres == 0.1) %>%
  dplyr::select(-thres) %>%
  gather(variable, value, -group, -topic, -steps18_n, -steps21_n) %>%
  filter(!is.na(value)) %>%
  mutate(group02 = gsub("_(pre|post|same)[0-9]{2}_w[0-9]{1}", "", variable),
         type = ifelse(grepl("pre", variable), "pre", 
                       ifelse(grepl("post", variable), "post", "same")),
         window = gsub("[^0-9]", "", gsub("(18|21)", "", variable)),
         year = ifelse(grepl("18", variable), "2018", "2021")) %>%
  group_by(group, group02, type, window, year) %>%
  summarise(res18 = round(sum(value) / sum(steps18_n), 2),
            res21 = round(sum(value) / sum(steps21_n), 2)) %>%
  as.data.frame() %>%
  gather(res_year, res, -group, -group02, -type, -window, -year) %>%
  mutate(res_year = ifelse(grepl("18", res_year), "2018", "2021")) %>%
  filter(year == res_year) %>%
  dplyr::select(-res_year) %>%
  mutate(group = recode(group,
                        `national_legislators` = "Members of Congress",
                        `national_media` = "National Media",
                        `state_legislators` = "State Legislators",
                        `state_media` = "State Media",
                        `state_partisans` = "State Partisans"),
         group02 = recode(group02,
                          `national_legislators` = "Members of Congress",
                          `national_media` = "National Media",
                          `state_legislators` = "State Legislators",
                          `state_media` = "State Media",
                          `state_partisans` = "State Partisans"),
  )

plotdb_b02 <- plotdb_b %>%
  filter(window == 3) %>%
  filter(group %in% c("State Legislators", "Members of Congress"),
         group02 %in% c("State Legislators", "Members of Congress"),) %>%
  dplyr::select(-window) %>%
  mutate(group = recode(group,
                        `Members of Congress` = "Members of\nCongress",
                        `State Legislators` = "State\nLegislators"),
         group02 = factor(as.character(group02), levels = rev(unique(group02))),
         type = factor(type, levels = c("pre", "same", "post"))) %>%
  filter(group ==  "Members of\nCongress",
         group02 == "State Legislators") %>%
  mutate(type = factor(type, levels = c("pre", "same", "post")))


plot_b <- ggplot(plotdb_b02,
       aes(x = type, y = res)) +
  geom_bar(stat = "identity", fill = "gray50") +
  geom_text(aes(y = res + 0.007, label = paste0(res * 100, "%"))) +
  facet_wrap(~ year) +
  scale_y_continuous("", breaks = seq(0, 1, 0.05), 
                     labels = paste0(seq(0, 100, 5), "%"), 
                     limits = c(0, 0.24),
                     expand = c(0,0)) +
  scale_x_discrete("\n% of spikes in issue attention by Members of Congress (+10pp)\nthat map to spikes in issue attention by State Legislators") +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))

#-----------
# FIGURE G2
#-----------
ggsave("./figures/figureG2.png", plot_b, width = 8, height = 5)
