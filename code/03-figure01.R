################################################################################
# 02-figure01.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Figure 1 of the paper, showing daily attention to 2
#           topics (Immigration & Defense) by 4 groups under study — Democrats 
#           and Republicans in Congress; and Democrat and Republican state 
#           legislators.
# Out File: ./figures/figure03.png
# Data In:  Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study.
#           ./data/group-day-issue-level-dataset-02-2018.csv"
#           ./data/group-day-issue-level-dataset-02-2021.csv"
# Data Out:
#           - ./figures/figure01.png
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(zoo)


#===============================================================================
# DATA
#===============================================================================
# Load the dataset in which we have group-day-issue-level information about how 
# much attention each group of analysis paid to each day to  each topic we study. 
year <- "2018"
load_file <- paste0("./data/group-day-issue-level-dataset-02-", year, ".csv")
db_2018 <- read.csv(load_file)

year<- "2021"
load_file<- paste0("./data/group-day-issue-level-dataset-02-", year, ".csv")
db_2021 <- read.csv(load_file)


#===============================================================================
# MAIN
#===============================================================================

#-------------------------------------------------------------------------------
# Generate Panel A on immigration (Top)
#-------------------------------------------------------------------------------
# Gather the data
db_gathered <- gather(db_2018,Group,Value, -Issue, -Date, -state)
db_gathered <- db_gathered %>% 
  group_by(Date, Issue, Group) %>% 
  summarise(Value = mean(Value))

# Get the subset of politicians and Immigration time series
immigration_subset <- subset(
  db_gathered, db_gathered$Issue == "Immigration" &
    Group %in% c(
      "national_legislators_republican", "national_legislators_democrat",
      "state_legislators_democrat", "state_legislators_republican"))

# Transform Date variable
immigration_subset$Date <- as.Date(immigration_subset$Date, 
                                   origin = "1970-01-01")

# Make an event dataframe to annotate the plot
events <- data.frame(
  time = c(as.Date("2018-06-20","%Y-%m-%d"),
           as.Date("2018-12-21","%Y-%m-%d"),
           as.Date("2018-01-13","%Y-%m-%d")),
  text = c("Trump signs executive order\non family separation",
           "Government\nshutdown over\nborder wall",
           "Public focus on\n immigration reform,\nTrump makes insulting\nremarks about a\nseries of countries"),
  height = c(26, 20.8,21.2))


# Generate a 5-day rolling average variable
immigration_subset <- immigration_subset %>% 
  group_by(Group) %>% 
  mutate(roll_mean = zoo::rollmean(Value, 5, na.pad = T))

# Rename the time series
immigration_subset$Group <- recode(
  as.factor(immigration_subset$Group),
  `national_legislators_democrat` = "Democrats in Congress",
  `national_legislators_republican` = "Republicans in Congress",
  `state_legislators_democrat` = "Democrat State Legislators",
  `state_legislators_republican` = "Republican State Legislators")
immigration_subset = subset(immigration_subset, !is.na(immigration_subset$roll_mean))


# Create Panel A of Figure 3
immi <- ggplot(data = immigration_subset) +
  geom_line(aes(x = Date, y = roll_mean*100, colour = Group,  group = Group, size = Group)) +
  scale_size_manual(values = c(1.5,1.5,1,1)) +
  geom_label(data = events, mapping = aes(label = text, x = time, y = height), 
             angle = 0,  size = 3.75) +
  scale_colour_manual("Group:", 
                      values = c(`Democrats in Congress` = "blue", 
                                 `Republicans in Congress`  = "red",
                                 `Democrat State Legislators`  = "cornflowerblue", 
                                 `Republican State Legislators`  = "darksalmon"
                      )) +
  ylim(0,27.2) +
  ylab("Attention to immigration in %") +
  theme_bw() +
  guides(size = "none") +
  guides(colour = guide_legend(override.aes = list(size = c(3,3,2,2)))) +
  theme(
    axis.line = element_line(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 9)
  )


#-------------------------------------------------------------------------------
# Generate Panel B on defense (Bottom)
#-------------------------------------------------------------------------------
# Gather the data into 
db_gathered <- gather(db_2021,Group,Value, -Issue, -Date, -state)
db_gathered <- db_gathered %>% 
  group_by(Date, Issue, Group) %>% 
  summarise(Value = mean(Value))

# Get the subset of politicians and defense time series
defense_subset <- subset(
  db_gathered, db_gathered$Issue == "Defense" &
    Group %in% c(
      "national_legislators_republican", "national_legislators_democrat",
      "state_legislators_democrat", "state_legislators_republican"))

# Transform Date variable
defense_subset$Date <- as.Date(defense_subset$Date, 
                                   origin = "1970-01-01")

# Make an event dataframe to annotate the plot
events <- data.frame(
  time = c(as.Date("2021-05-31","%Y-%m-%d"),
           as.Date("2021-11-11","%Y-%m-%d"),
           as.Date("2021-08-24","%Y-%m-%d")),
  text = c("Memorial Day",
           "Veterans Day",
           "Taliban takeover Afghanistan"),
  height = c(25.8,17.8,29.2))

# Generate a 5-day rolling average variable
defense_subset <- defense_subset %>% 
  group_by(Group) %>% 
  mutate(roll_mean = zoo::rollmean(Value, 5, na.pad = T))

# Rename the time series
defense_subset$Group <- recode(
  as.factor(defense_subset$Group),
  `national_legislators_democrat` = "Democrats in Congress",
  `national_legislators_republican` = "Republicans in Congress",
  `state_legislators_democrat` = "Democrat State Legislators",
  `state_legislators_republican` = "Republican State Legislators")
defense_subset = subset(defense_subset, !is.na(defense_subset$roll_mean))


# Create Panel B of Figure 3
defense <- ggplot(data = defense_subset) +
  geom_line(aes(x = Date, y = roll_mean*100, colour = Group,  group = Group, size = Group)) +
  scale_size_manual(values = c(1.5,1.5,1,1)) +
  geom_label(data = events, mapping = aes(label = text, x = time, y = height), 
             angle = 0,  size = 3.75) +
  scale_colour_manual("Group:", 
                      values = c(`Democrats in Congress` = "blue", 
                                 `Republicans in Congress`  = "red",
                                 `Democrat State Legislators`  = "cornflowerblue", 
                                 `Republican State Legislators`  = "darksalmon"
                      )) +
  ylab("Attention to defense in %") +
  theme_bw() +
  ylim(0,30.5)+
  guides(size = "none") +
  guides(colour = guide_legend(override.aes = list(size = c(3,3,2,2)))) +
  theme(
    axis.line = element_line(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 10),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 9)
  )


#===============================================================================
# OUTPUT
#===============================================================================
# Combine Panel A and Panel B to make Figure 3
plot_grid(immi  + theme(legend.position = "none"), 
          defense + theme(legend.position = "bottom",
                        legend.text = element_text(size = 15)), 
          ncol = 1, align = "v",
          rel_heights = c(1,1), labels = c("A", "B"))
ggsave(paste0(root_dir, "/figures/figure01.png"), width = 14, height = 7, dpi = 300)

