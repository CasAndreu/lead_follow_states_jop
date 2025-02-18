################################################################################
# AppendixF.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  This script fits independent models for each issue. This provides the
#           basis for the analyses presented in Appendix F. It then generates
#           Figure F1.
# Note:     Models were fit on NYU's HPC cluster. Instances used 40 CPU cores
#           and 100 GB of memory. With this setup, fitting both models took
#           half a day. In this script, we comment out the portion where we fit 
#           the models and instead load the fitted model objects from disk.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study. MoCs were differentiated by the state they, as in
#           the analyses for Figures 3 and 4.
#           are associated with.
#           /data/group-day-issue-level-dataset-04-2018.csv
#           /data/group-day-issue-level-dataset-04-2021.csv
# Data Out:
#           1. Two list objects, containing the estimated models and IRFs
#              for each issue.
#              /models/MODEL_by_issue-2018.Rdata
#              /models/MODEL_by_issue-2021.Rdata
#              
#           2. Figure F1 of the Appendix:
#              - ./figures/figureF1.png
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(vars)
library(boot)
library(stringr)
library(ggplot2)
library(ggh4x)
library(cowplot)


#===============================================================================
# FITTING MODELS
#===============================================================================


#for(year in c("2018", "2021")){
#  cat(year, "\n")
#  load_file = paste0("./data/group-day-issue-level-dataset-04-",year,".csv")
#  
#  maindb <- read.csv(load_file)
#  
#  # Sort order:
#  maindb = maindb[order(maindb$IssueState, maindb$Date),]
#  
#  #-------------------------------------------------------------------------------
#  # MAIN
#  #-------------------------------------------------------------------------------
#  # Define lags, number of bootstraps, and set seed
#  p <- 5
#  runs <- 300
#  seed <- 78223
#  set.seed(seed)
#  
#  # Generate an issue domain variable 
#  if(year == "2021"){
#    variables <- c("national_legislators", "national_media", "state_legislators", 
#                   "state_media", "state_partisans", "state_random_partisans", "Trump", "IssueState")
#  }
#  if(year == "2018"){
#    variables <- c("national_legislators", "national_media", "state_legislators", 
#                   "state_media", "state_partisans", "Trump", "IssueState")
#  }
#  
#  # Run three models for the different types of issues
#  issue_results <- list()
#  unique_issues <- unique(str_remove(maindb$IssueState, "[A-Z]{2}$"))
#  
#  for(issue_i in unique_issues){
#    cat(issue_i, "\n")
#    
#    # Subset the data by state group
#    issue_db <- maindb[which(str_remove(maindb$IssueState, "[A-Z]{2}$") == issue_i),]
#    
#    mean_vec = c()
#    for(group_i in variables[1:7]){
#      issue_results[[issue_i]][[group_i]] = exp(mean(issue_db[[group_i]]))
#    }
#    
#    
#    # Generate formula object
#    mformula <- formula(paste0("~", paste0(variables, collapse = " + ")))
#    model_data <- model.matrix(mformula, issue_db[, variables])
#    model_data <- model_data[, 2:ncol(model_data)] # removing intercept
#    
#    # Splitting the covariates of interest from the issue dummy variables
#    X_endogenous <- model_data[, variables[which(!variables %in% c("IssueState"))]]
#    X_exogenous <- model_data[
#      , which(!colnames(model_data) %in% 
#                variables[which(!variables %in% c("IssueState"))])]
#    
#    # Run the VAR model and estimate IRFs
#    var_model_merged <- VAR(y = X_endogenous, p = p, exogen = X_exogenous)
#    var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 15, ortho = F,
#                               cumulative = TRUE, runs = runs)
#    
#    # Store results in list in list
#    issue_results[[issue_i]][["IRF_res"]] = var_irfs_cum_merged
#  }
#  
#  # Save the estimated VAR model and IRFs.
#  object_name <- paste0("./models/MODEL_by_issue-", year, ".Rdata")
#  save(issue_results, file = object_name)
#}



#===============================================================================
# VISUALIZE THE MODELS IN FIGURE G1 USING THE IRFs
#===============================================================================

# Load utils & define which effects to show
source(paste0("./code/00-functions.R"))

which_effects <- "structural"
plot_db <- data.frame()

for(year in c("2018", "2021")){
  ##### Load
  cat(year, "\n")
  object_name <- paste0("./models/MODEL_by_issue-", year, ".Rdata")
  print(load(object_name))
  for(issue_i in names(issue_results)){
    cat(issue_i, "\n")
    results <- transform_var_irf(var_irfs = issue_results[[issue_i]]$IRF_res, days = 5)
    plot_db_i <- make_plot_data(final_input = results, 
                                which_day = 5,
                                which_effects = which_effects, column_group = c("state_legislators"))
    plot_db_i$issue <- issue_i
    plot_db_i$Year <- year
    for(group_i in names(issue_results[[issue_i]])[1:5]){
      plot_db_i[[group_i]] = issue_results[[issue_i]][[group_i]]
    }
    plot_db <- rbind(plot_db, plot_db_i)
  }
}


# Generate Figure G1
p1 <- ggplot(subset(plot_db, plot_db$rowgroup == "Members of\nCongress" & plot_db$direction == "State legislators' response")) +
  geom_point(aes(x = national_legislators*100, y = pe,
                 col = Year)) +
  facet_nested(.~"State legislators' response to Members of Congress") +
  stat_smooth(aes(x = national_legislators*100, y = pe, group = Year, col = Year),
              method = "lm", se = F
              #fill = "grey90", alpha = .5
              ) +
    xlab("Issue prevalence in percent among\nMembers of Congress") +
  ylab("5-day effect of a permanent 10\npercentage point increase") +
  scale_color_manual("Year:", 
                     breaks = c("2021", "2018"),
                     values = c("gray80", "gray40")) +
  
  guides(col = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black")
  )

# Generate Figure G1
p2<- ggplot(subset(plot_db, plot_db$rowgroup == "Members of\nCongress" & plot_db$direction == "Row groups' response")) +
  geom_point(aes(x = national_legislators*100, y = pe,
                 col = Year)) +
  facet_nested(.~"Members of Congress' response to State Legislators") +
  stat_smooth(aes(x = national_legislators*100, y = pe, group = Year, col = Year),
              method = "lm", se = F
              #fill = "grey90", alpha = .5
  ) +
  xlab("Issue prevalence in percent among\nMembers of Congress") +
  ylab("5-day effect of a permanent 10\npercentage point increase") +
  scale_color_manual("Year:", 
                     breaks = c("2021", "2018"),
                     values = c("gray80", "gray40")) +
  guides(col = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black")
  )

p3<- ggplot(subset(plot_db, plot_db$rowgroup == "Members of\nCongress" & plot_db$direction == "State legislators' response")) +
  geom_point(aes(x = state_legislators*100, y = pe,
                 col = Year)) +
  facet_nested(.~"State legislators' response to Members of Congress") +
  stat_smooth(aes(x = state_legislators*100, y = pe, group = Year, col = Year),
              method = "lm", se = F
              #fill = "grey90", alpha = .5
  ) +
  xlab("Issue prevalence in percent among\nState Legislators") +
  ylab("5-day effect of a permanent 10\npercentage point increase") +
  scale_color_manual("Year:", 
                     breaks = c("2021", "2018"),
                     values = c("gray80", "gray40")) +
  guides(col = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black")
  )

# Generate Figure G1
p4 <- ggplot(subset(plot_db, plot_db$rowgroup == "Members of\nCongress" & plot_db$direction == "Row groups' response")) +
  geom_point(aes(x = state_legislators*100, y = pe,
                 col = Year)) +
  facet_nested(.~"Members of Congress' response to State Legislators") +
  stat_smooth(aes(x = state_legislators*100, y = pe, group = Year, col = Year),
              method = "lm", se = F
  ) +
  xlab("Issue prevalence in percent among\nState Legislators") +
  ylab("5-day effect of a permanent 10\npercentage point increase") +
  scale_color_manual("Year:", 
                     breaks = c("2021", "2018"),
                     values = c("gray80", "gray40")) +
  guides(col = guide_legend(reverse = T)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black")
  )

by_plots<- plot_grid(p1 + theme(legend.position = "none",
                     plot.margin = margin(.75,.38,.1,.1, "cm"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank()), 
          p3 + theme(legend.position = "none",
                     plot.margin = margin(.75,.38,.1,.1, "cm"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.y = element_blank()),
          p2 + theme(legend.position = "none",
                     plot.margin = margin(.75,.38,.1,.1, "cm")),
          p4 + theme(legend.position = "none",
                     plot.margin = margin(.75,.38,.1,.1, "cm"),
                     axis.title.y = element_blank(),
                     axis.text.y = element_blank()),
          rel_heights = c(1,1.2), rel_widths = c(1.13,1),
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

by_issue_p<- plot_grid(by_plots,
          get_legend(p4),
          nrow = 2, 
          rel_heights = c(1,.1))


#-----------
# FIGURE F1
#-----------
ggsave("./figures/figureF1.png", by_issue_p, width = 7.5, height = 7.5)
