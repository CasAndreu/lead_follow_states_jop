################################################################################
# 05-figure04.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  This script fits independent models for state and national issues.
#           These results are reported in section 5.4 of the paper.
#           It then generates Figure 4 of the paper
# Note:     Models were fit on NYU's HPC cluster. Instances used 40 CPU cores
#           and 100 GB of memory. With this setup, fitting both models took
#           half a day. In this script, we comment out the portion where we fit 
#           the models and instead load the fitted model objects from disk.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study. As described in the paper, these analyses also use
#           the data where MoCs are differentiated by state.
#           /data/group-day-issue-level-dataset-04-2018.csv
#           /data/group-day-issue-level-dataset-04-2021.csv

# Data Out:
#           1. Two list objects, each containing the estimated model together
#              with the IRFs for each year.
#              /models/MODEL_ISSUE_OWN_2018.Rdata"
#              /models/MODEL_ISSUE_OWN_2021.Rdata"
#              
#           2. Figure 4 of the main paper:
#              - ./figures/figure04.png
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

#===============================================================================
# FITTING MODELS
#===============================================================================

#for(year in c("2018", "2021")){
#  cat(year, "\n")
#  load_file<- paste0("./data/group-day-issue-level-dataset-04-", year, ".csv")
#  
#  maindb <- read.csv(load_file)
#  
#  # Sort order:
#  maindb <- maindb[order(maindb$IssueState, maindb$Date),]
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
#  maindb$issue_group <- recode(str_extract(maindb$IssueState, ".+(?=[A-Z]{2})"),
#                               `Law and Crime` = "State owned",
#                               `Healthcare` = "State owned",
#                               `Education` = "State owned",
#                               `Transportation` = "State owned",
#                               `Labor` = "State owned",
#                               `Social Welfare` = "State owned",
#                               `Housing` = "State owned",
#                               `Gun Control` = "State owned",
#                               
#                               `Domestic Commerce` = "Federally owned",
#                               `Defense` = "Federally owned",
#                               `Intl. Affairs` = "Federally owned",
#                               `Technology` = "Federally owned",
#                               `Foreign Trade` = "Federally owned",
#                               
#                               `Gov. Operations` = "Both",
#                               `Economy` = "Both",
#                               `Civil Rights` = "Both",
#                               `Immigration` = "Both",
#                               `Environment` = "Both",
#                               `Public Lands` = "Both",
#                               `Energy` = "Both",
#                               `Agriculture` = "Both"
#  )
#  
#  # Run two models for the different types of issues
#  issue_group_results <- list()
#  for(which_issue_group in c("State owned", "Federally owned")){
#    cat(which_issue_group, "\n")
#    
#    # Subset the data by issue group
#    issue_group_db <- maindb[which(maindb$issue_group == which_issue_group),]
#    
#    # Generate formula object
#    mformula <- formula(paste0("~", paste0(variables, collapse = " + ")))
#    model_data <- model.matrix(mformula, issue_group_db[, variables])
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
#    issue_group_results[[which_issue_group]][["VAR_res"]] = var_model_merged
#    issue_group_results[[which_issue_group]][["IRF_res"]] = var_irfs_cum_merged
#  }
#  
#  # Save the estimated VAR model and IRFs.
#  # Un-comment the two lines below if you want to write out the models. 
#  object_name = paste0("./models/MODEL_ISSUE_OWN_", year, ".Rdata")
#  save(issue_group_results, file = object_name)
#}



#===============================================================================
# VISUALIZE THE MODELS IN FIGURE 4
#===============================================================================

# Load the figure code
source(paste0("./code/00-functions.R"))

# Define which effects to show
which_effects <- "structural"

##### 2018
year <- "2018"
object_name <- paste0("./models/MODEL_ISSUE_OWN_", year, ".Rdata")
print(load(object_name))

# Generate data for plotting 5-day responses to a permanent shock for 
# nationally owned issues and state owned issues
national_results_2018 <- transform_var_irf(
  var_irfs = issue_group_results$`Federally owned`$IRF_res, days = 5)
state_results_2018 <- transform_var_irf(
  var_irfs = issue_group_results$`State owned`$IRF_res, days = 5)
national_plot_db_2018 <- make_plot_data(final_input = national_results_2018,
                                   which_effects = which_effects,
                                   which_day = 5,
                                   column_group = c("state_legislators"))
state_plot_db_2018 <- make_plot_data(final_input = state_results_2018,
                                which_effects = which_effects,
                                which_day = 5,
                                column_group = c("state_legislators"))

# Make a variable that specifies which effects are captured and bidn the effects
national_plot_db_2018$stategroup <-  "National"
state_plot_db_2018$stategroup <- "State"
plot_db_2018 <- rbind(national_plot_db_2018, state_plot_db_2018)
plot_db_2018$Year <- "2018"

##### 2021
year <- "2021"
object_name <- paste0("./models/MODEL_ISSUE_OWN_", year, ".Rdata")
print(load(object_name))

# Generate data for plotting 5-day responses to a permanent shock for 
# nationally owned issues and state owned issues
national_results_2021 <- transform_var_irf(
  var_irfs = issue_group_results$`Federally owned`$IRF_res, days = 5)
state_results_2021 <- transform_var_irf(
  var_irfs = issue_group_results$`State owned`$IRF_res, days = 5)
national_plot_db_2021 <- make_plot_data(final_input = national_results_2021,
                                        which_effects = which_effects,
                                        which_day = 5,
                                        column_group = c("state_legislators"))
state_plot_db_2021 <- make_plot_data(final_input = state_results_2021,
                                     which_effects = which_effects,
                                     which_day = 5,
                                     column_group = c("state_legislators"))

# Make a variable that specifies which effects are captured and bidn the effects
national_plot_db_2021$stategroup <-  "National"
state_plot_db_2021$stategroup <- "State"
plot_db_2021 <- rbind(national_plot_db_2021, state_plot_db_2021)
plot_db_2021$Year <- "2021"



plot_db <- rbind(plot_db_2018, plot_db_2021)

# Define the order for the plot
plot_db$direction <- recode(
  plot_db$direction, 
  `Row groups' response` = "Row groups' response to\nState Legislators",
  `State legislators' response` = "State Legislators' response to\nrow group")
plot_db$stategroup <- factor(plot_db$stategroup,
                            levels = c("State", "National","Both"))

# Make level variable
plot_db$level <- ifelse(plot_db$rowgroup %in% 
                          c("Trump", "Members of Congress",
                            "National Media"), "National\ngroup", "State\ngroup")

# Define the order for the plot
plot_db$stategroup <- factor(plot_db$stategroup,
                             levels = c("National",
                                        "State"
                             ))
plot_db$which_eff <- str_c(plot_db$stategroup, plot_db$Year)
plot_db <- subset(plot_db, plot_db$rowgroup %in% c("Members of\nCongress", "State Partisans", "General Public"))

# Generate Figure 6
plot_by_issue <- ggplot(plot_db, 
                        aes(x = which_eff, y = pe, ymin = lwr, ymax = upr, 
                            col = stategroup)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_linerange(aes(x=which_eff, y=pe, ymin=lwr, ymax=upr), size = 5.5) + 
  geom_segment(aes(x=which_eff, xend=which_eff, y=pe-0.02, yend=pe+0.02), color="black", size=5) +
  #geom_point(aes(x=which_eff, y = (upr+lwr)/2, shape = Year), color = "black", stroke = 1.3) +
  geom_text(aes(x=which_eff, 
                #x=year_dir+.2/1.2, 
                y = upr+.47,
                #y = (upr+lwr)/2,
                label = Year), col = "black", size = 3.3) + 
  facet_nested(rowgroup ~  direction, switch = "y") +
  coord_flip() +
  ylab("\n5-day effect of a permanent 10 percentage point increase") +
  scale_color_manual("Issue domain:", 
                     breaks = c("National", "State"),
                     values = c("gray80", "gray40")) +
  scale_shape_manual("Year:", values = c(2,1)) +
  
  guides(col = guide_legend(ncol=2,nrow=1, reverse = T)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y.left = element_text(angle = 0, size = 11, 
                                     margin = margin(0,.2,0,.2, "cm")),
    strip.text.x = element_text(size = 11),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.y = element_blank()
  )
plot_by_issue

# Save the figure
ggsave(paste0("./figures/figure04", ".png"), plot_by_issue, width = 8.5, height = 5)
