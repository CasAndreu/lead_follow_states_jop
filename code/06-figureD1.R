################################################################################
# 06-figureD1.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  This script fits the models where we differentiate states by the 
#           level of professionalism.  Results are reported in Appendix D.
#           The script generates Figure D1.
# Note:     Models were fit on NYU's HPC cluster. Instances used 40 CPU cores
#           and 100 GB of memory. With this setup, fitting both models took
#           half a day. In this script, we comment out the portion where we fit 
#           the models and instead load the fitted model objects from disk.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study. This is the same data as used for the main models.
#           We then manually subset these data by state.
#           /data/group-day-issue-level-dataset-01-2018
#           /data/group-day-issue-level-dataset-01-2021
# Data Out:
#           1. Two list objects, containing the estimated models and IRFs.
#              /models/MODEL_PROFESSIONALISM_2018.Rdata
#              /models/MODEL_PROFESSIONALISM_2021.Rdata
#              
#           2. Figure D1 of the Appendix:
#              - ./figures/figureD1.png
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(vars)
library(boot)
library(data.table)
library(stringr)
library(ggplot2)
library(ggh4x)

#===============================================================================
# Directories
#===============================================================================

local = T
root_dir = if(local){
  "~/your_local_dir"}else{
    "some_hpc_dir"
  }


#===============================================================================
# FITTING MODELS
#===============================================================================

#for(year in c("2018", "2021")){
#  cat(year, "\n")
#  load_file = paste0(root_dir, "/data/group-day-issue-level-dataset-01-",
#                     year,
#                     ".csv")
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
#  maindb$state_group <- recode(str_extract(maindb$IssueState, "..$"),
#                               `CA` = "High Professionalism",
#                               `NY` = "High Professionalism",
#                               `MA` = "High Professionalism",
#                               `OH` = "High Professionalism",
#                               `IL` = "High Professionalism",
#                               
#                               `UT` = "Low Professionalism",
#                               `NV` = "Low Professionalism",
#                               `VA` = "Low Professionalism",
#                               `MT` = "Low Professionalism",
#                               
#                               `AZ` = "Medium Professionalism",
#                               `FL` = "Medium Professionalism",
#                               `NJ` = "Medium Professionalism",
#                               `TX` = "Medium Professionalism"
#  )
#  
#  # Run two models for the different states
#  state_group_results <- list()
#  for(which_issue_group in c("High Professionalism", "Low Professionalism")){
#    cat(which_issue_group, "\n")
#    
#    # Subset the data by state group
#    issue_group_db <- maindb[which(maindb$state_group == which_issue_group),]
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
#    state_group_results[[which_issue_group]][["VAR_res"]] = var_model_merged
#    state_group_results[[which_issue_group]][["IRF_res"]] = var_irfs_cum_merged
#  }
#  
#  # Save the estimated VAR model and IRFs.
#  # Un-comment the two lines below if you want to write out the models. 
#  object_name = paste0(root_dir, "/models/MODEL_PROFESSIONALISM_", year, ".Rdata")
#  save(state_group_results, file = object_name)
#}



#===============================================================================
# VISUALIZE THE MODELS IN FIGURE D1 USING THE IRFs
#===============================================================================

# Load the figure code
source(paste0(root_dir,"/code/00-functions.R"))

# Define which effects to show
which_effects <- "structural"

##### 2018
year = "2018"
object_name = paste0(root_dir, "/models/MODEL_PROFESSIONALISM_", year, ".Rdata")
print(load(object_name))

# Generate data for plotting 5-day responses to a permanent shock for 
# highly professionalized state legislatures
high_results_2018 <- transform_var_irf(
  var_irfs = state_group_results$`High Professionalism`$IRF_res, days = 5)
low_results_2018 <- transform_var_irf(
  var_irfs = state_group_results$`Low Professionalism`$IRF_res, days = 5)
high_plot_db_2018 <- make_plot_data(final_input = high_results_2018,
                                   which_effects = which_effects,
                                   which_day = 5,
                                   column_group = c("state_legislators"))
low_plot_db_2018 <- make_plot_data(final_input = low_results_2018,
                                   which_effects = which_effects,
                                   which_day = 5,
                                   column_group = c("state_legislators"))

# Make a variable that specifies which effects are captured and bidn the effects
high_plot_db_2018$stategroup <-  "High\nProfessionalism"
low_plot_db_2018$stategroup <- "Low\nProfessionalism"
plot_db_2018 <- rbind(high_plot_db_2018, low_plot_db_2018)
plot_db_2018$Year = "2018"

##### 2021
year = "2021"
object_name = paste0(root_dir, "/models/MODEL_PROFESSIONALISM_", year, ".Rdata")
print(load(object_name))

# Generate data for plotting 5-day responses to a permanent shock for 
# less professionalized state legislatures
high_results_2021 <- transform_var_irf(
  var_irfs = state_group_results$`High Professionalism`$IRF_res, days = 15)
low_results_2021 <- transform_var_irf(
  var_irfs = state_group_results$`Low Professionalism`$IRF_res, days = 15)
high_plot_db_2021 <- make_plot_data(final_input = high_results_2021,
                                        which_effects = which_effects,
                                        column_group = c("state_legislators"))
low_plot_db_2021 <- make_plot_data(final_input = low_results_2021,
                                     which_effects = which_effects,
                                     column_group = c("state_legislators"))

# Make a variable that specifies which effects are captured and bind the effects
high_plot_db_2021$stategroup <-  "High\nProfessionalism"
low_plot_db_2021$stategroup <- "Low\nProfessionalism"
plot_db_2021 <- rbind(high_plot_db_2021, low_plot_db_2021)
plot_db_2021$Year = "2021"

plot_db = rbind(plot_db_2018, plot_db_2021)

# Define the order for the plot
plot_db$direction <- recode(
  plot_db$direction, 
  `Row group response` = "Row groups' response",
  `State legislators response` = "State legislators' response")
plot_db$stategroup = factor(plot_db$stategroup,
                            levels = c("High\nProfessionalism", "Low\nProfessionalism"))

# Define the order for the plot
plot_db$stategroup <- factor(plot_db$stategroup,
                             levels = c("High\nProfessionalism",
                                        "Low\nProfessionalism"
                             ))
plot_db$which_eff = str_c(plot_db$stategroup, plot_db$Year)
plot_db$rowgroup = recode(plot_db$rowgroup,
                          `State Partisans` = "State\nPartisans")
plot_db = subset(plot_db, plot_db$rowgroup %in% c("Members of\nCongress", "State\nPartisans", "General Public"))

# Generate Figure D1
plot_by_state <- ggplot(plot_db, 
                        aes(x = which_eff, y = pe, ymin = lwr, ymax = upr, 
                            col = stategroup)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_linerange(aes(x=which_eff, y=pe, ymin=lwr, ymax=upr), size = 5.5) + 
  geom_segment(aes(x=which_eff, xend=which_eff, y=pe-0.012, yend=pe+0.012), color="black", size=5) +
    geom_text(aes(x=which_eff, y = upr+.45, label = Year), col = "black", size = 3.5) + 
  facet_nested(rowgroup ~  direction, switch = "y") +
  coord_flip() +
  ylab("\n5-day effect of a permanent 10 percentage point increase") +
  scale_color_manual("Level of professionalism:", 
                     breaks = c("High\nProfessionalism", "Low\nProfessionalism"),
                     values = c("gray80", "gray40")) +
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
plot_by_state


# Save the figure
ggsave(paste0(root_dir, "/figures/figureD1", ".png"), plot_by_state, width = 8.5, height = 5)





