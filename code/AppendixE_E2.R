################################################################################
# AppendixE_E2.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Figure E2, from Appendix E of the paper.
# Note:     Models for Figture E2 were fit on NYU's HPC cluster. Instances used 
#           40 CPU cores and 100 GB of memory. With this setup, fitting both 
#           models took half a day. In this script, we comment out the portion 
#           where we fit the models and instead load the fitted model objects from disk.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study. The time series for groups are differentiated by 
#           party affiliation. In other words, there are independent time series 
#           for democrats and republicans.
#           /data/group-day-issue-level-dataset-03-2018.csv
#           /data/group-day-issue-level-dataset-03-2018.csv
# Data Out:
#           1. Two estimated VAR models, one for 2018 and one for 2021. Each
#              contains the data both for Republicans and Democrats
#              /models/MODEL_by_issue_and_party_dual-2018.Rdata
#              /models/MODEL_by_issue_and_party_dual-2021.Rdata
#              
#           3. Figure E2
#              - ./figures/figureE2.png
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(vars)
library(data.table)
library(boot)
library(stringr)
library(ggplot2)
library(ggh4x)

#===============================================================================
# FITTING MODELS
#===============================================================================

#for(year in c("2018", "2021")){
#  cat(year, "\n")
#  
#  #-------------------------------------------------------------------------------
#  # DATA
#  #-------------------------------------------------------------------------------
#  #  Model data
#  load_file <- paste0("./data/group-day-issue-level-dataset-03-", year, ".csv")
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
#    variables <- c(
#      "national_legislators_democrat", "national_legislators_republican", 
#      "national_media_", "state_legislators_democrat", "state_legislators_republican", 
#      "state_media_", "state_random_partisans_", "state_partisans_democrat", "state_partisans_republican", 
#      "Trump_", "IssueState")
#  }
#  if(year == "2018"){
#    variables <- c(
#      "national_legislators_democrat", "national_legislators_republican", 
#      "national_media_", "state_legislators_democrat", "state_legislators_republican", 
#      "state_media_", "state_partisans_democrat", "state_partisans_republican", 
#      "Trump_", "IssueState")
#  }
#  
#  
#  
#  maindb$issue_group <- recode(str_extract(maindb$IssueState, ".+(?=[A-Z]{2})"),
#                               
#                               `Defense` = "Republican-owned",
#                               `Agriculture` = "Republican-owned",
#                               `Economy` = "Republican-owned",
#                               `Foreign Trade` = "Republican-owned",
#                               `Intl. Affairs` = "Republican-owned",
#                               `Energy` = "Republican-owned",
#                               
#                               
#                               `Domestic Commerce` = "No Party",
#                               `Gov. Operations` = "No Party",
#                               `Immigration` = "No Party",
#                               `Technology` = "No Party",
#                               `Education` = "No Party",
#                               `Transportation` = "No Party",
#                               `Law and Crime` = "No Party",
#                               `Public Lands` = "No Party",
#                               
#                               
#                               `Healthcare` = "Democratic-owned",
#                               `Labor` = "Democratic-owned",
#                               `Social Welfare` = "Democratic-owned",
#                               `Gun Control` = "Democratic-owned",
#                               `Civil Rights` = "Democratic-owned",
#                               `Housing` = "Democratic-owned",
#                               `Environment` = "Democratic-owned"
#                               
#  )
#  
#  
#  # Run models for both parties
#  state_group_results <- list()
#  for(which_issue_group in c("Republican-owned", "Democratic-owned")){
#    cat(which_issue_group, "\n")
#    
#    # Subset the data by party
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
#    state_group_results[[which_issue_group]][["VAR_res"]] = var_model_merged
#    state_group_results[[which_issue_group]][["IRF_res"]] = var_irfs_cum_merged
#  }
#  
#  # Save the estimated VAR model and IRFs.
#  # Un-comment the two lines below if you want to write out the models. 
#  object_name = paste0("./models/MODEL_by_issue_and_party_dual-", year, ".Rdata")
#  save(state_group_results, file = object_name)
#}




#===============================================================================
# VISUALIZE THE MODELS IN FIGURE F2
#===============================================================================

# Load the figure code
source("./code/00-functions.R")

# Define which effects to show
which_effects <- "structural"


#===============================================================================
# Get the effects for 2018
year <- "2018"
object_name <- paste0("./models/MODEL_by_issue_and_party_dual-", year, ".Rdata")
print(load(object_name))

# Generate data for plotting 5-day responses to a permanent shock for 
# nationally owned issues and state owned issues
rep_results_2018 <- transform_var_irf(
  var_irfs = state_group_results$`Republican-owned`$IRF_res, days = 5)
dem_results_2018 <- transform_var_irf(
  var_irfs = state_group_results$`Democratic-owned`$IRF_res, days = 5)


dem_rep_plot_db_2018 <- make_plot_data(final_input = rep_results_2018, party_d = T,
                                    which_effects = which_effects,
                                    which_day = 5,
                                    column_group = c("state_legislators_democrat"))
dem_dem_plot_db_2018 <- make_plot_data(final_input = dem_results_2018, party_d = T,
                                   which_effects = which_effects,
                                   which_day = 5,
                                   column_group = c("state_legislators_democrat"))
rep_rep_plot_db_2018 <- make_plot_data(final_input = rep_results_2018, party_d = T,
                                       which_effects = which_effects,
                                       which_day = 5,
                                       column_group = c("state_legislators_republican"))
rep_dem_plot_db_2018 <- make_plot_data(final_input = dem_results_2018, party_d = T,
                                       which_effects = which_effects,
                                       which_day = 5,
                                       column_group = c("state_legislators_republican"))

dem_rep_plot_db_2018$issue_group <-  "Republican-owned"
dem_dem_plot_db_2018$issue_group <- "Democratic-owned"
dem_rep_plot_db_2018$direction <- ifelse(dem_rep_plot_db_2018$direction == "State legislators' response",
                                        "Democratic State Legislators' response", NA)
dem_dem_plot_db_2018$direction <- ifelse(dem_dem_plot_db_2018$direction == "State legislators' response",
                                        "Democratic State Legislators' response", NA)

rep_rep_plot_db_2018$issue_group <-  "Republican-owned"
rep_dem_plot_db_2018$issue_group <- "Democratic-owned"
rep_rep_plot_db_2018$direction <- ifelse(rep_rep_plot_db_2018$direction == "State legislators' response",
                                        "Republican State Legislators' response", NA)
rep_dem_plot_db_2018$direction <- ifelse(rep_dem_plot_db_2018$direction == "State legislators' response",
                                        "Republican State Legislators' response", NA)

plot_db_2018 <- rbind(dem_rep_plot_db_2018, dem_dem_plot_db_2018, rep_rep_plot_db_2018, rep_dem_plot_db_2018)
plot_db_2018$Year <- "2018"


#===============================================================================
# Get the effects for 2021
year <- "2021"
object_name <- paste0("./models/MODEL_by_issue_and_party_dual-", year, ".Rdata")
print(load(object_name))

# Generate data for plotting 5-day responses to a permanent shock for 
# nationally owned issues and state owned issues
rep_results_2021 <- transform_var_irf(
  var_irfs = state_group_results$`Republican-owned`$IRF_res, days = 5)
dem_results_2021 <- transform_var_irf(
  var_irfs = state_group_results$`Democratic-owned`$IRF_res, days = 5)


dem_rep_plot_db_2021 <- make_plot_data(final_input = rep_results_2021, party_d = T,
                                       which_effects = which_effects,
                                       which_day = 5,
                                       column_group = c("state_legislators_democrat"))
dem_dem_plot_db_2021 <- make_plot_data(final_input = dem_results_2021, party_d = T,
                                       which_effects = which_effects,
                                       which_day = 5,
                                       column_group = c("state_legislators_democrat"))
rep_rep_plot_db_2021 <- make_plot_data(final_input = rep_results_2021, party_d = T,
                                       which_effects = which_effects,
                                       which_day = 5,
                                       column_group = c("state_legislators_republican"))
rep_dem_plot_db_2021 <- make_plot_data(final_input = dem_results_2021, party_d = T,
                                       which_effects = which_effects,
                                       which_day = 5,
                                       column_group = c("state_legislators_republican"))

dem_rep_plot_db_2021$issue_group <-  "Republican-owned"
dem_dem_plot_db_2021$issue_group <- "Democratic-owned"
dem_rep_plot_db_2021$direction <- ifelse(dem_rep_plot_db_2021$direction == "State legislators' response",
                                        "Democratic State Legislators' response", NA)
dem_dem_plot_db_2021$direction <- ifelse(dem_dem_plot_db_2021$direction == "State legislators' response",
                                        "Democratic State Legislators' response", NA)

rep_rep_plot_db_2021$issue_group <-  "Republican-owned"
rep_dem_plot_db_2021$issue_group <- "Democratic-owned"
rep_rep_plot_db_2021$direction <- ifelse(rep_rep_plot_db_2021$direction == "State legislators' response",
                                        "Republican State Legislators' response", NA)
rep_dem_plot_db_2021$direction <- ifelse(rep_dem_plot_db_2021$direction == "State legislators' response",
                                        "Republican State Legislators' response", NA)

plot_db_2021 <- rbind(dem_rep_plot_db_2021, dem_dem_plot_db_2021, rep_rep_plot_db_2021, rep_dem_plot_db_2021)
plot_db_2021$Year <- "2021"



#===============================================================================
# Merge and plot
plot_db <- rbind(plot_db_2018, plot_db_2021)
plot_db <- subset(plot_db, ! is.na(plot_db$direction))

# Define the order for the plot
plot_db$stategroup <- factor(plot_db$issue_group,
                            levels = c("Republican-owned", "Democratic-owned"))


plot_db$which_eff <- str_c(plot_db$Year, plot_db$issue_group)
plot_db_p <- subset(plot_db, plot_db$rowgroup %in% c("Democrats\nin Congress", "Republicans\nin Congress", 
                                                  "Democrat\nState Partisans", "Republican\nState Partisans", 
                                                  "State Media", "General Public"))

# Generate Figure F2
ggplot(plot_db_p, 
                        aes(x = which_eff, y = pe, ymin = lwr, ymax = upr, 
                            col = stategroup)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_linerange(aes(x=which_eff, y=pe, ymin=lwr, ymax=upr), size = 5.5, alpha = .8) + 
  geom_segment(aes(x=which_eff, xend=which_eff, y=pe-0.012, yend=pe+0.012), color="black", size=5) +
  geom_text(aes(x=which_eff, y = upr+.45, label = Year), col = "black", size = 3.5) + 
  facet_nested(rowgroup ~  direction, switch = "y") +
  coord_flip() +
  ylab("\n5-day effect of a permanent 10 percentage point increase") +
  scale_color_manual("Issue ownership:", 
                     breaks = c("Democratic-owned", "Republican-owned"),
                     values = c("blue", "red")) +
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


# Save the figure
ggsave("./figures/figureE2.png", width = 8.5, height = 8.5)
