################################################################################
# 05-figure03.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  This script fits the models in which we differentiate national
#           legislators by state (section 5.3 of the paper). It then generates 
#           Figure 3.
# Note:     Models were fit on NYU's HPC cluster. Instances used 40 CPU cores
#           and 100 GB of memory. With this setup, fitting both models took
#           half a day. In this script, we comment out the portion where we fit 
#           the models and instead load the fitted model objects from disk.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study. However, unlike in the files used for our main models
#           the time series for MoCs were differentiated by the state they
#           are associated with.
#           /data/group-day-issue-level-dataset-04-2018.csv
#           /data/group-day-issue-level-dataset-04-2021.csv
# Data Out:
#           1. Two estimated VAR models, one for 2018 and one for 2021.
#              /models/MODEL_DIFFERENTIATED_2018.Rdata
#              /models/MODEL_DIFFERENTIATED_2021.Rdata
#           2. Two IRF objects with the impulse response estimates based on the
#              fitted models.
#              /models/MODEL_DIFFERENTIATED_IRFs_2018.Rdata
#              /models/MODEL_DIFFERENTIATED_IRFs_2021.Rdata
#              
#           3. Figure 3 of the main paper:
#              - ./figures/figure03.png
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(vars)
library(ggplot2)
library(ggh4x)
library(boot)
library(stringr)


#===============================================================================
# FITTING MODELS
#===============================================================================

##### Defines years variable
#years <- c("2018", "2021")
#
###### For each year, run the models with the same parameters
#for(year in years){
#  
#  cat(year, "\n")
#  #-----------------------------------------------------------------------------
#  # DATA
#  #-----------------------------------------------------------------------------
#  # Model data
#  load_file <- paste0("./data/group-day-issue-level-dataset-04-", year,".csv")
#  maindb <- read.csv(load_file)
#  
#  # Sort:
#  maindb <- maindb[order(maindb$IssueState, maindb$Date),]
#  
#  #-----------------------------------------------------------------------------
#  # MAIN
#  #-----------------------------------------------------------------------------
#  # Create a formula object that will facilitate transforming the topic variable 
#  # from factor to dummies.
#  if(year == "2021"){
#    variables <- c("national_legislators", "national_media", "state_legislators", 
#                   "state_media", "state_partisans", "state_random_partisans", "Trump", "IssueState")
#  }
#  if(year == "2018"){
#    variables <- c("national_legislators", "national_media", "state_legislators", 
#                   "state_media", "state_partisans", "Trump", "IssueState")
#  }
#  mformula <- formula(paste0("~", paste0(variables, collapse = " + ")))
#  model_data <- model.matrix(mformula, maindb[, variables])
#  model_data <- model_data[, 2:ncol(model_data)] # remove intercept
#  
#  # Splitting the covariates of interest from the issue dummy variables
#  X_endogenous <- model_data[, variables[which(!variables %in% c("IssueState"))]]
#  X_exogenous <- model_data[
#    , which(!colnames(model_data) %in% 
#              variables[which(!variables %in% c("IssueState"))])]
#  
#  # Define lags, number of bootstraps, and set seed
#  p <- 5
#  runs <- 300
#  seed <- 78223
#  set.seed(seed)
#  
#  # Run the VAR model and estimate IRFs
#  var_model_merged <- VAR(y = X_endogenous, p = p, exogen = X_exogenous)
#  var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 15, ortho = F,
#                             cumulative = TRUE, runs = runs)
#  
#  # Save the estimated VAR model and IRFs.
#  save(var_model_merged, file = paste0("./models/MODEL_DIFFERENTIATED_", year, ".Rdata"))
#  save(var_irfs_cum_merged, file = paste0("./models/MODEL_DIFFERENTIATED_IRFs_", year, ".Rdata"))
#}



#===============================================================================
# VISUALIZE THE MODELS IN FIGURE 3 USING THE IRFs
#===============================================================================

# Load the figure code
source(paste0("./code/00-functions.R"))

# Define which type of effect to show
which_effects <- "structural"

##### Load the MOOC seperated ones
year <- "2021"
model_name <- paste0("./models/MODEL_DIFFERENTIATED_IRFs_", year, ".Rdata")
print(load(model_name))
var_irfs_cum_merged_2021 <- var_irfs_cum_merged
year <- "2018"
model_name <- paste0("./models/MODEL_DIFFERENTIATED_IRFs_", year, ".Rdata")
print(load(model_name))
var_irfs_cum_merged_2018 <- var_irfs_cum_merged

# Generate data for plotting 5-day responses to a permanent shock
final_input_2018 <- transform_var_irf(var_irfs = var_irfs_cum_merged_2018, days = 5)
plot_db_2018 <- make_plot_data(final_input = final_input_2018, 
                          which_effects = which_effects,
                          which_day = 5,
                          column_group = c("state_legislators"))
plot_db_2018$Year <- "2018"
plot_db_2018$model <- "Members of Congress\ndifferentiated by state"


final_input_2021 <- transform_var_irf(var_irfs = var_irfs_cum_merged_2021, days = 5)
plot_db_2021 <- make_plot_data(final_input = final_input_2021, 
                               which_effects = which_effects,
                               which_day = 5,
                               column_group = c("state_legislators"))
plot_db_2021$Year <- "2021"
plot_db_2021$model <- "Members of Congress\ndifferentiated by state"


##### Load the MOOC coherent
year = "2021"
model_name <- paste0("./models/MODEL_I_IRFs_", year, ".Rdata")
print(load(model_name))
var_irfs_cum_merged_2021 <- var_irfs_cum_merged
year <- "2018"
model_name <- paste0("./models/MODEL_I_IRFs_", year, ".Rdata")
print(load(model_name))
var_irfs_cum_merged_2018 <- var_irfs_cum_merged

# Generate data for plotting 5-day responses to a permanent shock
final_input_2018 <- transform_var_irf(var_irfs = var_irfs_cum_merged_2018, days = 5)
plot_db_2018_m1 <- make_plot_data(final_input = final_input_2018, 
                               which_effects = which_effects,
                               which_day = 5,
                               column_group = c("state_legislators"))
plot_db_2018_m1$Year <- "2018"
plot_db_2018_m1$model <- "Members of Congress\ncombined"


final_input_2021 <- transform_var_irf(var_irfs = var_irfs_cum_merged_2021, days = 5)
plot_db_2021_m1 <- make_plot_data(final_input = final_input_2021, 
                               which_effects = which_effects,
                               which_day = 5,
                               column_group = c("state_legislators"))
plot_db_2021_m1$Year <- "2021"
plot_db_2021_m1$model <- "Members of Congress\ncombined"


##### Combine all
plot_db <- rbind(plot_db_2018, plot_db_2021, plot_db_2018_m1, plot_db_2021_m1)

plot_db$rowgroup <- recode(plot_db$rowgroup,  "Trump" = "President")

# Make level variable
plot_db$level <- ifelse(plot_db$rowgroup %in% c("President", "Members of\nCongress",
                                               "National Media"), 
                        "National\ngroup", "State\ngroup")

#  Define the order for the plot
plot_db$direction <- factor(plot_db$direction,
                            levels = c("Row groups' response",
                                       "State legislators' response"
                            ))
plot_db$year_dir <- paste0(plot_db$direction, plot_db$Year, sep = "")
plot_db$model_year <- str_c(plot_db$model, "_", plot_db$Year, "_")
plot_db$direction <- recode(plot_db$direction,
                          `Row groups' response` = "Members of Congress' response\nto State Legislators",
                          `State legislators' response` = "State legislators' response\nto Members of Congress")

plot_db <- subset(plot_db, plot_db$rowgroup == "Members of\nCongress")

# Generate Figure showing the main results for Model I (Figure 4)
plot_overall <- ggplot(plot_db, aes(x = Year, y = pe, ymin = lwr, 
                                    ymax = upr, col = model, pattern = Year)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_linerange(aes(x=Year, ymin=lwr, ymax=upr), size = 5.5) + 
  geom_segment(aes(x=Year, xend=Year, y=pe-0.005, yend=pe+0.005), color="black", size=5) +

  scale_size_manual("Model:", values = c(4, 1.5)) +
  geom_text(aes(x=Year, y = upr+.11, label = Year), col = "black", size = 3.3) + 
  facet_nested(. ~ direction, switch = "y") +
  coord_flip() +
  ylab("\n5-day effect of a permanent 10 percentage point increase") +
  scale_color_manual("Model:", values = c("gray80", "gray40")) +
  guides(col = guide_legend(ncol=2,nrow=1, reverse = TRUE)) +
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
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.y = element_blank()
  )
plot_overall


#  Save the figure
ggsave(paste0("./figures/figure03", ".png"), plot_overall, width = 8.5, height = 2.5)

