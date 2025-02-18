################################################################################
# AppendixC.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Figure C1 in Appendix C of the paper.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study. However, unlike in the files used for other models,
#           the time series for groups are differentiated by party affiliation.
#           In other words, there are independent time series for democrats
#           and republicans.
#           ./data/group-day-issue-level-dataset-03-2018.csv
#           ./data/group-day-issue-level-dataset-03-2021.csv
# Data Out:
#           1. Two IRF objects with the impulse response estimates based on the
#              fitted models.
#              - ./models/MODEL_BY_OWN_PARTY_2018.Rdata
#              - ./models/MODEL_BY_OWN_PARTY_2021.Rdata
#              
#           2. Figure C2 of the Appendix:
#              - ./figures/figureC1.png
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


###### Defines years variable
#years <- c("2018", "2021")
#
###### For each year, run the models with the same parameters
#for(year in years){
#  cat(year, "\n")
#  
#  #-------------------------------------------------------------------------------
#  # DATA
#  #-------------------------------------------------------------------------------
#  #  Model data
#  load_file <- paste0("./data/group-day-issue-level-dataset-03-",year,".csv")
#  
#  maindb <- read.csv(load_file)
#  
#  # Sort order:
#  maindb <- maindb[order(maindb$IssueState, maindb$Date),]
#  
#  #-------------------------------------------------------------------------------
#  # MAIN
#  #-------------------------------------------------------------------------------
#  # Create a formula object that will facilitate transforming the topic variable 
#  # from factor to dummies.
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
#  # Create a formula object that will facilitate transforming the topic variable 
#  # from factor to dummies
#  mformula <- formula(paste0("~", paste0(variables, collapse = " + ")))
#  model_data <- model.matrix(mformula, maindb[, variables])
#  model_data <- model_data[, 2:ncol(model_data)] # remove intercept
#  
#  # Splitting the covariates of interest from the issue dummy variables
#  X_endogenous <- model_data[, variables[which(!variables %in% c("IssueState"))]]
#  X_exogenous <- model_data[
#    , which(!colnames(model_data) %in% variables[
#      which(!variables %in% c("IssueState"))])]
#  
#  # Define lags, number of bootstraps, and set seed
#  p <- 5
#  seed<- 82734
#  runs <- 300
#  set.seed(seed)
#  
#  # Run the VAR model and estimate IRFs:
#  var_model_merged <- VAR(y = X_endogenous, p = p, exogen = X_exogenous)
#  var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 15, ortho = F,
#                             cumulative = TRUE, 
#                             runs = runs)
#
#  # Save the estimated IRFs.
#  save(var_irfs_cum_merged, file = paste0("./models/MODEL_BY_OWN_PARTY_",year,".Rdata"))
#  
#}




#===============================================================================
# VISUALIZE THE MODELS IN FIGURE C2 USING THE IRFs
#===============================================================================

# Load the figure code
source(paste0("./code/00-functions.R"))

# Define which effects to show
which_effects <- "structural"

year<- "2021"
model_name <- paste0("./models/MODEL_BY_OWN_PARTY_",year,".Rdata")
print(load(model_name))
var_irfs_cum_merged_2021 <- var_irfs_cum_merged
year <- "2018"
model_name <- paste0("./models/MODEL_BY_OWN_PARTY_",year,".Rdata")
print(load(model_name))
var_irfs_cum_merged_2018 <- var_irfs_cum_merged


# Generate data for plotting 5-day responses to a permanent shock
final_input_2018 <- transform_var_irf(var_irfs = var_irfs_cum_merged_2018, days = 5)
plot_db_2018 <- make_plot_data(final_input = final_input_2018, party_d = T,
                               which_day = 5,
                               which_effects = which_effects,
                               column_group = c("state_legislators_democrat", 
                                                "state_legislators_republican"))
plot_db_2018$Year <- "2018"


# Generate data for plotting 5-day responses to a permanent shock
final_input_2021 <- transform_var_irf(var_irfs = var_irfs_cum_merged_2021, days = 5)
plot_db_2021 <- make_plot_data(final_input = final_input_2021, party_d = T,
                               which_effects = which_effects,
                               which_day = 5,
                               column_group = c("state_legislators_democrat", 
                                                "state_legislators_republican"))
plot_db_2021$Year <- "2021"

plot_db <- rbind(plot_db_2018, plot_db_2021)

##### Some recoding
plot_db$rowgroup <- recode(plot_db$rowgroup,  "Trump" = "President")
plot_db <- subset(plot_db, plot_db$direction == "State legislators' response")
plot_db <- subset(plot_db, plot_db$rowgroup %in% c("Republican\nState Partisans",
                                                  "Democrat\nState Partisans",
                                                  "Democrats\nin Congress",
                                                  "Republicans\nin Congress"))

plot_db$own_party <- ifelse(
  (str_detect(plot_db$rowgroup, "Democrat") & str_detect(plot_db$colgroup, "Democrat")) |
    (str_detect(plot_db$rowgroup, "Republican") & str_detect(plot_db$colgroup, "Republican"))
  , "Own party's State Legislators", "Other party's State Legislators")

plot_db$year_dyad <- str_c(plot_db$Year, plot_db$own_party)
# Generate Figure 5
plot_by_party <- ggplot(plot_db, aes(x = year_dyad, y = pe, ymin = lwr, 
                                    ymax = upr, col = own_party, pattern = Year)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_linerange(aes(x=year_dyad, ymin=lwr, ymax=upr), size = 5.5) + 
  geom_segment(aes(x=year_dyad, xend=year_dyad, y=pe-0.008, yend=pe+0.008), color="black", size=5) +
  scale_size_manual("Model:", values = c(4, 1.5)) +
  geom_text(aes(x=year_dyad, y = upr+.21, label = Year), col = "black", size = 3.5) + 
  facet_nested(rowgroup ~ direction, switch = "y") +
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
    axis.text.x = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.y = element_blank()
  )
plot_by_party


#  Save the figure
ggsave(paste0("./figures/figureC1", ".png"), plot_by_party, width = 8.5, height = 6)
