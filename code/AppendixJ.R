################################################################################
# AppendixJ.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  This script replicates the table in Appendix J.
# Data In:  
#           Two datasets in which we have group-day-issue-level information about
#           how much attention each group of analysis paid to each day to each 
#           topic we study:
#           ./data/group-day-issue-level-dataset-01-2018.csv
#           ./data/group-day-issue-level-dataset-01-2021.csv
# Data Out:  
#           This script does not save any files, it prints the table to the 
#           console.
################################################################################



#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(stringr)
library(xtable)
library(tseries)


#===============================================================================
# TESTING FUNCTION FOR ALL COMBINATIONS
#===============================================================================
get_tests <- function(year){
  load_file<- paste0("./data/group-day-issue-level-dataset-01-",year,".csv")
  
  
  maindb <- read.csv(load_file)
  
  # Sort:
  maindb <- maindb[order(maindb$IssueState, maindb$Date),]
  
  
  if(year == "2021"){
    variables_state <- c("state_legislators", "state_media", "state_partisans", "state_random_partisans")
    variables_nat <- c("national_legislators", "national_media", "Trump")
  }
  
  if(year == "2018"){
    variables_state <- c("state_legislators", "state_media", "state_partisans")
    variables_nat <- c("national_legislators", "national_media", "Trump")
  }
  
  
  maindb$issue <- str_extract(maindb$IssueState, ".+(?=[A-Z]{2})")
  maindb$state <- str_extract(maindb$IssueState, "[A-Z]{2}$")
  
  state_db <- maindb[, c("issue", "state", "IssueState", variables_state)]
  df_results_state <- data.frame()
  for(iss_state in unique(state_db$IssueState)){
    cat(iss_state, "\n")
    time_series_df <- subset(state_db, state_db$IssueState == iss_state)
    for(var in variables_state){
      #if(!is.character(time_series_df[,var])){
      cat(var, "\n")
      test_res <- adf.test(time_series_df[,var],
                          k = 5,
                          alternative = c("stationary"))
      df_results_state <- rbind(df_results_state, data.frame("p_val" = test_res$p.value, "df_value" = test_res$parameter,
                                                            "issue" = time_series_df$issue[1], 
                                                            "state" = time_series_df$state[1], "var" = var))
      #}
    }
  }
  
  nat_db <- maindb[, c("issue", "state", "IssueState", variables_nat)]
  nat_db <- subset(nat_db, state == "VA")
  nat_db$state = "ALL"
  nat_db$IssueState <- str_replace_all(nat_db$IssueState, "[A-Z]{2}$", "ALL")
  df_results_nat <- data.frame()
  for(iss_state in unique(nat_db$IssueState)){
    cat(iss_state, "\n")
    time_series_df <- subset(nat_db, nat_db$IssueState == iss_state)
    for(var in variables_nat){
      #if(!is.character(time_series_df[,var])){
      cat(var, "\n")
      test_res <- adf.test(time_series_df[,var],
                          k = 5,
                          alternative = c("stationary"))
      df_results_nat <- rbind(df_results_nat, data.frame("p_val" = test_res$p.value, "df_value" = test_res$parameter,
                                                        "issue" = time_series_df$issue[1], 
                                                        "state" = time_series_df$state[1], 
                                                        "var" = var))
      #}
    }
  }
  return(list("nat_res" = df_results_nat, "state_res" = df_results_state))
}



#===============================================================================
# RUN THE TESTS AND AGGREGATE RESULTS TO TABLE
#===============================================================================

#### Tun tests (note that this will throw warnings because tome combinations are)
#    not filled. This is expected behavior.
results_2018 <- get_tests(year = "2018")
results_2021 <- get_tests(year = "2021")

#### For writeup print distribution of stationarity
table(results_2018$nat_res$p_val <= .01)
table(results_2021$nat_res$p_val <= .01)
table(results_2018$nat_res$p_val <= .05)
table(results_2021$nat_res$p_val <= .05)

#### Define variables
variables_state <- c("state_legislators", "state_media", "state_partisans", "state_random_partisans")
variables_nat <- c("national_legislators", "national_media", "Trump")

#### Table nat groups
nat_results = data.frame()
for(var_i in variables_nat){
  pos_per_2018 = mean(subset(results_2018$nat_res, var == var_i)$p_val < .05)
  pos_per_2021 = mean(subset(results_2021$nat_res, var == var_i)$p_val < .05)
  together = rbind(results_2018$nat_res, results_2021$nat_res)
  pos_per_both = mean(subset(together, var == var_i)$p_val < .05)
  foo = cbind("Group or issue" = var_i, "Both years" = round(pos_per_both, 2), "2018" = round(pos_per_2018,2), "2021" = round(pos_per_2021,2))
  nat_results = rbind(nat_results, foo)
}

#### Table state groups
state_results = data.frame()
for(var_i in variables_state){
  pos_per_2018 = mean(subset(results_2018$state_res, var == var_i)$p_val < .05)
  pos_per_2021 = mean(subset(results_2021$state_res, var == var_i)$p_val < .05)
  together = rbind(results_2018$state_res, results_2021$state_res)
  pos_per_both = mean(subset(together, var == var_i)$p_val < .05)
  foo = cbind("Group or issue" = var_i, "Both years" = round(pos_per_both, 2), "2018" = round(pos_per_2018,2), "2021" = round(pos_per_2021,2))
  state_results = rbind(state_results, foo)
}


#### Table issues
issue_results = data.frame()
issues = unique(results_2018$state_res$issue)
results_2018_both = rbind(results_2018$state_res, results_2018$nat_res)
results_2021_both = rbind(results_2021$state_res, results_2021$nat_res)
for(issue_i in issues){
  pos_per_2018 = mean(subset(results_2018_both, issue == issue_i)$p_val < .05)
  pos_per_2021 = mean(subset(results_2021_both, issue == issue_i)$p_val < .05)
  together = rbind(results_2018_both, results_2021_both)
  pos_per_both = mean(subset(together, issue == issue_i)$p_val < .05)
  foo = cbind("Group or issue" = issue_i, "Both years" = round(pos_per_both, 2), "2018" = round(pos_per_2018,2), "2021" = round(pos_per_2021,2))
  issue_results = rbind(issue_results, foo)
}


p05 = rbind(nat_results, state_results, issue_results)
p05$`Group or issue` = recode(p05$`Group or issue`, 
             `national_legislators` = "Membergs of Congress",
             `national_media` = "National media",
             `Trump` = "President",
             `state_legislators` = "State Legislators",
             `state_partisans` = "State Partisans",
             `state_random_partisans` = "Random Public",
             `state_media` = "State Media")

latex_table <- xtable(p05,
                      label = "tab:station",
                      caption = "Stationarity by group, issue, and year.")

#----------
# TABLE J1
#----------
# Print the LaTeX code to the console
print(latex_table, include.rownames = F)
