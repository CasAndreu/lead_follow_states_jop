################################################################################
# AppendixL.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Table L in  Appendix L of the paper.
# Data In:  
#           1. Model objects for the main models in Figure 2
#              - ./models/MODEL_I_2018.Rdata
#              - ./models/MODEL_I_2021.Rdata
#
# Data Out:
#           1. Table L1 and L2
################################################################################

#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(boot)
library(ggplot2)

#===============================================================================
# MAIN
#===============================================================================
#-------------------------------------------------------------------------------
# 2018 VAR MODEL
#-------------------------------------------------------------------------------
# - load the 2018 VAR model
print(load("./models/MODEL_I_2018.Rdata"))

# - recursively pull coef of interest to build coef tab (e.g. exclude state-topic
#   fixed-effects)
groups <- c("state_legislators", "state_media", "state_partisans", 
            "national_legislators", "national_media", "Trump")

# - initialize empty dataframe
out <- NULL

for (group in groups) {
  # - pull coef/predictors for these
  group_coefs <- broom::tidy(var_model_merged$varresult[[group]])
  gof <- broom::glance(var_model_merged$varresult[[group]])
  group_coefs <- group_coefs %>%
    filter(!grepl("IssueState", term)) %>%
    mutate(pe = paste0(
      round(estimate, 3),
      " (",
      round(std.error, 3),
      ")", 
      ifelse(p.value < 0.05, "*", " ")
    )) %>%
    dplyr::select(term, pe)
  group_coefs$lag <- as.character(sapply(group_coefs$term, function(x)
    strsplit(x, split = "\\.")[[1]][2]))
  group_coefs$group <- as.character(sapply(group_coefs$term, function(x)
    strsplit(x, split = "\\.")[[1]][1]))
  group_coefs <- group_coefs %>%
    mutate(group = recode(group,
                          `national_legislators` = "Members of Congress",
                          `national_media` = "National Media",
                          `state_legislators` = "State Legislators",
                          `state_media` = "State Media",
                          `state_partisans` = "State Partisans",
                          `Trump` = "President",
                          `const` = "Intercept"),
           group = factor(group, levels = c(
             "Intercept",
             "State Legislators",
             "State Partisans",
             "State Media",
             "Members of Congress",
             "National Media",
             "President"
           ))) %>%
    dplyr::select(group, lag, pe) %>%
    arrange(group, lag)
  colnames(group_coefs)[3] <- paste0(group, "_2018")
  # - add goodness of fit stats
  gof_df <- gof %>%
    dplyr::select(r.squared, adj.r.squared, logLik, AIC, BIC) %>%
    t() %>%
    as.data.frame()
  gof_df$group <- rownames(gof_df)
  gof_df <- gof_df %>%
    mutate(lag = NA) %>%
    dplyr::select(group, lag, V1) %>%
    mutate(V1 = round(V1, 3),
           group = recode(group,
                          `r.squared` = "R Squared",
                          `adj.r.squared` = "Adjusted R Squared",
                          `logLik` = "Log Likelihood"))
  colnames(gof_df)[3] <- paste0(group, "_2018")
  rownames(gof_df) <- NULL
  group_coefs <- rbind(group_coefs, gof_df)
  if (is.null(out)) {
    out <- group_coefs
  } else {
    out <- left_join(out, group_coefs)
  }
}

# - purge the 2018 model before loding the 2021 one, to free up memory
rm(var_model_merged)

#-------------------------------------------------------------------------------
# 2021 VAR MODEL
#-------------------------------------------------------------------------------
# - load the 2021 VAR model
print(load("./models/MODEL_I_2021.Rdata"))

# - recursively pull coef of interest to build coef tab (e.g. exclude state-topic
#   fixed-effects)
groups <- c("state_legislators", "state_media", "state_random_partisans", 
            "state_partisans", "national_legislators", "national_media", "Trump")

# - initialize empty dataframe
out02 <- NULL

for (group in groups) {
  # - pull coef/predictors for these
  group_coefs <- broom::tidy(var_model_merged$varresult[[group]])
  gof <- broom::glance(var_model_merged$varresult[[group]])
  group_coefs <- group_coefs %>%
    filter(!grepl("IssueState", term)) %>%
    mutate(pe = paste0(
      round(estimate, 3),
      " (",
      round(std.error, 3),
      ")", 
      ifelse(p.value < 0.05, "*", " ")
    )) %>%
    dplyr::select(term, pe)
  group_coefs$lag <- as.character(sapply(group_coefs$term, function(x)
    strsplit(x, split = "\\.")[[1]][2]))
  group_coefs$group <- as.character(sapply(group_coefs$term, function(x)
    strsplit(x, split = "\\.")[[1]][1]))
  group_coefs <- group_coefs %>%
    mutate(group = recode(group,
                          `national_legislators` = "Members of Congress",
                          `national_media` = "National Media",
                          `state_legislators` = "State Legislators",
                          `state_media` = "State Media",
                          `state_partisans` = "State Partisans",
                          `state_random_partisans` = "General Public",
                          `Trump` = "President",
                          `const` = "Intercept"),
           group = factor(group, levels = c(
             "Intercept",
             "State Legislators",
             "General Public",
             "State Partisans",
             "State Media",
             "Members of Congress",
             "National Media",
             "President"
           ))) %>%
    dplyr::select(group, lag, pe) %>%
    arrange(group, lag)
  colnames(group_coefs)[3] <- paste0(group, "_2021")
  # - add goodness of fit stats
  gof_df <- gof %>%
    dplyr::select(r.squared, adj.r.squared, logLik, AIC, BIC) %>%
    t() %>%
    as.data.frame()
  gof_df$group <- rownames(gof_df)
  gof_df <- gof_df %>%
    mutate(lag = NA) %>%
    dplyr::select(group, lag, V1) %>%
    mutate(V1 = round(V1, 3),
           group = recode(group,
                          `r.squared` = "R Squared",
                          `adj.r.squared` = "Adjusted R Squared",
                          `logLik` = "Log Likelihood"))
  colnames(gof_df)[3] <- paste0(group, "_2021")
  rownames(gof_df) <- NULL
  group_coefs <- rbind(group_coefs, gof_df)
  if (is.null(out02)) {
    out02 <- group_coefs
  } else {
    out02 <- left_join(out02, group_coefs)
  }
}

# - purge the 2021 model before loding the 2021 one, to free up memory
rm(var_model_merged)

#-------------------------------------------------------------------------------
# 2018 & 2021 IN SEPARATE TABLES
#-------------------------------------------------------------------------------
out_02 <- out %>%
  dplyr::select(group, lag, 
                state_legislators_2018,
                state_partisans_2018,
                state_media_2018,
                national_legislators_2018,
                national_media_2018,
                Trump_2018) %>%
  mutate(group = ifelse(!is.na(lag), paste0(group, " (", lag, ")"), 
                        as.character(group))) %>%
  dplyr::select(-lag)

out02_02 <- out02 %>%
  dplyr::select(group, lag, 
                state_legislators_2021,
                state_random_partisans_2021,
                state_partisans_2021,
                state_media_2021,
                national_legislators_2021,
                national_media_2021,
                Trump_2021) %>%
  mutate(group = ifelse(!is.na(lag), paste0(group, " (", lag, ")"), 
                        as.character(group))) %>%
  dplyr::select(-lag)


#--------------
# TABLE L1
#--------------
print(out_02 %>% as.data.frame())
 #                    group state_legislators_2018 state_partisans_2018 state_media_2018 national_legislators_2018 national_media_2018      Trump_2018
 #                Intercept        -3.087 (0.103)*      -1.802 (0.028)*   -2.267 (0.13)*           -1.187 (0.046)*     -1.687 (0.065)* -2.272 (0.133)*
 #   State Legislators (l1)          0.11 (0.003)*       0.003 (0.001)*   0.011 (0.004)*            0.006 (0.001)*      0.001 (0.002)  -0.003 (0.004) 
 #   State Legislators (l2)         0.042 (0.003)*      -0.006 (0.001)*  -0.009 (0.004)*           -0.008 (0.001)*     -0.009 (0.002)* -0.006 (0.004) 
 #   State Legislators (l3)         0.024 (0.003)*      -0.004 (0.001)*  -0.013 (0.004)*           -0.007 (0.001)*     -0.002 (0.002)       0 (0.004) 
 #   State Legislators (l4)         0.027 (0.003)*      -0.003 (0.001)*   0.002 (0.004)            -0.002 (0.001)      -0.003 (0.002)  -0.002 (0.004) 
 #   State Legislators (l5)         0.031 (0.003)*       0.002 (0.001)*   0.002 (0.004)             0.004 (0.001)*      0.006 (0.002)* -0.009 (0.004)*
 #     State Partisans (l1)         0.213 (0.013)*       0.435 (0.004)*   0.293 (0.016)*             0.14 (0.006)*      0.245 (0.008)*  0.248 (0.017)*
 #     State Partisans (l2)        -0.041 (0.014)*       0.076 (0.004)*   -0.05 (0.017)*           -0.053 (0.006)*      -0.02 (0.009)* -0.012 (0.018) 
 #     State Partisans (l3)        -0.024 (0.014)        0.058 (0.004)*  -0.059 (0.018)*           -0.009 (0.006)      -0.023 (0.009)*  0.004 (0.018) 
 #     State Partisans (l4)        -0.055 (0.014)*        0.04 (0.004)*  -0.013 (0.017)            -0.017 (0.006)*     -0.026 (0.009)* -0.028 (0.018) 
 #     State Partisans (l5)         0.013 (0.013)        0.074 (0.003)*   0.029 (0.016)            -0.004 (0.006)       0.039 (0.008)*   0.06 (0.016)*
 #         State Media (l1)         0.029 (0.003)*       0.007 (0.001)*    0.19 (0.003)*            0.012 (0.001)*      0.014 (0.002)*   0.01 (0.003)*
 #         State Media (l2)         0.008 (0.003)*       0.001 (0.001)    0.103 (0.003)*            0.001 (0.001)       0.004 (0.002)*      0 (0.003) 
 #         State Media (l3)         0.001 (0.003)       -0.003 (0.001)*   0.087 (0.003)*           -0.003 (0.001)*     -0.004 (0.002)* -0.005 (0.003) 
 #         State Media (l4)        -0.005 (0.003)       -0.001 (0.001)    0.076 (0.003)*           -0.004 (0.001)*     -0.004 (0.002)*  0.003 (0.003) 
 #         State Media (l5)        -0.004 (0.003)        0.002 (0.001)*   0.086 (0.003)*           -0.001 (0.001)           0 (0.002)  -0.006 (0.003) 
 # Members of Congress (l1)         0.195 (0.008)*       0.064 (0.002)*    0.079 (0.01)*            0.374 (0.003)*      0.116 (0.005)*   0.107 (0.01)*
 # Members of Congress (l2)        -0.034 (0.008)*      -0.044 (0.002)*   -0.068 (0.01)*            0.064 (0.004)*     -0.016 (0.005)* -0.038 (0.011)*
 # Members of Congress (l3)        -0.002 (0.008)       -0.018 (0.002)*    -0.05 (0.01)*            0.045 (0.004)*      0.013 (0.005)* -0.031 (0.011)*
 # Members of Congress (l4)        -0.013 (0.008)       -0.014 (0.002)*   -0.016 (0.01)             0.018 (0.004)*       0.01 (0.005)   0.022 (0.011)*
 # Members of Congress (l5)         0.009 (0.008)       -0.001 (0.002)     0.028 (0.01)*             0.08 (0.004)*      0.003 (0.005)    0.006 (0.01) 
 #      National Media (l1)         0.028 (0.005)*        0.03 (0.001)*   0.057 (0.007)*            0.027 (0.002)*      0.241 (0.003)*  0.033 (0.007)*
 #      National Media (l2)        -0.013 (0.005)*      -0.003 (0.001)   -0.016 (0.007)*            0.007 (0.002)*      0.044 (0.003)*  0.004 (0.007) 
 #      National Media (l3)        -0.014 (0.005)*           0 (0.001)   -0.009 (0.007)            -0.009 (0.002)*      0.032 (0.003)*  0.013 (0.007) 
 #      National Media (l4)         0.002 (0.005)            0 (0.001)        0 (0.007)             0.004 (0.002)       0.074 (0.003)* -0.011 (0.007) 
 #      National Media (l5)         0.003 (0.005)            0 (0.001)        0 (0.007)             0.006 (0.002)*      0.036 (0.003)* -0.004 (0.007) 
 #           President (l1)        -0.006 (0.003)*        0.01 (0.001)*  -0.002 (0.003)             0.003 (0.001)*      0.012 (0.002)*  0.174 (0.003)*
 #           President (l2)         0.001 (0.003)        0.003 (0.001)*   0.002 (0.003)             0.004 (0.001)*      0.006 (0.002)*  0.055 (0.003)*
 #           President (l3)         0.002 (0.003)       -0.001 (0.001)   -0.001 (0.003)             0.001 (0.001)       0.003 (0.002)   0.041 (0.003)*
 #           President (l4)         0.001 (0.003)        0.001 (0.001)   -0.003 (0.003)             0.006 (0.001)*          0 (0.002)    0.04 (0.003)*
 #           President (l5)             0 (0.003)       -0.002 (0.001)*       0 (0.003)            -0.008 (0.001)*     -0.004 (0.002)*  0.046 (0.003)*
 #                R Squared                  0.508                0.924            0.442                     0.734               0.746            0.38
 #       Adjusted R Squared                  0.506                0.924             0.44                     0.734               0.745           0.378
 #           Log Likelihood            -162507.531           -32850.058      -185223.873                -82161.795         -117013.599     -187719.374
 #                      AIC             325623.062            66308.117       371055.747                 164931.59          234635.197      376046.748
 #                      BIC             328513.061            69198.116       373945.746                167821.589          237525.196      378936.747

# - LaTeX code
print(xtable(out_02), include.rownames = FALSE)

#--------------
# TABLE L2
#--------------
print(out02_02 %>% as.data.frame())
#                    group state_legislators_2021 state_random_partisans_2021 state_partisans_2021 state_media_2021 national_legislators_2021 national_media_2021      Trump_2021
#                Intercept         -1.88 (0.119)*             -2.094 (0.071)*      -1.911 (0.028)*  -3.315 (0.087)*           -0.877 (0.046)*      -2.126 (0.08)*  -2.107 (0.21)*
#   State Legislators (l1)         0.122 (0.003)*              0.008 (0.002)*       0.003 (0.001)*   0.018 (0.002)*           -0.001 (0.001)       0.003 (0.002)    0.02 (0.006)*
#   State Legislators (l2)         0.046 (0.003)*             -0.002 (0.002)       -0.006 (0.001)*   0.006 (0.002)*            -0.01 (0.001)*     -0.006 (0.002)* -0.016 (0.006)*
#   State Legislators (l3)         0.029 (0.003)*             -0.001 (0.002)       -0.002 (0.001)*   0.001 (0.002)            -0.003 (0.001)*     -0.006 (0.002)* -0.016 (0.006)*
#   State Legislators (l4)         0.025 (0.003)*             -0.006 (0.002)*      -0.005 (0.001)*  -0.002 (0.002)            -0.004 (0.001)*     -0.012 (0.002)* -0.015 (0.006)*
#   State Legislators (l5)          0.03 (0.003)*                  0 (0.002)        0.002 (0.001)*   0.006 (0.002)*            0.001 (0.001)      -0.003 (0.002)       0 (0.006) 
#      General Public (l1)         0.041 (0.005)*              0.191 (0.003)*       0.018 (0.001)*   0.019 (0.004)*            0.002 (0.002)       0.009 (0.004)*   0.019 (0.01)*
#      General Public (l2)         0.007 (0.005)                0.13 (0.003)*      -0.007 (0.001)*   0.003 (0.004)            -0.011 (0.002)*     -0.001 (0.004)   -0.007 (0.01) 
#      General Public (l3)        -0.005 (0.005)               0.097 (0.003)*       0.003 (0.001)*   0.017 (0.004)*           -0.001 (0.002)       0.005 (0.004)    0.014 (0.01) 
#      General Public (l4)         0.017 (0.005)*              0.104 (0.003)*      -0.004 (0.001)*   0.003 (0.004)            -0.009 (0.002)*     -0.001 (0.004)   -0.002 (0.01) 
#      General Public (l5)         0.018 (0.005)*              0.118 (0.003)*       0.006 (0.001)*   0.026 (0.004)*            0.002 (0.002)      -0.007 (0.004)    0.026 (0.01)*
#     State Partisans (l1)         0.469 (0.015)*              0.295 (0.009)*       0.473 (0.004)*   0.254 (0.011)*            0.241 (0.006)*       0.314 (0.01)*  0.486 (0.027)*
#     State Partisans (l2)        -0.092 (0.016)*              -0.083 (0.01)*       0.026 (0.004)*  -0.031 (0.012)*           -0.043 (0.006)*     -0.031 (0.011)* -0.141 (0.029)*
#     State Partisans (l3)        -0.076 (0.016)*              -0.025 (0.01)*       0.041 (0.004)*  -0.044 (0.012)*            -0.02 (0.006)*      -0.02 (0.011)  -0.102 (0.029)*
#     State Partisans (l4)        -0.069 (0.016)*              -0.062 (0.01)*       0.027 (0.004)*  -0.057 (0.012)*           -0.041 (0.006)*     -0.059 (0.011)* -0.061 (0.029)*
#     State Partisans (l5)        -0.003 (0.015)              -0.035 (0.009)*       0.068 (0.004)*   0.032 (0.011)*           -0.009 (0.006)        0.016 (0.01)    -0.2 (0.027)*
#         State Media (l1)         0.058 (0.004)*              0.018 (0.003)*       0.011 (0.001)*   0.101 (0.003)*            0.014 (0.002)*      0.025 (0.003)*  0.044 (0.008)*
#         State Media (l2)         0.005 (0.004)              -0.002 (0.003)       -0.006 (0.001)*   0.037 (0.003)*           -0.005 (0.002)*      0.003 (0.003)  -0.017 (0.008)*
#         State Media (l3)         0.008 (0.004)               0.007 (0.003)*      -0.005 (0.001)*   0.042 (0.003)*           -0.003 (0.002)       0.001 (0.003)  -0.008 (0.008) 
#         State Media (l4)         0.008 (0.004)               0.005 (0.003)*      -0.001 (0.001)    0.025 (0.003)*            0.003 (0.002)       0.004 (0.003)  -0.029 (0.008)*
#         State Media (l5)         0.001 (0.004)               0.004 (0.003)        0.002 (0.001)*    0.04 (0.003)*            0.002 (0.002)       0.006 (0.003)*  -0.02 (0.008)*
# Members of Congress (l1)         0.106 (0.009)*              0.049 (0.006)*       0.066 (0.002)*    0.06 (0.007)*            0.317 (0.004)*       0.11 (0.006)*  0.247 (0.016)*
# Members of Congress (l2)         -0.036 (0.01)*             -0.028 (0.006)*      -0.023 (0.002)*  -0.015 (0.007)*            0.075 (0.004)*      0.002 (0.006)   0.073 (0.017)*
# Members of Congress (l3)          0.024 (0.01)*             -0.028 (0.006)*       0.012 (0.002)*   0.018 (0.007)*            0.069 (0.004)*      0.069 (0.006)* -0.043 (0.017)*
# Members of Congress (l4)         -0.031 (0.01)*              -0.03 (0.006)*      -0.014 (0.002)*       0 (0.007)             0.039 (0.004)*     -0.014 (0.006)*  0.004 (0.017) 
# Members of Congress (l5)         0.025 (0.009)*             -0.025 (0.006)*       0.009 (0.002)*   0.004 (0.007)             0.086 (0.004)*     -0.007 (0.006)   0.116 (0.016)*
#      National Media (l1)         0.003 (0.005)               0.007 (0.003)*       0.012 (0.001)*   0.026 (0.004)*            0.028 (0.002)*       0.17 (0.003)*  0.049 (0.009)*
#      National Media (l2)        -0.026 (0.005)*             -0.012 (0.003)*      -0.006 (0.001)*  -0.008 (0.004)*           -0.007 (0.002)*      0.053 (0.003)*  0.028 (0.009)*
#      National Media (l3)        -0.015 (0.005)*             -0.002 (0.003)       -0.003 (0.001)*   0.006 (0.004)             0.005 (0.002)*      0.048 (0.003)*  0.028 (0.009)*
#      National Media (l4)         0.001 (0.005)              -0.008 (0.003)*           0 (0.001)    0.003 (0.004)              0.02 (0.002)*      0.041 (0.003)*  0.003 (0.009) 
#      National Media (l5)         0.004 (0.005)                0.01 (0.003)*       0.007 (0.001)*   0.007 (0.004)             0.004 (0.002)*      0.044 (0.003)*  0.027 (0.009)*
#           President (l1)         0.007 (0.002)*              0.004 (0.001)*               0 (0)        0 (0.001)             0.008 (0.001)*     -0.002 (0.001)   0.147 (0.003)*
#           President (l2)        -0.002 (0.002)              -0.002 (0.001)           -0.002 (0)*  -0.003 (0.001)*           -0.002 (0.001)*     -0.003 (0.001)*  0.073 (0.003)*
#           President (l3)        -0.003 (0.002)              -0.002 (0.001)           -0.002 (0)*  -0.004 (0.001)*            0.003 (0.001)*     -0.005 (0.001)*  0.015 (0.003)*
#           President (l4)        -0.003 (0.002)               0.002 (0.001)*               0 (0)   -0.004 (0.001)*            0.002 (0.001)*      0.006 (0.001)*  0.057 (0.003)*
#           President (l5)         0.002 (0.002)               0.003 (0.001)*           0.004 (0)*   0.004 (0.001)*            0.006 (0.001)*      0.006 (0.001)*  0.077 (0.003)*
#                R Squared                 0.539                       0.716                0.926            0.636                     0.806               0.715           0.285
#       Adjusted R Squared                 0.538                       0.715                0.926            0.635                     0.806               0.714           0.283
#           Log Likelihood           -167431.752                 -116355.752           -23812.218      -135563.139                -72831.645         -127374.953     -223741.731
#                      AIC            335481.504                  233329.505            48242.435       271744.278                146281.289          255367.907      448101.462
#                      BIC            338419.883                  236267.884            51180.815       274682.658                149219.669          258306.286      451039.842


# - LaTeX code
print(xtable(out02_02), include.rownames = FALSE)