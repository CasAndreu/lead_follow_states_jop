################################################################################
# AppendixE_E1.R
# Paper:    Bottom Up? Top Down? Determinants of Issue-Attention in State 
#           Politics.
# Journal:  Journal of Politics.
# Authors:  Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, 
#           Richard Bonneau, and Jonathan Nagler.
# Purpose:  To replicate Figure E1 in Appendix E of the paper.
# Data In:  
#           1. Dataset with avg attention to each CAP topic by party
# Data Out:
#           1. Figure E1
################################################################################


#===============================================================================
# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(gridExtra)

#===============================================================================
# DATA
#===============================================================================
# - load dataset with average attention paid to each CAP topic, by party, in
#   2018 and 2021 combined
db <- read.csv("./data/avg-issue-attention-by-party-grand-means.csv")

#===============================================================================
# MAIN
#===============================================================================

#-------------------------------------------------------------------------------
# [ A ] A plot showing the average daily attention by each party. 
#-------------------------------------------------------------------------------
plotdb_a <- db %>%
  dplyr::select(-D_over_R, -R_over_D) %>%
  gather(party, value, -Issue) %>%
  arrange(party, value) %>%
  mutate(Issue = gsub("\\.", " ", Issue),
         Issue = recode(Issue,
                        `Gov  Operations` = "Gov. Operations",
                        `Intl  Affairs` = "Intl. Affairs"),
         Issue = factor(Issue, levels = unique(Issue)))

plot_a <- ggplot(plotdb_a,
                 aes(x = Issue, y = value, color = party)) +
  geom_point(size = 4, pch = 1) +
  geom_point(size = 4, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  coord_flip() +
  scale_y_continuous("\nAvg. issue daily attention (%) by Party (2018 & 2021 combined)",
                     breaks = seq(0, 0.15, 0.025),
                     labels = paste(seq(0, 15, 2.5), "%")) +
  scale_x_discrete("") +
  scale_color_manual("", values = c("blue4", "red3")) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


#-------------------------------------------------------------------------------
# [ B ] A plot showing relative party diff by issue
#-------------------------------------------------------------------------------
db <- db %>%
  mutate(Issue = gsub("\\.", " ", Issue),
         Issue = recode(Issue,
                        `Gov  Operations` = "Gov. Operations",
                        `Intl  Affairs` = "Intl. Affairs"),
         owner = ifelse(R_over_D > 1.2, "Republican Issue", 
                        ifelse(R_over_D < 0.8, "Democratic Issue", "None"))) %>%
  arrange(R_over_D) %>%
  mutate(Issue = factor(Issue, levels = Issue))

plot_b <- ggplot(db,
                 aes(x = Issue, y = R_over_D, color = owner)) +
  geom_point(size = 4, pch = 1) +
  geom_point(size = 4, alpha = 0.5) +
  geom_hline(yintercept = 1, color = "black") +
  geom_hline(yintercept = 1.2, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.8, color = "black", linetype = "dashed") +
  coord_flip() +
  scale_y_continuous("\nRatio: Republican/Democrat issue attention") +
  scale_x_discrete("") +
  scale_color_manual("", values = c("blue4", "gray", "red3")) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


# Save the figure
ggsave("./figures/figureE1.png", plot = grid.arrange(plot_a, plot_b, ncol=2),
       width = 16, height = 6)
