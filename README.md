# Bottom Up? Top Down? Determinants of Issue-Attention in State Politics

This repository contains the replication code for the paper _"Bottom Up? Top Down? Determinants of Issue-Attention in State Politics"_, authored by Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, Richard Bonneau, and Jonathan Nagler; and conditionally accepted for publication at _The Journal of Politics_. 

> __Abstract__:
> Who shapes the issue-attention cycle of state legislators? Although state governments make critical policy decisions, data and methodological constraints have limited researchers' ability to study state-level agenda setting. For this paper, we collect more than 122 million Twitter messages sent by state and national actors in 2018 and 2021. We then employ supervised machine learning and time series techniques to study how the issue-attention of state lawmakers evolves vis-à-vis various local- and national-level actors. Our findings suggest that state legislators operate at the confluence of national and local influences. In line with arguments highlighting the nationalization of state politics, we find that state legislators are consistently responsive to policy debates among members of Congress. However, despite growing nationalization concerns, we also find strong evidence of issue responsiveness by legislators to members of the public in their states and moderate responsiveness to regional media sources.

## Downloading Large Files from Google Drive

A few files are too large to be stored in this repository. You can find them in Google Drive. Before running any code, you should download all files from the [models](https://drive.google.com/drive/folders/15V-mTaaCA1bcDYnG4S9oz46KkBj-sSDj) folder, and place them in the `models` directory of this repository. Additionally, you also need to download the following large file from the `data` folder, and place it in the [data](tweet-level-topic-preds-all-tweets-state-legislators-2018-2021.csv) directory in this respository: `tweet-level-topic-preds-all-tweets-state-legislators-2018-2021.csv`.   

## Data

- Time series with information about the attention the groups under analysis devoted to each topic.

  - `group-day-issue-level-dataset-01-2018.csv` & `group-day-issue-level-dataset-01-2021.csv`: day-issue-state-level time series, for 2018 and 2021. All members of Congress pooled together.
  - `group-day-issue-level-dataset-02-2018.csv` & `group-day-issue-level-dataset-02-2021.csv`: day-issue-state-level time series, for 2018 and 2021. Only members of Congress from the same state pooled together.
  - `group-day-issue-level-dataset-03-2018.csv` & `group-day-issue-level-dataset-03-2021.csv`: day-issue-state-level time series, for 2018 and 2021. Only members of Congress from the same state  pooled together. Distinguishing members of Congress, state legislators, and state partisans, by party ID.

- `annotated-tweets-no-text-no-author.csv`: tweet id and topic codes for the 19,949 tweets manually annotated for this project for training the machine learning topic classifiers.
- `avg-daily-att-per-issue-and-group-2018.csv` & `avg-daily-att-per-issue-and-group-2021.csv`: average daily attention that groups under analysies devoted to each topic in 2018 and 2021.
- `avg-issue-attention-by-party-grand-means.csv`: average daily attention that Democrats and Republicans devoted to each topic, 2018 and 2021 combined.
- `bills-with-topic-predictions.csv`: predicted topic probabilities for bills introduced in state legislatures during the period under analysis.
- `extended_bills_df_ALL-SIMPLIFIED.csv`: id, title, state, date, and description, for the bills introduced in state legislatures during the period under analysis.
- `state-legislators-mentions-by-partisans-summary.csv`: information about how often other gropus under anlaysis mention state legislators in their tweets.
- `state-legislators-network-connections-data.csv`: information about how state legislators are connected on Twitter to the other groups under anlaysis.

## Code (Main Paper)

- [00-functions.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/00-functions.R): a set of functions used in the other figure-generating scripts.

- [01-table02.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/01-table02.R): code to generate Table 2 of the paper.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/table02.png">

- [02-table04.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/02-table04.R): code to generate Table 4 of the paper.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/table04.png">

- [03-figure01.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/03-figure01.R): code to generate Figure 1 of the paper.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figure01.png">

- [04-figure02.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/04-figure02.R): code to generate Figure 2 of the paper.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figure04.png">

- [05-figure03.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/05-figure03.R): code to generate Figure 3 of the paper.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figure03.png">

- [06-figure04.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/06-figure04.R): code to generate Figure 4 of the paper.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figure04.png">

## Code (Online Appendix)

- [AppendixA.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/AppendixA.R): code to generate Tables A2-A6 in Appendix A.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/tableA2.png">
<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/tableA3.png">
<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/tableA4.png">
<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/tableA5.png">
<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/tables/tableA6.png">

- [AppendixC.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/AppendixC.R): code to generate Figure C1 in Appendix C.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figureC1.png">

- [AppendixD.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/AppendixD.R): code to generate Figure D1 in Appendix D.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figureD1.png">

- [AppendixE.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/AppendixE.R): code to generate Figures E1 and E2 in Appendix E.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figureE1.png">
<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figureE2.png">

- [AppendixF.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/AppendixF.R): code to generate Figures F1 in Appendix F.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figureF1.png">

- [AppendixF.R](https://github.com/CasAndreu/lead_follow_states_jop/blob/main/code/AppendixF.R): code to generate Figures F1 in Appendix F.

<img src = "https://github.com/CasAndreu/lead_follow_states_jop/blob/main/figures/figureF1.png">





## Figures

- Directory storing the figures, so where the output of the figure-generating code is placed. 

> _Note_: To also avoid fully over-writing the "original" figure files, we placed an `*-ORIGINAL.png` version of each figure in there.

## Models

- Directory storing the VAR models and the resulting IRFs. In the figure-generating scripts we offer the option to estimate the VAR models from scratch, or to use a pre-trained/saved version of the models (`*-ORIGINAL.Rdata`) to generate the figures without having to spend computing time estimating the VAR models on your own. So this directory contains these pre-saved versions of the models, as well as it serves as output directory for the figure-generating scripts to place the freshly estimated VARs/IRFs.
