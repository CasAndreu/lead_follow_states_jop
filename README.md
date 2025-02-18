# Who Influences The Public Agenda in State Politics? Evidence From 45 Million Twitter Messages

This repository contains the replication code for the paper _"Bottom Up? Top Down? Determinants of Issue-Attention in State Politics"_, authored by Andreu Casas, Oscar Stuhler, Julia Payson, Joshua A. Tucker, Richard Bonneau, and Jonathan Nagler.

> __Abstract__:
> Who shapes the issue-attention cycle of state legislators? Although state governments make critical policy decisions, data and methodological constraints have limited researchers' ability to study state-level agenda setting. For this paper, we collect more than 122 million Twitter messages sent by state and national actors in 2018 and 2021. We then employ supervised machine learning and time series techniques to study how the issue-attention of state lawmakers evolves vis-à-vis various local- and national-level actors. Our findings suggest that state legislators operate at the confluence of national and local influences. In line with arguments highlighting the nationalization of state politics, we find that state legislators are consistently responsive to policy debates among members of Congress. However, despite growing nationalization concerns, we also find strong evidence of issue responsiveness by legislators to members of the public in their states and moderate responsiveness to regional media sources.

Here a [pdf copy of the paper](https://github.com/SMAPPNYU/lead_follow_states/blob/main/LeadFollowStatesPolitics-7may2021.pdf) to be submitted. [TO BE UPDATED!]

## Downloading Large Files from Google Drive

A few files are too large to be stored in this repository. You can find them in the following Google Drive: https://drive.google.com/drive/folders/1gLJ-GY44JeyDbsllqrj9NJ65rNSXeDmq?usp=sharing. Before running any code, you should download the files in there and place them in the following directories:
  
  - Place the following file under the `data` directory: `tweet-level-data-for-all-groups-but-partisans.csv`. 
  - Place the following file under the `models` directory: `MODEL_III_RESULT_LISTS-ORIGINAL.Rdata`.

## Data

- Raw datasets that we pre-process to generate the data to which we fit the VAR models:

  - `partisans-day-party-level-dataset.csv`: Party-day-level dataset for "state partisans". This data has already been aggregated at the party-day level in order to anonymize data for these non-public individuals. I contains information about the daily attention paid by these partisans to each of the 22 issues of analysis.
  - `tweet-level-data-for-all-groups-but-partisans.csv`: Tweet-level dataset with information about the the Pr of the tweet being about each of the 22 issues we study, plus additional information about who sent the tweet. Contains tweets for 5 of the 6 groups we study: state legislators, national legislators, state media, Trump, and national media.

- Datasets ready to be used for descriptive analysis and fitting the VAR models:

  - `avg-daily-att-per-issue-and-group.csv`: used to generate descriptive Figure 2 in the paper.
  - `group-day-issue-level-dataset-01.csv`: used to fit the first and third VAR model, reported in Figure 4 and Figure 6.
  - `group-day-issue-level-dataset-02.csv`: used to generate descriptive Figure 3 in the paper.
  - `group-day-issue-level-dataset-03.csv`: used to fit the second VAR model, reported in Figure 5.

> _Note_: In the code we show how to generate this second set of pre-processed datasets. To avoid fully over-writing the "original" datasets, we duplicated each of these datasets: see the `*-ORIGINAL.CSV` versions in the data directory.

## Pre-processing code

- [01-generating-group-day-issue-level-dataset-for-model01.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-preprocessing/01-generating-group-day-issue-level-dataset-for-model01.R): generates `group-day-issue-level-dataset-01.csv` from `partisans-day-party-level-dataset.csv` and `tweet-level-data-for-all-groups-but-partisans.csv`.
- [02-calculating-avg-daily-att-per-issue-and-group.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-preprocessing/02-calculating-avg-daily-att-per-issue-and-group.R): generates `avg-daily-att-per-issue-and-group.csv` from `partisans-day-party-level-dataset.csv` and `tweet-level-data-for-all-groups-but-partisans.csv`.
- [03-generating-group-day-level-dataset-for-figure03.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-preprocessing/03-generating-group-day-level-dataset-for-figure03.R): generates `group-day-issue-level-dataset-02.csv` from `partisans-day-party-level-dataset.csv` and `tweet-level-data-for-all-groups-but-partisans.csv`.
- [04-generating-group-day-level-dataset-for-model02.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-preprocessing/04-generating-group-day-level-dataset-for-model02.R): generates `group-day-issue-level-dataset-03.csv` from `partisans-day-party-level-dataset.csv` and `tweet-level-data-for-all-groups-but-partisans.csv`.

## Code to generate the figures

- [00-functions.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-figures/00-functions.R): a set of functions used in the other figure-generating scripts.

- [01-figure02.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-figures/01-figure02.R): code to generate Figure 2 in the paper.

<img src = "https://github.com/SMAPPNYU/lead_follow_states/blob/main/figures/figure02-ORIGINAL.png">

- [02-figure03.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-figures/02-figure03.R): code to generate Figure 3 in the paper.

<img src = "https://github.com/SMAPPNYU/lead_follow_states/blob/main/figures/figure03-ORIGINAL.png">

- [03-figure04.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-figures/03-figure04.R): code to generate Figure 4 in the paper.

<img src = "https://github.com/SMAPPNYU/lead_follow_states/blob/main/figures/figure04-ORIGINAL.png">

- [04-figure05.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-figures/04-figure05.R): code to generate Figure 5 in the paper.

<img src = "https://github.com/SMAPPNYU/lead_follow_states/blob/main/figures/figure05-ORIGINAL.png">

- [05-figure06.R](https://github.com/SMAPPNYU/lead_follow_states/blob/main/code-figures/05-figure06.R): code to generate Figure 6 in the paper.

<img src = "https://github.com/SMAPPNYU/lead_follow_states/blob/main/figures/figure06-ORIGINAL.png">

## Figures

- Directory storing the figures, so where the output of the figure-generating code is placed. 

> _Note_: To also avoid fully over-writing the "original" figure files, we placed an `*-ORIGINAL.png` version of each figure in there.

## Models

- Directory storing the VAR models and the resulting IRFs. In the figure-generating scripts we offer the option to estimate the VAR models from scratch, or to use a pre-trained/saved version of the models (`*-ORIGINAL.Rdata`) to generate the figures without having to spend computing time estimating the VAR models on your own. So this directory contains these pre-saved versions of the models, as well as it serves as output directory for the figure-generating scripts to place the freshly estimated VARs/IRFs.
