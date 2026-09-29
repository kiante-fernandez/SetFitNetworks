# Preference centrality predicts choices between sets

Code and data for Fernandez, K. A., Karmarkar, U. R., & Krajbich, I. (2026). *Preference centrality predicts choices between sets* (submitted). Preprint: https://osf.io/preprints/psyarxiv/3fahj

## Layout

| Folder | Contents |
|---|---|
| `src/` | R analysis scripts |
| `data/` | Data for every dataset in the paper (de-identified) |
| `fits/`, `output/`, `results/` | Model fits, figures and tables; empty in the repository, filled by the scripts |

## Data

| Dataset | Source | In `data/` |
|---|---|---|
| Rating Study 1 | Lee & Holyoak (2021), https://osf.io/x8bpa/ | `lee_2021_rating1.csv` |
| Rating Study 2 | Leng et al. (2025) | `shengav_rating.csv`, `leng_2025/Study4_2.csv`, `Study5a_2.csv`, `Study5b_2.csv`, `Study6_2.csv` |
| Rating Study 3 | The Liking Initiative (Fernandez, Goyal, & Krajbich, 2026): https://doi.org/10.5281/zenodo.22216442, https://github.com/liking-initiative | `liking_initiative/` (the snapshot used here; use the Liking Initiative for current data) |
| Set-Choice Studies 1–3 | This paper (Studies 2–3 preregistered: https://osf.io/74qhv, https://osf.io/7wces) | `pilot_30/`, `exp_2/`, `exp_3/` (task files); `fernandez_202*_rating_exp*.csv` (ratings); `LowHighWithinBetween.RData`, `modularity_100_6.RData`, `average_strength_100_6.RData` (sets shown) |
| Binary-Choice Study 1 | Lee & Hare (2023), https://osf.io/nepx5/ | `Lee_Hare_2023_OSF/` |
| Binary-Choice Study 2 | Lee & Holyoak (2021), https://osf.io/x8bpa/ | `lee_2021_exp2_5_v2.csv` |
| Binary-Choice Study 3 | Smith & Krajbich (2018) | `smith_krajbich_2018/` |
| Multi-alternative Study 1 | Leng et al. (2025) | `leng_2025/Study3a_1.csv`, `Study3a_2.csv` |
| Multi-alternative Studies 2–3 | Fernandez et al. (in prep) | `choose_k/choosek_R.csv` (Study 2), `choose_k/exp_2_processed_V2.csv` (Study 3) |
| Multi-alternative Study 4 | Thomas et al. (2021) | `thomas2021/` |

Food item names and images are in `snackitemnames_nicholas/`. Set-Choice participants are numbered `S<study>_<nnn>` in their original order, and Prolific IDs are recoded as `P<nnnn>` (Leng et al.), `M<nnnn>` (Multi-alternative Studies 2–3) and `L<nnnn>` (Liking Initiative). Names, emails, payment handles and nationality were removed; no analysis uses them.

## Running

Requires R (≥ 4.0.2) and CmdStan (via [cmdstanr](https://mc-stan.org/cmdstanr/)):

```r
install.packages(c("tidyverse", "here", "igraph", "EGAnet", "qgraph", "NetworkToolbox", "SemNeT", "brms",
  "bayestestR", "BayesFactor", "tidybayes", "ggdist", "patchwork", "see", "ggcorrplot", "ggeffects",
  "performance", "report", "sjPlot", "broom", "broom.mixed", "lme4", "lmerTest", "simr", "readxl", "jsonlite",
  "clustAnalytics", "LaplacesDemon", "assortnet", "factoextra", "clue", "e1071", "gridExtra", "RColorBrewer",
  "jpeg", "progress", "rstantools", "knitr"))
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
```

Run scripts from the repository root (e.g. open `SetFitNetworks.Rproj`):

1. Networks: `exploratory_graph_analysis.R`, `fernandez_rating_network.R`, `shenhav_rating_network.R`, `rangel_rating_network_binary_choice_analysis.R`, then `thomas2021_item_mapping.R` and `thomas2021_network.R`.
2. `create_canonical_loadings.R` (PCA loadings applied to every network).
3. The analyses below.

Estimated networks and model fits are not in the repository; the scripts estimate them on the first run (several hours) and reuse them afterwards. `subgraph_selection.R` draws new random sets and overwrites the ones in `data/`, so it is not needed to reproduce the paper. Fig. 8 loads about 13 GB of model fits (peak ~9 GB RAM); with 16 GB or less, run it on its own: `Rscript src/figures_4_to_8.R 8`.

## Scripts by result

| Result | Script |
|---|---|
| Fig. 1; SI §1–3 (networks and their stability) | `exploratory_graph_analysis.R`, `shenhav_rating_network.R`, `rangel_rating_network_binary_choice_analysis.R` |
| Fig. 2; SI §4 (similarity judgments) | `exp_2_similarity_rating.R`, `exp_3_similarity_rating.R` |
| Set-choice regressions; SI §7–9 | `exp_1_network_difference_regression.R`, `exp_2_…`, `exp_3_…` |
| Meta-analysis estimates in the text | `internal_meta_analysis.R` |
| Figs. 4–8 | `figures_4_to_8.R` (e.g. `Rscript src/figures_4_to_8.R 7 8` for Figs. 7–8 only) |
| Centrality vs. liking | `centrality_vs_liking_all_studies.R` |
| Binary-Choice Studies 1–2 (SI §11) | `binary_choice_analysis.R` |
| Binary-Choice Study 3 (SI §11) | `rangel_rating_network_binary_choice_analysis.R` |
| Multi-alternative Studies 1–4 (SI §12) | `item_level_multi_alternative_analysis.R`, `item_level_rt_analysis.R` |
| SI §10 (set selection) | `subgraph_selection.R`, `generate_image_group.R` |
| SI §13 (cross-network centrality) | `centrality_validation_analysis.R` |
| SI §14 (within-set variance) | `set_variance_regression.R` |
| SI §15 (power analysis) | `exp_2_power_analysis.R`, `exp_3_power_analysis.R` |
| SI §16 (sensitivity) | `power_analysis_single_choice.R` |
| SI §18 (value parametrization) | `choice_value_parametrization.R`, `rt_individual_values_regression.R` |

`utils.R` and `apply_pca_weights.R` hold shared functions. Code is licensed under GPL-3.0.
