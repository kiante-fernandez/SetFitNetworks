### Citation

Fernandez, K. A., Karmarkar, U. R., & Krajbich, I. (2024).
[Preference centrality, but not set similarity, predicts choices between sets]

[Preprint]()

# Preference similarity, Set Choice

This repository hosts the code and supplementary materials for the paper "Preference centrality, but not set similarity, predicts choices between sets".

> Kianté A. Fernandez<sup>1</sup>, Uma R. Karmarkar<sup>2,3</sup>, & Ian Krajbich<sup>1</sup>  
> <sup>1</sup>Department of Psychology, University of California, Los Angeles  
> <sup>2</sup>School of Global Policy and Strategy, University of California, San Diego  
> <sup>3</sup>Rady School of Management, University of California, San Diego

## Abstract
Before selecting individual items, how do people choose between menus of items? Comparing sets is complex and may depend on their internal cohesion. Indeed, leading theories predict that people prefer sets with similar items. We test this with a computational approach that leverages network science to measure a novel form of similarity specific to economic choice. This "preference similarity" is defined as the strength of associations between items derived from correlations in their liking ratings. We find little evidence that people prefer sets with items that are similar to others in the set. Instead, we find that people prefer sets containing individual items that are highly central, i.e., items which generally have stronger associations with other items. Overall, we validate a quantitative tool for measuring similarity and show that while people prefer sets with items that are similar to many other items, they don't prefer more similar sets.

## Repository Contents
- `data/` - Datasets used in the study
- `src/` - Source code for analysis

The `src` directory contains the following analysis scripts:

### Network Analysis
- `exploratory_graph_analysis.R` - Network analysis for Lee et al. Ratings
- `fernandez_rating_network.R` -  Network analysis for Set-Choice Ratings
- `bakkour_rating_network.R` - Network analysis for Bakkour et al. Ratings
- `shenhav_rating_network.R` - Network analysis for Shenhav et al. Ratings

### Experimental Analysis
- `exp_1_network_difference_regression.R` - Regression analysis for Set-Choice Study 1
- `exp_2_network_difference_regression.R` - Regression analysis for Set-Choice Study 2
- `exp_3_network_difference_regression.R` - Regression analysis for Set-Choice Study 3
- `exp_2_similarity_rating.R` - Similarity rating analysis for Study 2
- `exp_3_similarity_rating.R` - Similarity rating analysis for Study 3
- `binary_choice_analysis.R` - Analysis of Single-Choice Study 1 & 2

### Utility Scripts
- `generate_image_group.R` - Script for generating stimulus sets for set choice experiments
- `subgraph_selection.R` - Functions for analyzing network subgraphs for generating experimental stimuli
- `internal_meta_analysis.R` - Meta-analysis across experiments
- `utils.R` - General utility functions

The `data` directory contains the following data files:

- Rating Study 1 (Lee & Holyoak 2021) - https://osf.io/x8bpa/
- Rating Study 2 (Leng & Shenhav, in prep) - please contact original authors
- Rating Study 3 (Li et al. 2023) - https://github.com/christineli0330/mem_dm_share
- Set-Choice Study 1 - in `data` folder  
- Set-Choice Study 2 - in `data` folder
- Set-Choice Study 3 - in `data` folder
- Single-Choice 1 (Lee & Hare 2023) -  https://osf.io/nepx5/
- Single-Choice 2 (Lee & Holyoak 2021) - https://osf.io/x8bpa/

## Requirements
- R (>= 4.0.2)
  - qgraph
  - igraph 
  - EGAnet
  - brms
  - NetworkToolbox

