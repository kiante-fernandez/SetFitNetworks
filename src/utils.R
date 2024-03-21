# utils

library(purrr) # Functional Programming Tools
suppressPackageStartupMessages(library(tidyverse)) # Easily Install and Load the 'Tidyverse'
library(jsonlite) # A Simple and Robust JSON Parser and Generator for R

# helper functions for working with lists
list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}
list.cbind <- function(.data) {
  list.do(.data, "cbind")
}

load_food_names <- function() {
  # load all the images to calculate the value for a group of foods
  food_folder <- here::here("data", "snackitemnames_nicholas", "Lee_Holyoak_2021_images")
  FoodNames <- readxl::read_excel(here::here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
  temp <- list.files(path = food_folder, pattern = "*.jpg", full.names = T)
  foods_in_image <- stringr::str_extract(temp, "item\\d+")
  foods_in_image <- stringr::str_extract(foods_in_image, "\\d+")
  # get row idx for each of the image numbers
  foods_in_image <- tibble::rowid_to_column(data.frame(Image = as.numeric(foods_in_image)))
  foods_in_image <- dplyr::left_join(FoodNames, foods_in_image, "Image")
  return(list(FoodNames = FoodNames, foods_in_image = foods_in_image))
}


NetworkStat <- function(subgraph) {
  ## %######################################################%##
  #                                                          #
  ### calculate a bunch of micro and mesoscale measures#####
  #                                                          #
  ## %######################################################%##
  G <- subgraph
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")

  net_stat_temp <- data.frame(
    degree = degree(subgraph, normalized = TRUE),
    strength = strength(subgraph),
    eigen = igraph::eigen_centrality(G)$vector,
    weighted_transitivity = transitivity(subgraph, type = "weighted"),
    closeness = igraph::closeness(G, normalized = TRUE, cutoff = -1),
    betweenness = betweenness(G, normalized = TRUE),
    participation = NetworkToolbox::participation(adj_temp, comm = V(subgraph)$snack_type)$overall
  ) %>%
    tibble::rownames_to_column("Name")

  return(net_stat_temp)
}

calculate_net_stats <- function(g) {
  ## another version of calculating the netstats of various measures for a graph
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  path_lengths <- distances(G)
  diag(path_lengths) <- NA # path length to oneself is zero

  adj_temp <- igraph::as_adjacency_matrix(g, sparse = F, attr = "weight")

  # here I calculate a range of metrics on the graph
  net_degree <- data.frame(
    degree = degree(g, normalized = TRUE),
    strength = strength(g),
    eigen = igraph::eigen_centrality(G)$vector,
    weighted_transitivity = transitivity(g, type = "weighted"),
    closeness = igraph::closeness.estimate(G, normalized = TRUE, cutoff = -1),
    betweenness = betweenness(G, normalized = TRUE),
    sd = apply(cor_snack_food, 2, sd),
    precision = apply(cor_snack_food, 2, function(.){LaplacesDemon::var2prec(var(.))})
  ) %>%
    tibble::rownames_to_column("Name") %>%
    dplyr::left_join(load_food_names()$foods_in_image, "Name")

  net_degree$snack_type <- V(g)$snack_type
  
  dat_pca <- net_degree[,c("degree","strength","eigen","weighted_transitivity","closeness","betweenness")]
  # dat_pca <- net_degree[,c("strength","eigen","weighted_transitivity","closeness")]
  rownames(dat_pca) <- net_degree$Name

  pca_res <- prcomp(dat_pca, center = TRUE, scale. = TRUE)
  # pca_res <- princomp(dat_pca)
  
  #try to use another type of PCA functions with more 
  #ablilties to change things 
  print(pca_res)
  # summary(pca_res)
  # library("factoextra")
  # scree_p <- fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 70))+
  #   theme_classic()+
  #   theme(
  #     axis.text = element_text(face = "bold"),
  #     text = element_text(size = 15),
  #     axis.title = element_text(face = "bold")
  #   )+labs(x = "PC")
  # pc_plt <- fviz_pca_var(pca_res, col.var = "black")+
  #   theme_classic()+
  #   theme(
  #     axis.text = element_text(face = "bold"),
  #     axis.text.x = element_text(face="bold", size=14),
  #     text = element_text(size = 15),
  #     axis.title = element_text(face = "bold")
  #   ) + labs(x = "PC1", y = "PC2", title = "")
  # # fviz_pca_var(pca_res, col.var = "contrib",
  # #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  # fviz_contrib(pca_res, choice = "var", axes = 1, ylim = c(0, 40))+
  #   theme_classic()+
  #   theme(
  #   axis.text = element_text(face = "bold"),
  #   axis.text.x = element_text(face="bold", size=14),
  #   text = element_text(size = 15),
  #   axis.title = element_text(face = "bold")
  # )+
  #   scale_x_discrete(labels=c("weighted_transitivity" = "transitivity"))+
  #   labs(x = "centrality measure", title = "PC1")
  # fviz_contrib(pca_res, choice = "var", axes = 2, ylim = c(0, 40))+
  #   theme_classic()+
  #   theme(
  #     axis.text = element_text(face = "bold"),
  #     axis.text.x = element_text(face="bold", size=14),
  #     text = element_text(size = 15),
  #     axis.title = element_text(face = "bold")
  #   )+
  #   scale_x_discrete(labels=c("weighted_transitivity" = "transitivity"))+
  #   labs(x = "centrality measure", title = "PC2")
  # 
  # pc_nodes <- fviz_pca_ind(pca_res, col.ind = net_degree$snack_type,
  #              gradient.cols = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  #                          "#A6761D"),
  #              repel = TRUE # Avoid text overlapping (slow if many points)
  # )+ theme_classic()+ theme(
  #   axis.text = element_text(face = "bold"),
  #   text = element_text(size = 15),
  #   axis.title = element_text(face = "bold"),
  #   legend.position = "none"
  # )+    labs(x = "PC1", y = "PC2", title = "")
  # (scree_p + pc_plt)/ pc_nodes + plot_annotation(tag_levels = "A")
  # fviz_pca_ind(pca_res, col.ind = net_degree$snack_type,
  #              gradient.cols = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"),
  #              repel = TRUE # Avoid text overlapping (slow if many points)
  # )+ theme_classic()+ theme(
  #   axis.text = element_text(face = "bold"),
  #   text = element_text(size = 15),
  #   axis.title = element_text(face = "bold"),
  #   legend.position = "none"
  # )+    labs(x = "PC1", y = "PC2", title = "")

  print(summary(pca_res))
  # net_degree$PCA1 <-  pca_res$x[,1] * -1 #change the scale w/ linear transformation
  net_degree$PCA1 <-  pca_res$x[,1] #change the scale w/ linear transformation
  net_degree$PCA2 <- pca_res$x[,2]
  net_degree$PCA3 <- pca_res$x[,3]
  net_degree$PCA4 <-  pca_res$x[,4]
  net_degree$PCA5 <- pca_res$x[,5]
  net_degree$PCA6 <- pca_res$x[,6]
  
  # net_degree$PCA1 <-  pca_res$scores[,1]
  # net_degree$PCA2 <- pca_res$scores[,2]
  # net_degree$PCA3 <- pca_res$scores[,3]
  # net_degree$PCA4 <-  pca_res$scores[,4]
  # net_degree$PCA5 <- pca_res$scores[,5]
  # net_degree$PCA6 <- pca_res$scores[,6]
  
  cor_p <- net_degree %>% 
    dplyr::select(degree,strength,eigen,weighted_transitivity,closeness,betweenness,PCA1, PCA2, PCA3, PCA4, PCA5, PCA6) %>% 
    cor() %>% 
    ggcorrplot::ggcorrplot(type = "upper",
                           lab = TRUE)+
    theme_classic()+
    labs(x = "", y = "") +
    theme(
      axis.text = element_text(face = "bold"),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(cor_p)
  return(net_degree)
}

# get correlations between items
lee_2021_rating1 <- readr::read_csv(here::here("data", "lee_2021_rating1.csv"), col_names = load_food_names()$FoodNames$Name)
cor_snack_food <- SemNeT::similarity(lee_2021_rating1, method = "cor")
cor_snack_food <- data.frame(matrix(cor_snack_food[cor_snack_food != 1], 59, 60))

create_dataset <- function(df, type, standardized = TRUE) {
  # creates the regressors for the analysis and can scale all the variables

  if (type == "choice") {
    model_dat <- df %>%
      exlusions() %>%
      group_by(subject_id) %>%
      mutate(
        nd = scale(left_net1 - right_net1, center = standardized, scale = standardized),
        vd = scale(left_rating - right_rating, center = standardized, scale = standardized),
        sd = scale(left_sim - right_sim, center = standardized, scale = standardized),
        ov = scale(left_rating + right_rating, center = standardized, scale = standardized),
        on = scale(left_net1 + left_net1, center = standardized, scale = standardized),
        os = scale(left_sim + right_sim, center = standardized, scale = standardized),
        zleft_rating = scale(left_rating, center = standardized, scale = standardized),
        zright_rating = scale(right_rating, center = standardized, scale = standardized),
        zleft_net1 = scale(left_net1, center = standardized, scale = standardized),
        zright_net1 = scale(right_net1, center = standardized, scale = standardized),
        zleft_net2 = scale(left_net2, center = standardized, scale = standardized),
        zright_net2 = scale(right_net2, center = standardized, scale = standardized),
        zleft_sim = scale(left_sim, center = standardized, scale = standardized),
        zright_sim = scale(right_sim, center = standardized, scale = standardized),
        zleft_sd = scale(left_sd, center = standardized, scale = standardized),
        zright_sd = scale(right_sd, center = standardized, scale = standardized)
      ) %>%
      ungroup() %>%
      select(subject_id, choice, rt, nd, vd, sd, ov, on, os, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2, zleft_sim, zright_sim, zleft_sd, zright_sd)
  } else if (type == "correct/rt") {
    c = 0
    model_dat <- df %>%
      exlusions() %>%
      group_by(subject_id) %>%
      #add constant
      # mutate(left_net = left_net + c,
      #        right_net = right_net + c) %>% 
      mutate( # take absolute value
        nd1 = scale(abs(left_net1 - right_net1), center = standardized, scale = standardized),
        nd2 = scale(abs(left_net2 - right_net2), center = standardized, scale = standardized),
        vd = scale(abs(left_rating - right_rating), center = standardized, scale = standardized),
        sd = scale(abs(left_sim - right_sim), center = standardized, scale = standardized),
        ov = scale(left_rating + right_rating, center = standardized, scale = standardized),
        on = scale(left_net1 + left_net1, center = standardized, scale = standardized),
        os = scale(left_sim + right_sim, center = standardized, scale = standardized)
      ) %>%
      select(subject_id, choice,correct, rt, vd, nd1, nd2, sd, ov, on, os)
  }
  return(model_dat)
}

estimate_brms <- function(df, outcome = "choice") {
  #
  # TODO just create a folder for each network statistic, then add an argument that places each model in what ever name you write
  #     create a error too. if the folder name does not exist in the directory then throw an error and don't run the models 'could not find folder to save models'
  if (outcome == "choice") {
    df_temp <- create_dataset(df, type = "choice")
    # base model
    # model1 <- brm(choice ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_choice01"))
    # add network difference
    # model2A <- brm(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 + zleft_rating + zright_rating + zleft_net + zright_net | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_choice02A"))
    # add similarity difference
    # model2B <- brm(choice ~ zleft_rating + zright_rating + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating + zleft_sim + zright_sim | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_choice02B"))
    # both
    # model3 <- brm(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating + zleft_net + zright_net + zleft_sim + zright_sim | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_choice03"))
    # interactions
    model4 <- brm(choice ~ (zleft_rating * zleft_net) + (zright_rating * zright_net) + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating + zleft_net + zright_net + zleft_sim + zright_sim | subject_id), data = df_temp, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_choice04"))

    # return(list(model1, model2A, model2B, model3, model4))
    return(list(model4))
    
  } else if (outcome == "correct") {
    # the coded as correct models (which take the absolute value for the regressors)
    model1 <- brm(correct ~ vd + ov + (1 | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct01"))
    # add network difference
    model2A <- brm(correct ~ vd + ov + nd + (1 | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct02A"))
    model2B <- brm(correct ~ vd + ov + sd + (1 | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct02B"))
    # add similarity difference
    model3 <- brm(correct ~ vd + ov + nd + sd + (1 | subject_id), data = df, family = "bernoulli", cores = 4, iter = 10000, file = here::here("fits", "fit_correct03"))
    
    return(list(model1, model2A, model2B, model3))
    
  } else if (outcome == "rt") {
    df_temp <- create_dataset(df, type = "correct/rt")

    # the coded as response time models (which take the absolute value for the regressors)
    # model1 <- brm(log(rt) ~ vd + ov + (vd + ov | subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_rt01"))
    # add network difference
    # model2A <- brm(log(rt) ~ vd + ov + nd + (vd + ov + nd | subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_rt02A"))
    # model2B <- brm(log(rt) ~ vd + ov + sd + (vd + ov + sd | subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_rt02B"))
    # add similarity difference
    model3 <- brm(log(rt) ~ vd + ov + nd + sd + (vd + ov + nd + sd | subject_id), data = df_temp, cores = 4, iter = 10000, file = here::here("fits", "exp_2_fit_rt03"))

    # return(list(model1, model2A, model2B, model3))
    return(list(model3))
    
  }
}

estimate_mlms <- function(df, outcome = "choice") {
  # estimate the mixed effect regressions using lme4 package
  if (outcome == "choice") {
    df_temp <- create_dataset(df, type = "choice")
    # base model
    model1 <- glmer(choice ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating| subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # add network difference
    model2A <- glmer(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 + zleft_rating + zright_rating| subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # add similarity difference
    model2B <- glmer(choice ~ zleft_rating + zright_rating + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # add both
    model3 <- glmer(choice ~ zleft_rating + zright_rating + zleft_net + zright_net + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

    model4 <- glmer(choice ~ (zleft_rating * zleft_net) + (zright_rating * zright_net) + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

    # model5 <- glmer(choice ~ (zleft_rating*zleft_net) +  (zright_rating*zright_net) + zleft_sim + zright_sim + zleft_sd + zright_sd + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    
    # return(list(model1, model2, model3, model4)) 
    return(list(model1, model2A, model2B, model3, model4))
    # return(list(model1, model2A, model2B, model3, model4, model5))
    
  } else if (outcome == "correct") {
    # base model
    df <- create_dataset(df, type = "correct/rt")
    model1 <- glmer(correct ~ vd + ov + (1 | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2A <- glmer(correct ~ vd + ov + nd + (1 | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    model2B <- glmer(correct ~ vd + ov + sd + (1 | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add similarity difference
    model3 <- glmer(correct ~ vd + ov + nd + sd + (1 | subject_id), data = df, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

    return(list(model1, model2A, model2B, model3))
    
  } else if (outcome == "rt") {
    df_temp <- create_dataset(df, type = "correct/rt")
    # df_temp <- create_dataset(df, type = "choice")
    
    # df = df[df$correct == 1,] #check only correct
    # base model
    model1 <- lmer(log(rt) ~ vd + ov + (1 + vd + ov | subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add network difference
    model2A <- lmer(log(rt) ~ vd + ov + nd + (1 + vd + ov| subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    # add similarity difference
    model2B <- lmer(log(rt) ~ vd + ov + sd + (1 + vd + ov| subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

    model3 <- lmer(log(rt) ~ vd + ov + nd + sd + (1 + vd + ov| subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

    return(list(model1, model2A, model2B, model3))
    
    # model1 <- lmer(log(rt) ~ zleft_rating + zright_rating + (1 + zleft_rating + zright_rating| subject_id), data = df_temp , control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # # add network difference
    # model2A <- lmer(log(rt) ~ zleft_rating + zright_rating + zleft_net + zright_net + (1 + zleft_rating + zright_rating| subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # # add similarity difference
    # model2B <- lmer(log(rt) ~ zleft_rating + zright_rating + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # # add both
    # model3 <- lmer(log(rt) ~ zleft_rating + zright_rating + zleft_net + zright_net + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # 
    # model4 <- lmer(log(rt) ~ (zleft_rating * zleft_net) + (zright_rating * zright_net) + zleft_sim + zright_sim + (1 + zleft_rating + zright_rating | subject_id), data = df_temp, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # 
    # model5 <- lmer(log(rt) ~ (zleft_rating*zleft_net) +  (zright_rating*zright_net) + zleft_sim + zright_sim + zleft_sd + zright_sd + (1 + zleft_rating + zright_rating | subject_id), data = df_temp,control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
    # 
    # return(list(model1, model2, model3, model4)) 
    # return(list(model1, model2A, model2B, model3, model4, model5))
  }
}

generate_table <- function(ms, type, net_stat, save = F) {
  # create proper file name with the filename
  file_name <- here::here("tables", paste0(type, "_", net_stat, ".html"))

  title <- paste0("mixed model for ", type)

  if (save == TRUE) {
    table_temp <- modelsummary(
      ms,
      title = title,
      fmt = 2,
      shape = term ~ model + statistic,
      estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
      statistic = "p.value",
      coef_omit = "Intercept",
      gof_omit = "RMSE|ICC|R2 Cond.",
      output = file_name
    )
  } else {
    table_temp <- modelsummary(
      ms,
      title = title,
      fmt = 2,
      shape = term ~ model + statistic,
      estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
      statistic = "p.value",
      coef_omit = "Intercept",
      gof_omit = "RMSE|ICC|R2 Cond.",
      output = "gt"
    )
  }
  return(table_temp)
}

extract_edge_list <- function(g, filename = NULL) {
  # Get edge list and weights
  edge_list <- igraph::get.edgelist(g)
  weights <- E(g)$weight
  # Combine edge list with weights
  edge_list_weights <- as.data.frame(cbind(edge_list, weights))
  colnames(edge_list_weights) <- c("start_node", "end_node", "weight")
  edge_list_weights$weight <- as.numeric(edge_list_weights$weight)
  if (!is.null(filename)) {
    write.csv(edge_list_weights, "edge_list.csv", row.names = FALSE)
  }
}

