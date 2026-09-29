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
        zright_sd = scale(right_sd, center = standardized, scale = standardized),
        zleft_num_community = scale(left_num_community, center = standardized, scale = standardized),
        zright_num_community = scale(right_num_community, center = standardized, scale = standardized),
        zleft_var = scale(left_VAR, center = standardized, scale = standardized),
        zright_var = scale(right_VAR, center = standardized, scale = standardized),
        zleft_mlik = scale(left_mlik, center = standardized, scale = standardized),
        zright_mlik = scale(right_mlik, center = standardized, scale = standardized)
      ) %>%
      ungroup() %>%
      select(subject_id, choice, rt, nd, vd, sd, ov, on, os, zleft_rating, zright_rating, zleft_net1, zright_net1, zleft_net2, zright_net2, zleft_sim, zright_sim, zleft_sd, zright_sd,
             zleft_num_community, zright_num_community, zleft_var, zright_var, zleft_mlik, zright_mlik)
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
        ncd = scale(abs(left_num_community - right_num_community), center = standardized, scale = standardized),
        ov = scale(left_rating + right_rating, center = standardized, scale = standardized),
        on = scale(left_net1 + left_net1, center = standardized, scale = standardized),
        os = scale(left_sim + right_sim, center = standardized, scale = standardized),
        vard = scale(abs(left_VAR - right_VAR), center = standardized, scale = standardized),
        ovar = scale(left_VAR + right_VAR, center = standardized, scale = standardized),
        zleft_rating = scale(left_rating, center = standardized, scale = standardized),
        zright_rating = scale(right_rating, center = standardized, scale = standardized),
        zleft_net1 = scale(left_net1, center = standardized, scale = standardized),
        zright_net1 = scale(right_net1, center = standardized, scale = standardized),
        zleft_sim = scale(left_sim, center = standardized, scale = standardized),
        zright_sim = scale(right_sim, center = standardized, scale = standardized)
      ) %>%
      select(subject_id, choice,correct, rt, vd, nd1, nd2, ncd, sd, ov, on, os, vard, ovar, zleft_rating, zright_rating,
             zleft_net1, zright_net1, zleft_sim, zright_sim)
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

# weight = "betweenness"
# experiment = 2
organize_group_data <- function(experiment, weight = "degree") {
  
  # which data set are we working with?
  if (experiment == 1) {
    temp_files <- list.files(path = here::here("data", "pilot_30"), pattern = ".json", full.names = T)
    network_stats <- "LowHighWithinBetween"
    sim_img_pattern <- "../../img/grid_stimuli/grid_6_LowHighWithinBetween_"
    # load subgraphs
    load(file = here::here("data", "LowHighWithinBetween.RData"))
    #if you exclude certain "non- significant graphs" which ones? (see subgraph_permutation_testing.R)
    res_sig <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1,
                 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1,
                 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1,
                 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1,
                 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1)
    
  } else if (experiment == 2) {
    temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
    network_stats <- "modularity"
    sim_img_pattern <- "../../img/grid_stimuli/grid_6_modularity_"
    # load subgraphs
    load(file = here::here("data", "modularity_100_6.RData"))
    #if you exclude certain "non- significant graphs" which ones? (see subgraph_permutation_testing.R)
    res_sig<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    
  }
  else if (experiment == 3) {
    # temp_files <- list.files(path = here::here("data", "exp_3"), pattern = ".json", full.names = T)
    #test path
    temp_files <- list.files(path = here::here("data", "exp_3", "drive-20230627"), pattern = ".json", full.names = T)
    network_stats <- "average_strength"
    sim_img_pattern <- "../../img/grid_stimuli/grid_6_average_strength_"
    
    # load subgraphs
    load(file = here::here("data", "average_strength_100_6.RData"))
  }
  
  # will get used to calculate network meausres
  G <- g
  E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  mem <- membership(cluster_leading_eigen(G))
  
  file_idx <- length(temp_files) # how many subjects data to preprocess
  ############################
  ## organize the data and calculate value of group of items and net stats for each subject
  ##
  subject_df <- vector(mode = "list", length = file_idx)
  # pp =1
  for (pp in seq_len(file_idx)) {
    # load the  subjects data
    subject_temp <- jsonlite::parse_json(jsonlite::read_json(temp_files[[pp]]), simplifyVector = T)
    # this gets the ratings in check
    subject_rating_temp <- subject_temp %>%
      filter(screen_id == "ratings") %>%
      select(stimulus, response) %>%
      mutate(
        Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item"),
        Image = as.numeric(stringr::str_remove(Image, pattern = ".jpg"))
      ) %>%
      dplyr::left_join(load_food_names()$foods_in_image, by = "Image")
    
    ns <- which.max(map_dbl(map(network_stats, grepl, x = subject_temp$options[subject_temp$screen_id == "task"]), sum))
    
    set_values_temp <- vector(mode = "numeric", length = 100)
    set_values_MAX_temp <- vector(mode = "numeric", length = 100)
    set_values_MIN_temp <- vector(mode = "numeric", length = 100)
    set_values_VAR_temp <- vector(mode = "numeric", length = 100)
    
    set_network_temp <- vector(mode = "numeric", length = 100)
    #item_score
    set_degree_temp <- vector(mode = "numeric", length = 100)
    set_strength_temp <- vector(mode = "numeric", length = 100)
    set_eigen_temp <- vector(mode = "numeric", length = 100)
    set_weighted_transitivity_temp <- vector(mode = "numeric", length = 100)
    set_closeness_temp <- vector(mode = "numeric", length = 100)
    set_betweenness_temp <- vector(mode = "numeric", length = 100)
    #set-score
    set_edge_density_temp <- vector(mode = "numeric", length = 100)
    set_modularity_temp <- vector(mode = "numeric", length = 100)
    set_conductance_temp <- vector(mode = "numeric", length = 100)
    
    set_pca1_temp <- vector(mode = "numeric", length = 100)
    set_pca2_temp <- vector(mode = "numeric", length = 100)
    
    set_level_pca1_temp <- vector(mode = "numeric", length = 100)
    set_level_pca2_temp <- vector(mode = "numeric", length = 100)
    
    set_cluster_temp <- vector(mode = "numeric", length = 100)
    set_correlations_temp <- vector(mode = "numeric", length = 100)
    set_sd_temp <- vector(mode = "numeric", length = 100)
    set_similarity_temp <- vector(mode = "numeric", length = 100)
    
    set_weighted_values_temp <- vector(mode = "numeric", length = 100)
    
    set_fruit_temp <- vector(mode = "numeric", length = 100)
    set_num_community_temp <- vector(mode = "numeric", length = 100)
    
    # similarity ratings
    subject_similarity_temp <- subject_temp %>%
      filter(screen_id == "similarity") %>%
      select(stimulus, response) %>% # think about RT
      mutate(
        stimulus = stringr::str_remove(stimulus, pattern = sim_img_pattern),
        stimulus = as.numeric(stringr::str_remove(stimulus, pattern = ".jpg"))
      ) %>%
      unnest(response)
    # foo = 1
    
    get_subgraph_pc <- function(subgraphs){
      #extract relevant stats on subgraph
      strength_res <-do.call(rbind,  map(subgraphs, function(x) mean(strength(igraph::induced_subgraph(g, V(x)$name)))))
      ed_res <-do.call(rbind,  map(subgraphs, function(x) edge_density(igraph::induced_subgraph(g, V(x)$name))))
      mod_res <-do.call(rbind,  map(subgraphs, function(x)       as.numeric(modularity(igraph::induced_subgraph(g, V(x)$name), V(igraph::induced_subgraph(g, V(x)$name))$snack_type))))
      con_res <- do.call(rbind,  map(subgraphs, function(x){
        tempsg <- igraph::induced_subgraph(g, V(x)$name)
        mem[names(mem)] <- 1
        mem[names(mem) %in% V(tempsg)$name] <- 2
        conductance_temp <- clustAnalytics::conductance(g, mem)[2]
        as.numeric(conductance_temp)
      }))
      set_level <- data.frame(cbind(strength_res,ed_res,mod_res, con_res))
      names(set_level) <- c("average_strength", "edge_density", "modularity", "conductance")
      
      pca_res <- prcomp(set_level, center = TRUE, scale. = TRUE)
      # print(pca_res)
      
      set_level$PCA1 <-  pca_res$x[,1]
      set_level$PCA2 <- pca_res$x[,2]
      set_level$PCA3 <- pca_res$x[,3]
      
      # set_level %>% 
      #   cor() %>% 
      #   ggcorrplot::ggcorrplot(type = "upper",
      #                          lab = TRUE)+
      #   theme_classic()+
      #   labs(x = "", y = "") +
      #   theme(
      #     axis.text = element_text(face = "bold"),
      #     text = element_text(size = 15),
      #     axis.title = element_text(face = "bold"),
      #     axis.text.x = element_text(angle = 45, hjust = 1)
      #   )
      return(set_level)
    }
    set_level_scores <- get_subgraph_pc(subgraphs)
    # foo = 1
    for (foo in 1:100) {
      # select which stat to calculate
      
      # this section calculates each of the subgraph stats based on the induced
      # subgraph rather than the larger network. To get node importance
      # measures at the level of the entire graph, this code would need to change
      # pull out a candidate sub graph
      size <- net_degree[net_degree$Name %in% res[[foo]], ]$Item
      subgraph <- igraph::induced_subgraph(g, size)
      # graph_stats <- NetworkStat(subgraph)
      # calculate centrality with respect to larger graph
      graph_stats <- net_degree[net_degree$Name %in% res[[foo]], ]
      adj_temp <- igraph::as_adjacency_matrix(subgraph, sparse = F, attr = "weight")
      
      if (2 %in% graph_stats[graph_stats$Name %in% res[[foo]], ]$snack_type){
        set_fruit_temp[[foo]] <-  sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$snack_type == 2)
      } else {set_fruit_temp[[foo]] <- 0}
      
      set_degree_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$degree)
      # set_strength_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$strength)
      #set-level Average Strength within subgraph
      set_strength_temp[[foo]] <- set_level_scores[foo, "average_strength"]
      #until you make correct names test pca wiht strength name
      # set_strength_temp[[foo]] <- set_level_scores[foo, "PCA1"]
      # set_strength_temp[[foo]] <- set_level_scores[foo, "PCA2"]
      # set_strength_temp[[foo]] <- set_level_scores[foo, "PCA3"]
      
      # set_weighted_transitivity_temp[[foo]] <- NetworkToolbox::clustcoeff(adj_temp, weighted = T)$CC
      # ifelse(is.nan(set_weighted_transitivity_temp[[foo]]), set_weighted_transitivity_temp[[foo]] <- 0, set_weighted_transitivity_temp[[foo]] <- set_weighted_transitivity_temp[[foo]])
      set_weighted_transitivity_temp[[foo]] <-sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$weighted_transitivity)
      
      set_eigen_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$eigen)
      set_closeness_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$closeness)
      set_betweenness_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$betweenness)
      
      set_edge_density_temp[[foo]] <- set_level_scores[foo, "edge_density"]
      set_modularity_temp[[foo]] <- set_level_scores[foo, "modularity"]
      # for testing the permutation method
      # if (res_sig[[foo]] == 0){
      #   set_network_temp[[foo]] <- 0
      #   }
      set_conductance_temp[[foo]] <- set_level_scores[foo, "conductance"]
      
      set_pca1_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA1)
      set_pca2_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA2)
      # set_pca3_temp[[foo]] <- sum(graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA3)
      
      set_level_pca1_temp[[foo]] <- set_level_scores[foo, "PCA1"]
      set_level_pca2_temp[[foo]] <- set_level_scores[foo, "PCA2"]
      
      # calculated the weighted value of the set
      x <- do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response)
      
      if (weight == "degree") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$degree
      } else if (weight == "strength") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$strength
      } else if (weight == "weighted_transitivity") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$weighted_transitivity
      } else if (weight == "eigen") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$eigen
      } else if (weight == "closeness") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$closeness
      } else if (weight == "betweenness") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$betweenness
      } else if (weight == "PCA1") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA1
      } else if (weight == "PCA2") {
        wt <- graph_stats[graph_stats$Name %in% res[[foo]], ]$PCA2
      }
      # wt <- (wt - min(wt)) / diff(range(wt)) #normalize (corrected the issue)
      
      # use weight to get weighted average
      # set_weighted_values_temp[[foo]] <- weighted.mean(x, wt)
      set_weighted_values_temp[[foo]] <- NA
      
      
      set_values_temp[[foo]] <- sum(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      # set_values_temp[[foo]] <- mean(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      set_values_VAR_temp[[foo]] <- var(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      #?consider even more momments? e1071::skewness(), e1071::kurtosis()
      set_values_MAX_temp[[foo]] <- max(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      set_values_MIN_temp[[foo]] <- min(do.call(rbind, subject_rating_temp[subject_rating_temp$Name %in% res[[foo]], ]$response))
      
      #
      set_correlations_temp[[foo]] <- sum(apply(cor_snack_food[colnames(cor_snack_food) %in% res[[foo]], ], 2, mean, na.rm = T)[res[[foo]]])
      set_sd_temp[[foo]] <- sum(net_degree[net_degree$Name %in% res[[foo]], ]$precision)
      
      # Count number of unique communities in the set
      set_num_community_temp[[foo]] <- length(unique(graph_stats[graph_stats$Name %in% res[[foo]], ]$snack_type))
      
      if (experiment == 1) {
        next
      }
      set_similarity_temp[[foo]] <- subject_similarity_temp$response[[foo]]
    }
    
    task_temp <- subject_temp %>%
      filter(screen_id == "task") %>%
      select(subject_id, rt, options, key_press) %>%
      mutate(key_press = ifelse(key_press == "f", 1, 0)) # if left 1, ow right 0
    xxxx <- as.data.frame(do.call(rbind, task_temp$options)) %>% mutate(subject_id = pp)
    xxxx[, 1] <- as.numeric(str_remove(str_remove(xxxx[, 1], pattern = paste0("../../img/grid_stimuli/grid_6_", network_stats[[ns]], "_")), ".jpg"))
    xxxx[, 2] <- as.numeric(str_remove(str_remove(xxxx[, 2], pattern = paste0("../../img/grid_stimuli/grid_6_", network_stats[[ns]], "_")), ".jpg"))
    xxxx$rt <- task_temp$rt
    xxxx$choice <- task_temp$key_press
    names(xxxx) <- c("left", "right", "subject_id", "rt", "choice")
    xxxx$network_statistic <- network_stats[[ns]]
    
    # define variable names for left and right
    xxxx$left_rating <- NULL
    xxxx$right_rating <- NULL
    xxxx$left_wtrating <- NULL
    xxxx$right_wtrating <- NULL
    
    xxxx$left_net_degree <- NULL
    xxxx$right_net_degree <- NULL
    xxxx$left_net_strength <- NULL
    xxxx$right_net_strength  <- NULL
    xxxx$left_net_eigen <- NULL
    xxxx$right_net_eigen <- NULL
    xxxx$left_net_betweenness <- NULL
    xxxx$right_net_betweenness <- NULL
    xxxx$left_net_closeness <- NULL
    xxxx$right_net_closeness <- NULL
    xxxx$left_net_weighted_transitivity <- NULL
    xxxx$right_net_weighted_transitivity <- NULL
    xxxx$left_net_edge_density <- NULL
    xxxx$right_net_edge_density <- NULL
    xxxx$left_net_modularity <- NULL
    xxxx$right_net_modularity <- NULL
    xxxx$left_net_conductance <- NULL
    xxxx$right_net_conductance <- NULL
    
    xxxx$left_net_pca1 <- NULL
    xxxx$right_net_pca1 <- NULL
    xxxx$left_net_pca2 <- NULL
    xxxx$right_net_pca2 <- NULL
    
    xxxx$left_net_set_pca1 <- NULL
    xxxx$right_net_set_pca1 <- NULL
    xxxx$left_net_set_pca2 <- NULL
    xxxx$right_net_set_pca2 <- NULL
    
    xxxx$left_sim <- NULL
    xxxx$right_sim <- NULL
    xxxx$left_correlation <- NULL
    xxxx$right_correlation <- NULL
    xxxx$left_sd <- NULL
    xxxx$right_sd <- NULL
    xxxx$left_MAX <- NULL
    xxxx$right_MAX <- NULL
    xxxx$left_MIN <- NULL
    xxxx$right_MIN <- NULL
    xxxx$left_VAR <- NULL
    xxxx$right_VAR <- NULL
    
    xxxx$left_fruit <- NULL
    xxxx$right_fruit <- NULL
    
    xxxx$left_num_community <- NULL
    xxxx$right_num_community <- NULL
    
    for (foo in seq_len(nrow(xxxx))) {
      xxxx$left_rating[[foo]] <- as.numeric(set_values_temp[xxxx$left[[foo]]])
      xxxx$right_rating[[foo]] <- as.numeric(set_values_temp[xxxx$right[[foo]]])
      xxxx$left_wtrating[[foo]] <- as.numeric(set_weighted_values_temp[xxxx$left[[foo]]])
      xxxx$right_wtrating[[foo]] <- as.numeric(set_weighted_values_temp[xxxx$right[[foo]]])
      
      xxxx$left_net_degree[[foo]] <- as.numeric(set_degree_temp[xxxx$left[[foo]]])
      xxxx$right_net_degree[[foo]] <- as.numeric(set_degree_temp[xxxx$right[[foo]]])
      xxxx$left_net_strength[[foo]] <- as.numeric(set_strength_temp[xxxx$left[[foo]]])
      xxxx$right_net_strength[[foo]]  <- as.numeric(set_strength_temp[xxxx$right[[foo]]])
      xxxx$left_net_eigen[[foo]] <- as.numeric(set_eigen_temp[xxxx$left[[foo]]])
      xxxx$right_net_eigen[[foo]] <- as.numeric(set_eigen_temp[xxxx$right[[foo]]])
      xxxx$left_net_betweenness[[foo]] <- as.numeric(set_betweenness_temp[xxxx$left[[foo]]])
      xxxx$right_net_betweenness[[foo]] <- as.numeric(set_betweenness_temp[xxxx$right[[foo]]])
      xxxx$left_net_closeness[[foo]] <- as.numeric(set_closeness_temp[xxxx$left[[foo]]])
      xxxx$right_net_closeness[[foo]] <- as.numeric(set_closeness_temp[xxxx$right[[foo]]])
      xxxx$left_net_weighted_transitivity[[foo]] <- as.numeric(set_weighted_transitivity_temp[xxxx$left[[foo]]])
      xxxx$right_net_weighted_transitivity[[foo]] <- as.numeric(set_weighted_transitivity_temp[xxxx$right[[foo]]])
      xxxx$left_net_edge_density[[foo]] <- as.numeric(set_edge_density_temp[xxxx$left[[foo]]])
      xxxx$right_net_edge_density[[foo]] <- as.numeric(set_edge_density_temp[xxxx$right[[foo]]])
      xxxx$left_net_modularity[[foo]] <- as.numeric(set_modularity_temp[xxxx$left[[foo]]])
      xxxx$right_net_modularity[[foo]] <- as.numeric(set_modularity_temp[xxxx$right[[foo]]])
      xxxx$left_net_conductance[[foo]] <- as.numeric(set_conductance_temp[xxxx$left[[foo]]])
      xxxx$right_net_conductance[[foo]] <- as.numeric(set_conductance_temp[xxxx$right[[foo]]])
      
      xxxx$left_net_pca1[[foo]] <- as.numeric(set_pca1_temp[xxxx$left[[foo]]])
      xxxx$right_net_pca1[[foo]] <- as.numeric(set_pca1_temp[xxxx$right[[foo]]])
      xxxx$left_net_pca2[[foo]] <- as.numeric(set_pca2_temp[xxxx$left[[foo]]])
      xxxx$right_net_pca2[[foo]] <- as.numeric(set_pca2_temp[xxxx$right[[foo]]])
      
      xxxx$left_net_set_pca1[[foo]] <- as.numeric(set_level_pca1_temp[xxxx$left[[foo]]])
      xxxx$right_net_set_pca1[[foo]] <- as.numeric(set_level_pca1_temp[xxxx$right[[foo]]])
      xxxx$left_net_set_pca2[[foo]] <- as.numeric(set_level_pca2_temp[xxxx$left[[foo]]])
      xxxx$right_net_set_pca2[[foo]] <- as.numeric(set_level_pca2_temp[xxxx$right[[foo]]])
      
      xxxx$left_sim[[foo]] <- as.numeric(set_similarity_temp[xxxx$left[[foo]]])
      xxxx$right_sim[[foo]] <- as.numeric(set_similarity_temp[xxxx$right[[foo]]])
      xxxx$left_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$left[[foo]]])
      xxxx$right_correlation[[foo]] <- as.numeric(set_correlations_temp[xxxx$right[[foo]]])
      xxxx$left_sd[[foo]] <- as.numeric(set_sd_temp[xxxx$left[[foo]]])
      xxxx$right_sd[[foo]] <- as.numeric(set_sd_temp[xxxx$right[[foo]]])
      xxxx$left_MAX[[foo]] <- as.numeric(set_values_MAX_temp[xxxx$left[[foo]]])
      xxxx$right_MAX[[foo]] <- as.numeric(set_values_MAX_temp[xxxx$right[[foo]]])
      xxxx$left_MIN[[foo]] <- as.numeric(set_values_MIN_temp[xxxx$left[[foo]]])
      xxxx$right_MIN[[foo]] <- as.numeric(set_values_MIN_temp[xxxx$right[[foo]]])
      xxxx$left_VAR[[foo]] <- as.numeric(set_values_VAR_temp[xxxx$left[[foo]]])
      xxxx$right_VAR[[foo]] <- as.numeric(set_values_VAR_temp[xxxx$right[[foo]]])
      
      xxxx$left_fruit[[foo]] <-  as.numeric(set_fruit_temp[xxxx$left[[foo]]])
      xxxx$right_fruit[[foo]] <-  as.numeric(set_fruit_temp[xxxx$right[[foo]]])
      
      xxxx$left_num_community[[foo]] <- as.numeric(set_num_community_temp[xxxx$left[[foo]]])
      xxxx$right_num_community[[foo]] <- as.numeric(set_num_community_temp[xxxx$right[[foo]]])
    }
    # xxxx$value_network_corr <- cor(set_values_temp, set_network_temp)
    # xxxx$value_network_corr_p <- cor.test(set_values_temp, set_network_temp)$p.value
    
    subject_df[[pp]] <- xxxx
  }
  
  df <- as.data.frame(do.call(rbind, subject_df)) %>%
    unnest(cols = c(
      left_rating, right_rating, left_wtrating, right_wtrating, 
      left_net_degree, right_net_degree, 
      left_net_strength, right_net_strength,
      left_net_eigen, right_net_eigen,
      left_net_betweenness, right_net_betweenness,
      left_net_closeness, right_net_closeness,
      left_net_weighted_transitivity,right_net_weighted_transitivity,
      left_net_edge_density,right_net_edge_density,
      left_net_modularity,right_net_modularity,
      left_net_conductance,right_net_conductance,
      left_net_pca1,right_net_pca1,
      left_net_pca2,right_net_pca2,
      left_net_set_pca1,right_net_set_pca1,
      left_net_set_pca2,right_net_set_pca2,
      left_sim, right_sim,
      left_correlation, right_correlation, left_sd, right_sd,
      left_MAX, right_MAX, left_MIN, right_MIN, left_VAR, right_VAR,
      left_fruit, right_fruit,
      left_num_community, right_num_community
    ))
  # population mean liking of the set (filled by set_mean_liking_control_regression.R); NA otherwise
  df$left_mlik <- NA_real_; df$right_mlik <- NA_real_
  # add the correct response col and choose max and not choose min col
  df$correct <- as.numeric((df$left_rating > df$right_rating & df$choice == 1) | (df$left_rating < df$right_rating & df$choice == 0))
  
  df$correctwt <- as.numeric((df$left_wtrating > df$right_wtrating & df$choice == 1) | (df$left_wtrating < df$right_wtrating & df$choice == 0))
  
  df$choose_max <- factor(as.numeric((df$left_MAX > df$right_MAX & df$choice == 1) | (df$left_MAX < df$right_MAX & df$choice == 0)))
  df$choose_min <- factor(as.numeric((df$left_MIN < df$right_MIN & df$choice == 0) | (df$left_MIN > df$right_MIN & df$choice == 1)))
  
  return(df)
}

#' Apply Canonical PCA Weights to New Data
#'
#' This function takes a dataframe of centrality scores and applies a pre-defined
#' set of PCA loadings to calculate consistent PC scores. This ensures that PC1, PC2, etc.,
#' have the same meaning and direction across different datasets.
#'
#' @param new_data A dataframe with columns for centrality measures (e.g., 'degree', 'strength').
#'                 The column names must match the row names of `reference_loadings`.
#' @param reference_loadings A matrix of PCA loadings (p measures x k components) to apply.
#'
#' @return The original `new_data` dataframe with new columns for PC scores (e.g., PC1, PC2).

apply_pca_weights <- function(new_data, reference_loadings) {
  # Get the names of the measures from the loading matrix
  pca_cols <- rownames(reference_loadings)
  
  # Ensure all required columns exist in the new data
  if (!all(pca_cols %in% names(new_data))) {
    missing_cols <- pca_cols[!pca_cols %in% names(new_data)]
    stop("The following required columns are missing from 'new_data': ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # 1. Select the relevant columns in the correct order
  data_to_transform <- new_data[, pca_cols]
  
  # 2. Standardize the data (scale to mean=0, sd=1)
  scaled_data <- scale(data_to_transform)
  
  # Handle cases where a column has zero variance after filtering, which results in NaNs
  if (any(is.nan(scaled_data))) {
      scaled_data[is.nan(scaled_data)] <- 0
      warning("NaNs produced during scaling (likely due to zero variance in a column). Replaced with 0.", call. = FALSE)
  }
  
  # 3. Apply loadings via matrix multiplication
  # Result is a matrix of n_samples x k_components
  pc_scores <- scaled_data %*% reference_loadings
  
  # 4. Combine with original data and return
  pc_scores_df <- as_tibble(pc_scores)
  
  # Add an informative prefix to the new columns
  names(pc_scores_df) <- paste0("PC", 1:ncol(pc_scores_df))
  
  bind_cols(new_data, pc_scores_df)
}
