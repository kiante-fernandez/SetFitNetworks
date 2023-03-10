# initial analysis of the similarity rating data

library(igraph)
library(purrr)
library(tidyverse)
library(here)
source(here::here("src", "utils.R"))

res_sig<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# temp_files <- list.files(path = here::here("data", "pilot_30"), pattern = ".json", full.names = T)
temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)

load(file = here::here("data", "modularity_100_6.RData"))

# load(file = here::here("data", "LowHighWithinBetween.RData"))
source("exploratory_graph_analysis.R")

g <-graph_from_adjacency_matrix(net_sim_GPT3,"undirected",weighted = TRUE,diag = F)
clp <- cluster_fast_greedy(g)
V(g)$snack_type <- clp$membership
net_degree <- calculate_net_stats(g)

V(g)$name <- net_degree$Name
  
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
mem <- membership(cluster_leading_eigen(G))

# get mod scores
# mod_res <- map(subgraphs, function(x) unique(V(x)$mod))
mod_res <- map(subgraphs, function(x) {temp <- igraph::induced_subgraph(g, V(x)$name) 
                                                                     as.numeric(modularity(temp, V(temp)$snack_type))})
mod_res <- do.call(rbind, mod_res)



# mod_res[res_sig == 0] <- 0

# edge_dens <- map(subgraphs, function(x) edge_density(x))

edge_dens <- map(subgraphs, function(x) edge_density(igraph::induced_subgraph(g, V(x)$name) ))

edge_dens <- do.call(rbind, edge_dens)

# note the negative weights issue here
# eigen_cen <- map(subgraphs, function(x) {
#   E(x)$weight <- 2**((E(x)$weight - min(E(x)$weight)) / diff(range(E(x)$weight)))
#   sum(eigen_centrality(x)$vector)
# })
eigen_cen <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  E(temp)$weight <- 2**((E(temp)$weight - min(E(temp)$weight)) / diff(range(E(temp)$weight)))
  
  sum(eigen_centrality(temp)$vector)
})
eigen_cen <- do.call(rbind, eigen_cen)

strength_res <- map(subgraphs, function(x) sum(strength(igraph::induced_subgraph(g, V(x)$name))))
strength_res <- do.call(rbind, strength_res)
#did i mess up how to caculate strength
strength_res <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  sum(net_degree[net_degree$Name %in% V(temp)$name, ]$strength)
})
strength_res <- do.call(rbind, strength_res)

degree_res <- map(subgraphs, function(x) sum(degree(igraph::induced_subgraph(g, V(x)$name),normalized = TRUE)))
degree_res <- do.call(rbind, degree_res)

pca1 <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  print(V(temp)$name)
  print(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA1)
  sum(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA1)
})
pca1 <- do.call(rbind, pca1)

pca2 <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  print(V(temp)$name)
  print(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA2)
  print(sum(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA2))
})
pca2 <- do.call(rbind, pca2)

pca3 <- map(subgraphs, function(x) {
  temp <- igraph::induced_subgraph(g, V(x)$name)
  
  sum(net_degree[net_degree$Name %in% V(temp)$name, ]$PCA3)
})
pca3 <- do.call(rbind, pca3)
# conductance
# source("exploratory_graph_analysis.R")
# source('fernandez_rating_network.R') #load the EGA from the new rating data
#^^need file it s on other machine I think


con_res <- map(subgraphs, function(x) {
  x <- igraph::induced_subgraph(g, V(x)$name)
  mem[names(mem)] = 1
  mem[names(mem) %in% V(x)$name] = 2  
  conductance_temp <- clustAnalytics::conductance(g, mem)[2]
  as.numeric(conductance_temp)
  return(unlist(conductance_temp))
  })
# data <- temp_files[[1]]
similarity_ratings <- function(data) {
  set_values_temp <- vector(mode = "numeric", length = 100)

  # load the  subjects data
  subject_temp <- jsonlite::parse_json(jsonlite::read_json(data), simplifyVector = T)
  # this gets the ratings in check
  subject_rating_temp <- subject_temp %>%
    filter(screen_id == "similarity") %>%
    select(stimulus, response) %>% # think about RT
    mutate(
      stimulus = stringr::str_remove(stimulus, pattern = "../../img/grid_stimuli/grid_6_modularity_"),
      stimulus = as.numeric(stringr::str_remove(stimulus, pattern = ".jpg"))
    ) %>%
    unnest(response)

  subject_value_temp <- subject_temp %>%
    filter(screen_id == "ratings") %>%
    select(stimulus, response) %>%
    mutate(
      Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item"),
      Image = as.numeric(stringr::str_remove(Image, pattern = ".jpg"))
    ) %>%
    dplyr::left_join(load_food_names()$foods_in_image, by = "Image")

  for (foo in 1:100) {
    set_values_temp[[foo]] <- sum(do.call(rbind, subject_value_temp[subject_value_temp$Name %in% res[[foo]], ]$response))
  }

  # normalize the ratings?
  subject_rating_temp$responsenormalized <- (subject_rating_temp$response - min(subject_rating_temp$response)) / diff(range(subject_rating_temp$response)) # xnormalized = (x - min(x)) / range(x)
  subject_rating_temp$modularity <- as.vector(mod_res)
  subject_rating_temp$edge_densi <- as.vector(edge_dens)
  subject_rating_temp$degree_res <- as.vector(degree_res)
  subject_rating_temp$strength_res <- as.vector(strength_res)
  subject_rating_temp$eigen_cen <- as.vector(eigen_cen)
  subject_rating_temp$con_cen <- as.vector(unlist(con_res))

  
  subject_rating_temp$pca1 <- as.vector(pca1)
  subject_rating_temp$pca2 <- as.vector(pca2)
  subject_rating_temp$pca3 <- as.vector(pca3)
  
  subject_rating_temp$ratings <- set_values_temp

  subject_rating_temp$subject_id <- unique(subject_temp$subject_id)
  
  return(subject_rating_temp)
}
individual_food_ratings <- function(data){
  # load the  subjects data
  subject_temp <- jsonlite::parse_json(jsonlite::read_json(data), simplifyVector = T)
  
  subject_value_temp <- subject_temp %>%
    filter(screen_id == "ratings") %>%
    dplyr::select(stimulus, response) %>%
    mutate(
      Image = stringr::str_remove(stimulus, pattern = "../../img/60Foods/item"),
      Image = as.numeric(stringr::str_remove(Image, pattern = ".jpg"))
    ) %>%
    dplyr::left_join(load_food_names()$foods_in_image, by = "Image") %>% 
    unnest(response)
  
  subject_value_temp$subject_id <- unique(subject_temp$subject_id)
  
  return(subject_value_temp)
}

res_list <- map(temp_files, similarity_ratings)
res <- do.call(rbind, map(temp_files, similarity_ratings))

ggplot(res, aes(ratings)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 50) +
  # geom_vline(xintercept = mean(res$response), linetype = "dashed", size = .7)+
  theme_classic() +
  labs(x = "Sum ratings", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

ggplot(res, aes(response)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 50) +
  geom_vline(xintercept = mean(res$response), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "Similarity Judgment", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
ggplot(res, aes(responsenormalized)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 50) +
  geom_vline(xintercept = mean(res$responsenormalized), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "Similarity Judgment (normalized)", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )


ggplot(res, aes(modularity)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 11) +
  geom_vline(xintercept = mean(res$modularity), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "Q", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )


ggplot(res, aes(edge_densi)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 9) +
  geom_vline(xintercept = mean(res$edge_densi), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "Edge Density", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

ggplot(res, aes(con_cen)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 8) +
  geom_vline(xintercept = mean(res$con_cen), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "Conductance", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
ggplot(res, aes(pca1)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 8) +
  geom_vline(xintercept = mean(res$pca1), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "PCA1", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

ggplot(res, aes(pca2)) +
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 8) +
  geom_vline(xintercept = mean(res$pca1), linetype = "dashed", size = .7) +
  theme_classic() +
  labs(x = "PCA2", y = "Count") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
library(lme4)
library(lmerTest)
m <- lmer(responsenormalized ~ strength_res  + (1 | subject_id) + (1 | stimulus), data = res)
summary(m)
summary(lm(responsenormalized ~ strength_res, data = res))

compares <- res %>%
  group_by(stimulus) %>%
  summarise(
    subgraph_mean = mean(response),
    se = sqrt(var(response) / length(response)),
    subgraph_sd = sd(response)
  )

compares$mod <- mod_res
compares$ed <- edge_dens
compares$st <- strength_res
compares$d <- degree_res

compares$ec <- eigen_cen
compares$c <- unlist(con_res)
compares$pca1 <- pca1
compares$pca2 <- pca2
compares$pca3 <- pca3

correlation::correlation(compares)
dat_pca <- compares[,c("mod","ed","st","ec","c")]

# dat_pca <- compares[,c("mod","ed","st","ec","c")]

pca_res <- prcomp(dat_pca, center = TRUE, scale. = TRUE)
pca_res
plot(pca_res)
summary(pca_res)
# biplot(pca_res , c(1,3))
m_data <- data.frame(cbind(subgraph_mean = compares$subgraph_mean,
                           subgraph_sd =compares$subgraph_sd , pca_res$x)) 


m_data <- data.frame(cbind(subgraph_mean = compares$subgraph_mean, subgraph_sd =compares$subgraph_sd , pca_res$x)) 
summary(lm(subgraph_mean ~ subgraph_sd + PC1 + PC2 +  PC3, m_data))
# summary(lm(subgraph_mean ~ scale(subgraph_sd) + scale(PC1) + scale(PC2) +  scale(PC3), m_data))

#graphs with more connections have higher similarity scores

# compares <- compares %>%
#   filter(mod != 0)

library(BayesFactor)
library(bayestestR)
library(see)
result <- correlationBF(compares$subgraph_mean, compares$st)
describe_posterior(result, test = "p_direction")
bayesfactor_models(result)
result <- correlationBF(compares$subgraph_mean, compares$ed)
describe_posterior(result, test = "p_direction")
bayesfactor_models(result)
result <- correlationBF(compares$subgraph_mean, compares$mod)
describe_posterior(result, test = "p_direction")
bayesfactor_models(result)
plot(bayesfactor_models(result)) +
  scale_fill_pizza()
result <- correlationBF(compares$subgraph_mean, compares$ec)
describe_posterior(result, test = "p_direction")
bayesfactor_models(result)
plot(bayesfactor_models(result)) +
  scale_fill_pizza()
result <- correlationBF(compares$subgraph_mean, compares$c)
describe_posterior(result, test = "p_direction")
bayesfactor_models(result)
plot(bayesfactor_models(result)) +
  scale_fill_pizza()
result <- correlationBF(scale(compares$subgraph_mean), scale(compares$pca2))
describe_posterior(result, test = "p_direction")
bayesfactor_models(result)
plot(bayesfactor_models(result)) +
  scale_fill_pizza()

result <- cor.test(compares$subgraph_mean, compares$c, method = "spearman")
report::report(result)

result <- cor.test(compares$subgraph_mean,compares$st,  method = "spearman")
report::report(result)

result <- cor.test(compares$subgraph_mean,compares$ed,  method = "spearman")
report::report(result)

result <- cor.test(compares$subgraph_mean,compares$ec,  method = "spearman")

report::report(result)

result <- cor.test(scale(compares$subgraph_mean), scale(compares$pca1), method = "kendall")
report::report(result)

result <- cor.test(compares$subgraph_mean, compares$pca2, method = "spearman")
report::report(result)

report::report(cor.test(compares$subgraph_mean, compares$d, method = "spearman"))


report::report(cor.test(compares$subgraph_mean, compares$pca1, method = "spearman"))
report::report(cor.test(compares$subgraph_mean, compares$pca2, method = "spearman"))
report::report(cor.test(compares$subgraph_mean, compares$pca3, method = "spearman"))

#  # piecewise analysis on modularity
# library(segmented)
# # fit simple linear regression model
# fit <- lm(subgraph_mean ~ mod, data = compares)
# # fit piecewise regression model to original model, estimating a breakpoint at x=9
# segmented.fit <- segmented(fit, seg.Z = ~mod, psi = 0)
# # view summary of segmented model
# summary(segmented.fit)
# # group according to the changepoint
# compares$grp <- factor(ifelse(compares$mod > 0, 1, 0))
# 
# # you can also see a quadratic relationship with our similarity score
# ggplot(compares, aes(mod, subgraph_mean, group = grp)) +
#   geom_point() +
#   theme_classic() +
#   geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
#   geom_smooth(method = "lm", formula = y ~ x, se = T, size = 1.8, color = "black") +
#   labs(y = "Similarity", x = "Q") +
#   # geom_vline(xintercept = 0, linetype = "dashed", size = .7) +
#   theme(
#     axis.text = element_text(face = "bold"),
#     text = element_text(size = 15),
#     axis.title = element_text(face = "bold")
#   )

ggplot(compares, aes(ec, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "Eigen centrality", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

ggplot(compares, aes(ed, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "Edge Density", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

# compares[-c(32,46,98),]
ggplot(compares, aes(st, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "Strength", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
ggplot(compares, aes(d, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "Degree", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
ggplot(compares, aes(c, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "Conductance", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

ggplot(compares, aes(pca1, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "PCA1", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

ggplot(compares, aes(pca2, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "PCA2", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
ggplot(compares, aes(pca3, subgraph_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "PCA3", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

compares %>% 
  dplyr::select(subgraph_mean, mod, ed, st, pca1, pca2,pca3) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE)+
  theme_classic()+
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

m <- (lm(subgraph_mean~ scale(subgraph_sd) + scale(c), data = compares))
report::report(m)
plot(ggeffects::ggeffect(m, terms = c("subgraph_sd", "c [0.5, .8, 1]")))


net_degree <- calculate_net_stats(g)

E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

# library(RColorBrewer)
# dput(brewer.pal(n = 7, name = 'Dark2'))
net_degree <- net_degree%>% 
  mutate(colors = 
           case_when(  
             snack_type == 1 ~ "#1B9E77",
             snack_type == 2 ~ "#D95F02",
             snack_type == 3 ~ "#7570B3",
             snack_type == 4 ~ "#E7298A",
             snack_type == 5 ~ "#66A61E",
             snack_type == 6 ~ "#E6AB02",
             snack_type == 7 ~ "#A6761D"
           )
  )

par(mfrow = c(1, 3)) # set the plotting area into a 1*3 array

plot(subgraphs[[32]],
  layout = layout.circle(subgraphs[[32]]),
  margin = .0,
  vertex.label.color = "black",
  vertex.label.font = 2,
  vertex.label.cex = 1.5,
  vertex.label.dist = 2,
  vertex.size = 20,
  vertex.label.family = "Times",
  main = paste0("Most Similar. Q = ", round(compares$mod[[32]], 3), " D = ", round(compares$ed[[32]], 3), " s = ", round(compares$st[[32]], 3), " c = ", round(compares$c[[32]], 3)),
  edge.width = abs(E(subgraphs[[32]])$weight) * 10,
)

plot(subgraphs[[2]],
  layout = layout.circle(subgraphs[[2]]),
  margin = .0,
  vertex.label.color = "black",
  vertex.label.font = 2,
  vertex.label.cex = 1.5,
  vertex.label.dist = 2,
  vertex.size = 20,
  vertex.label.family = "Times",
  main = paste0("Least Similar. Q = ", round(compares$mod[[2]], 3), " D = ", round(compares$ed[[2]], 3), " s = ", round(compares$st[[2]], 3), " c = ", round(compares$c[[2]], 3)),
  edge.width = abs(E(subgraphs[[2]])$weight) * 10,
)
plot(subgraphs[[73]],
  layout = layout.circle(subgraphs[[73]]),
  margin = .0,
  vertex.label.color = "black",
  vertex.label.font = 2,
  vertex.label.cex = 1.5,
  vertex.label.dist = 2,
  vertex.size = 20,
  vertex.label.family = "Times",
  main = paste0("Hightest Variance. Q = ", round(compares$mod[[73]], 3), " D = ", round(compares$ed[[73]], 3), " s = ", round(compares$st[[73]], 3), " c = ", round(compares$c[[73]], 3)),
  edge.width = abs(E(subgraphs[[73]])$weight) * 10,
)


plot_subgraph <- function(n){
  temp <- igraph::induced_subgraph(g, V(subgraphs[[n]])$name) 
  # temp <- subgraphs[[n]]
  
  V(temp)$color <- net_degree[net_degree$Name %in% V(temp)$name,]$colors
  
  plot(temp,
       # layout = layout.circle(subgraphs[[n]]),
       margin = .0,
       # vertex.label.color = "black",
       vertex.label.font = 2,
       vertex.label.cex = 1.5,
       vertex.label.dist = 1,
       # vertex.size = 30,
       vertex.label.family = "Times",
       # main = paste0("avg sim: ", round(compares$subgraph_mean[n], 5)),
       main = paste0("avg sim: ", round(compares$subgraph_mean[n], 2), " PC score: ", round(compares$pca2[n], 2)),
       # edge.width = abs(E(subgraphs[[n]])$weight) * 6,
       
       vertex.shape="none", 
       vertex.label.color=V(temp)$color,
       vertex.size = NULL
  )
}

res %>%
  dplyr::select(responsenormalized, ratings, subject_id, stimulus) %>%
  group_by(stimulus) %>%
  summarise(
    rating_mean = mean(ratings),
    rating_se = sqrt(var(ratings) / length(ratings)),
    sim_mean = mean(responsenormalized),
    sim_se = sqrt(var(responsenormalized) / length(responsenormalized))
  ) %>%
  ggplot(aes(rating_mean, sim_mean)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = sim_mean - sim_se, ymax = sim_mean + sim_se), size = .7, color = "red") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  labs(x = "Sum Ratings", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

# Open pdf file
pdf(file= "subgraphs.pdf" )
# create a 2X2 grid
par( mfrow= c(2,2) )
#the high sim 
for (foo_plt in arrange(compares, subgraph_mean)$stimulus){
  print(plot_subgraph(foo_plt))
}
dev.off()

single_item_ratings <- do.call(rbind, map(temp_files, individual_food_ratings))

plotting <- single_item_ratings %>% 
  group_by(Name) %>% 
  summarise(n = n(),
            item_mean = mean(response),
            se = sqrt(var(response) / length(response))) %>% 
  left_join(net_degree)
  

p1 <- single_item_ratings %>%
  ggplot(aes(x = reorder(factor(Name),response) , y = response)) +
  geom_boxplot(fill = arrange(plotting, item_mean)$colors)+
  theme_classic()+
  theme(legend.position="none")+
  labs(y = "Liking ratings",
       x = "Food Item")+
  coord_flip()+
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face="bold", color=arrange(plotting, item_mean)$colors ,
                               size=10, angle=40)
  )

p2 <- single_item_ratings %>% 
  group_by(Name) %>% 
  summarise(n = n(),
            item_mean = mean(response),
            se = sqrt(var(response) / length(response))) %>% 
  left_join(net_degree) %>% 
  ggplot(aes(x = reorder(factor(Name),PCA2) , y = PCA2)) +
  geom_col(fill = "darkgreen")+
  theme_classic()+
  theme(legend.position="none")+
  labs(y = "PCA2",
       x = "Food Item")+
  coord_flip()+
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face="bold", color=arrange(net_degree, PCA2)$colors,
                               size=10, angle=40)
  )
p4 <- single_item_ratings %>% 
  group_by(Name) %>% 
  summarise(n = n(),
            item_mean = mean(response),
            se = sqrt(var(response) / length(response))) %>% 
  left_join(net_degree) %>% 
  ggplot(aes(x = reorder(factor(Name),PCA1) , y = PCA1)) +
  geom_col(fill = "darkgreen")+
  theme_classic()+
  theme(legend.position="none")+
  labs(y = "PCA1",
       x = "Food Item")+
  coord_flip()+
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face="bold", color=arrange(net_degree, PCA1)$colors,
                               size=10, angle=40)
  )

p3 <- single_item_ratings %>% 
  group_by(Name) %>% 
  summarise(n = n(),
            item_mean = mean(response),
            se = sqrt(var(response) / length(response))) %>% 
  left_join(net_degree) %>% 
  ggplot(aes(x = PCA2 , y = item_mean)) +
  # geom_point(color = "darkgreen", size = 3)+
  geom_pointrange(aes(ymin = item_mean - se, ymax = item_mean + se), size = .7, color = "darkgreen") +
  geom_smooth(method = "lm", se = T, size = 1.8, color = "black") +
  theme_classic()+
  theme(legend.position="none")+
  labs(y = "Liking rating",
       x = "PCA2")+
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
library(patchwork)
p4 + p2
# (p3  + p1)/(p2 +plot_spacer())+
(p3  + p1)+
  plot_annotation(tag_levels = 'A')

test <- single_item_ratings %>% 
  group_by(Name) %>% 
  summarise(n = n(),
            item_mean = mean(response),
            se = sqrt(var(response) / length(response))) %>% 
  left_join(net_degree)

# m2 <- lm(item_mean ~ scale(sds) + scale(PCA1),test) #PCA test
m2 <- lm(item_mean ~ scale(PCA1) + scale(PCA2) + scale(PCA3),test) #PCA test
summary(m2)
report::report(m2)

test %>%dplyr::select(item_mean,sds,degree, strength,eigen,weighted_transitivity, closeness,betweenness,PCA1, PCA2, PCA3) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE)+
  theme_classic()+
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face="bold",size=10, angle=40),
    axis.text.x = element_text(face="bold",size=10, angle=40, hjust= .9)
  )

# library("factoextra")
# fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
