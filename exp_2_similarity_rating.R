# initial analysis of the similarity rating data

library(igraph)
library(purrr)
library(tidyverse)
source(here::here("src", "utils.R"))

temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)

load(file = here::here("data", "modularity_100_6.RData"))

# load(file = here::here("data", "LowHighWithinBetween.RData"))

# get mod scores
mod_res <- map(subgraphs, function(x) unique(V(x)$mod))
mod_res <- map(subgraphs, function(x) {as.numeric(modularity(x, V(x)$snack_type))})
mod_res <- do.call(rbind, mod_res)
# mod_res[res_sig == 0] <- 0

edge_dens <- map(subgraphs, function(x) edge_density(x))
edge_dens <- do.call(rbind, edge_dens)

# note the negative weights issue here
eigen_cen <- map(subgraphs, function(x) {
  E(x)$weight <- 2**((E(x)$weight - min(E(x)$weight)) / diff(range(E(x)$weight)))
  sum(eigen_centrality(x)$vector)
})

eigen_cen <- do.call(rbind, eigen_cen)

strength_res <- map(subgraphs, function(x) sum(strength(x)))
strength_res <- do.call(rbind, strength_res)

# conductance
source("exploratory_graph_analysis.R")
G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
mem <- membership(cluster_leading_eigen(G))

con_res <- map(subgraphs, function(x) {
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
  subject_rating_temp$responsenormalized <- (subject_rating_temp$response - min(subject_rating_temp$response)) / range(subject_rating_temp$response) # xnormalized = (x - min(x)) / range(x)
  subject_rating_temp$modularity <- as.vector(mod_res)
  subject_rating_temp$edge_densi <- as.vector(edge_dens)
  subject_rating_temp$strength_res <- as.vector(strength_res)
  subject_rating_temp$eigen_cen <- as.vector(eigen_cen)
  subject_rating_temp$con_cen <- as.vector(unlist(con_res))
  
  subject_rating_temp$ratings <- set_values_temp

  subject_rating_temp$subject_id <- unique(subject_temp$subject_id)
  
  return(subject_rating_temp)
}
individual_food_ratings <- function(data){
  # load the  subjects data
  subject_temp <- jsonlite::parse_json(jsonlite::read_json(data), simplifyVector = T)
  
  subject_value_temp <- subject_temp %>%
    filter(screen_id == "ratings") %>%
    select(stimulus, response) %>%
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
compares$ec <- eigen_cen
compares$c <- unlist(con_res)

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

result <- cor.test(compares$subgraph_mean, compares$c)
report::report(result)
 # piecewise analysis on modularity
library(segmented)
# fit simple linear regression model
fit <- lm(subgraph_mean ~ mod, data = compares)
# fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~mod, psi = 0)
# view summary of segmented model
summary(segmented.fit)
# group according to the changepoint
compares$grp <- factor(ifelse(compares$mod > 0, 1, 0))

# you can also see a quadratic relationship with our similarity score
ggplot(compares, aes(mod, subgraph_mean, group = grp)) +
  geom_point() +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", formula = y ~ x, se = T, size = 1.8, color = "black") +
  labs(y = "Similarity", x = "Q") +
  geom_vline(xintercept = 0, linetype = "dashed", size = .7) +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )

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


compares %>% 
  select(subgraph_mean, subgraph_sd, mod, ed, st, c) %>% 
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
  plot(subgraphs[[n]],
       layout = layout.circle(subgraphs[[n]]),
       margin = .0,
       vertex.label.color = "black",
       vertex.label.font = 2,
       vertex.label.cex = 1.5,
       vertex.label.dist = 2,
       vertex.size = 20,
       vertex.label.family = "Times",
       main = paste0("Q = ", round(compares$mod[[n]], 3), " D = ", round(compares$ed[[n]], 3), " s = ", round(compares$st[[n]], 3), " c = ", round(compares$c[[n]], 3)),
       edge.width = abs(E(subgraphs[[n]])$weight) * 10,
  )
}


res %>%
  select(response, ratings, subject_id, stimulus) %>%
  group_by(stimulus) %>%
  summarise(
    rating_mean = mean(ratings),
    rating_se = sqrt(var(ratings) / length(ratings)),
    sim_mean = mean(response),
    sim_se = sqrt(var(response) / length(response))
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

par(mfrow = c(2, 4)) # set the plotting area into a 1*3 array
arrange(compares, c)$stimulus

plot_subgraph(46)
plot_subgraph(98)
plot_subgraph(32)
plot_subgraph(60)

plot_subgraph(14)
plot_subgraph(8)
plot_subgraph(2)
plot_subgraph(1)

single_item_ratings <- do.call(rbind, map(temp_files, individual_food_ratings))

single_item_ratings %>%
  ggplot(aes(x = reorder(factor(Name),response) , y = response)) +
  geom_boxplot(fill = "orange")+
  theme_classic()+
  theme(legend.position="none")+
  labs(y = "Ratings",
       x = "Food Items")+
  coord_flip()+
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )


