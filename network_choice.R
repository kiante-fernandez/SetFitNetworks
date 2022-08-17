library(here)
library(tidyverse)

load("/Users/kiantefernandez/Documents/OSU/Fernandez_2021_EEG_2x2_Choice/data/smith_2020_paper_data/TaskTrial.RData")

FoodNamesTop100 <- read_csv("~/Documents/OSU/Fernandez_2021_EEG_2x2_Choice/EEG_2x2_Choice/FoodNamesTop100.csv")

TEST <- taste.df %>% select("SubjectNumber", "Choice", "PicL", "PicR")
TEST$selected <- ifelse(TEST$Choice == 1, TEST$PicL,
  ifelse(TEST$Choice == 2, TEST$PicR, NA)
)
choice_probs<- data.frame()

for (subject_idx in unique(TEST$SubjectNumber)) {
  subject <- TEST %>% filter(SubjectNumber == subject_idx)
  # number of times an option was shown
  selected <- summary(factor(subject$selected, levels = 1:100))
  total <- summary(factor(c(subject$PicL, subject$PicR), levels = 1:100))
  p <- data.frame(selected / total)
  p$selected.total[is.nan(p$selected.tota)] <- 0
  subject_row <- t(p)
  rownames(subject_row) <- paste0(subject_idx)
  choice_probs <- rbind(subject_row, choice_probs)
}
for (food_id in 1:100) {
  colnames(choice_probs)[food_id]<- as.character(FoodNamesTop100[food_id, ])
}

network <- bootnet::estimateNetwork(choice_probs, 
                                    default = "TMFG",#Triangulated Maximally Filtered Graph:
                                    graphType = "pcor")

graph_choice <- graph_from_adjacency_matrix(network$graph,
                                             "undirected",
                                             weighted = TRUE,
                                             add.colnames = T,
                                             diag = F)
#you have negative weights
E(graph_choice)$weight
#algos somethings do not take negative weights
# E(graph_choice)$weight <- E(graph_choice)$weight / sum((E(graph_choice)$weight))
# E(graph_choice)$weight <- 2**((E(graph_choice)$weight - min(E(graph_choice)$weight)) / diff(range(E(graph_choice)$weight)))

c1 <- cluster_fast_greedy(graph_choice)
c2 <- cluster_edge_betweenness(graph_choice)
c3 <- cluster_infomap(graph_choice)
c4 <- cluster_leading_eigen(graph_choice)
c5 <- cluster_louvain(graph_choice)
c6 <- cluster_walktrap(graph_choice, steps = 4)
get_clustering_stats <- function(c) {
  # modularity measure for each algorithm
  print(modularity(c))
  # memberships of nodes
  # print(membership(c))
  # number of communities
  print(length(c))
  # size of communities
  print(sizes(c))
}
for (method in list(c1, c2, c3, c4, c5, c6)) {
  get_clustering_stats(method)
}

l3 <- layout_nicely(graph_choice)
#give nodes the names
for (node_name in 1:100) {
  V(graph_choice)$name[[node_name]] <- as.character(FoodNamesTop100[node_name, ])
}
plot(graph_choice,
     layout = l3,
     margin = .0,
     vertex.label = V(graph_ratings)$name,
     vertex.label.color = "black",
     vertex.label.cex = .6,
     vertex.size = 2,
     vertex.label.family = "Times",
     edge.curved = .15,
     edge.width = E(graph_choice)$weight,
     mark.groups = communities(c6)
)


mean_choice_subject <- apply(choice_probs, 2, mean)
mean_choice_subject <- data_frame(mean = mean_choice_subject, name = names(mean_choice_subject))
mean_choice_subject %>%
  arrange(mean) %>%
  ggplot(aes(x = reorder(name, -mean), y = mean, fill = factor(name))) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    x = "Snack Foods",
    y = "average amount chosen"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))

#look at choice proportion
choice_proportion <- data.frame(option = rep(0, 100), p = rep(0, 100))

for (item_idx in 1:100) {
  TEST[TEST$PicL == item_idx | TEST$PicR == item_idx, ] %>%
    mutate(Choice = ifelse(Choice == item_idx, "PicL", "PicR")) %>%
    pivot_longer(c("PicL", "PicR"),
      names_to = "side",
      values_to = "option"
    ) %>%
    filter(Choice == side) %>%
    filter(option == item_idx) %>%
    summarise(n = n()) -> selected

  TEST[TEST$PicL == item_idx | TEST$PicR == item_idx, ] %>%
    mutate(Choice = ifelse(Choice == item_idx, "PicL", "PicR")) %>%
    pivot_longer(c("PicL", "PicR"),
      names_to = "side",
      values_to = "option"
    ) %>%
    group_by(option) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    slice(1) -> total

  p <- selected$n / total$n

  choice_proportion$option[item_idx] <- item_idx
  choice_proportion$p[item_idx] <- p
}


for (food_id in 1:100) {
  choice_proportion$option[choice_proportion$option == food_id] <- as.character(FoodNamesTop100[food_id, ])
}

choice_proportion$option <- factor(choice_proportion$option)

# Barplot
choice_proportion %>%
  arrange(p) %>%
  ggplot(aes(x = reorder(option, -p), y = p, fill = factor(option))) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    x = "Snack Foods",
    y = "Proportion chosen"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))


# Degree <- degree(graph_ratings)
# Authority <- authority.score(graph_ratings)$vector
# Hub <- hub.score(graph_ratings)$vector
# Closeness <- closeness(graph_ratings)
# Betweenness <- betweenness(graph_ratings)
# Strength <- strength(graph_ratings)
# Transitivity <- transitivity(graph_ratings, "local")
# Eig <- eigen_centrality(graph_ratings)$vector
#
# subgraph_centrality(graph_ratings)
#
# choice_proportion$degree <- Degree
# #
# cor.test(choice_proportion$p,subgraph_centrality(graph_ratings))


# ggplot(choice_proportion, aes(degree, p)) + geom_point()+
#   theme_classic()+geom_smooth(method = "lm")

# 35/79 = 0.443038
