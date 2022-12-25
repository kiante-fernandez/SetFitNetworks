#initial analysis of the similarity rating data

library(igraph)
library(ggeffects)

temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
load(file = here::here("data", "modularity_100_6.RData"))

# get mod scores
mod_res <- map(subgraphs, function(x) unique(V(x)$mod))
mod_res <- do.call(rbind, mod_res)

similarity_ratings <- function(data) {
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

  # normalize the ratings?
  subject_rating_temp$responsenormalized <- (subject_rating_temp$response - min(subject_rating_temp$response)) / range(subject_rating_temp$response) # xnormalized = (x - min(x)) / range(x)
  subject_rating_temp$modularity <- mod_res

  return(subject_rating_temp)
}

res_list <- map(temp_files, similarity_ratings)

res <- do.call(rbind, res_list)

# make the mod scores only positive for graph
# res$modularity <-  abs(min(res$modularity)) + res$modularity

ggplot(res, aes(response, modularity)) +
  stat_summary(
    fun.data = "mean_cl_boot",
    geom = "pointrange",
    colour = "red"
  ) +
  theme_classic() +
  geom_smooth(method = "lm", se = F, size = 1.7, color = "black") +
  labs(y = "Q", x = "Similarity")

compares <- res %>%
  group_by(stimulus) %>%
  summarise(
    mean = mean(response),
    se = sqrt(var(response) / length(response))
  )

compares$mod <- mod_res

compares$absmod <- abs(compares$mod)

cor.test(compares$mean, compares$mod)
# correlation with absolute value seems to suggest a relation.
cor.test(compares$mean, compares$absmod)

mod <- lm(mean ~ poly(mod, 2), compares)
summary(mod)
plot(ggpredict(mod))

# you can also see a quadratic relationship with our similarity score
ggplot(compares, aes(mod, mean)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, size = 1.8, color = "black") +
  theme_classic() +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se)) +
  labs(x = "Q", y = "Similarity") +
  scale_y_continuous(limits = c(20, 95))

plot(subgraphs[[32]]) # highests score, lowest variance

plot(subgraphs[[73]]) # highests variance

plot(subgraphs[[2]]) # lowest score
