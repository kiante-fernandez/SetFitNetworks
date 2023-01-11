#initial analysis of the similarity rating data

library(igraph)

library(ggeffects)

temp_files <- list.files(path = here::here("data", "exp_2"), pattern = ".json", full.names = T)
load(file = here::here("data", "modularity_100_6.RData"))

# get mod scores
mod_res <- map(subgraphs, function(x) unique(V(x)$mod))
mod_res <- do.call(rbind, mod_res)

data <- temp_files[[1]]
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
  subject_rating_temp$subject_id <- unique(subject_temp$subject_id)

  return(subject_rating_temp)
}

res_list <- map(temp_files, similarity_ratings)

res <- do.call(rbind, res_list)

# make the mod scores only positive for graph
# res$modularity <-  abs(min(res$modularity)) + res$modularity

# res$modularity <- scale(res$modularity)
# ggplot(res, aes(modularity,response)) +
#   stat_summary(
#     fun.data = "mean_cl_boot",
#     geom = "pointrange",
#     colour = "red",
#     size = .7
#   ) +
#   theme_classic() +
#   geom_smooth(method = "lm", se = F, size = 1.6, color = "black") +
#   labs(x = "Q", y = "Similarity Judgment")+
#   theme(axis.text = element_text(face="bold"),
#         text = element_text(size = 15),
#         axis.title = element_text(face="bold")
#   )
# 

res
ggplot(res, aes(response))+geom_histogram()

m0 <- lmer(response ~  poly(modularity, 2) + (poly(modularity, 2) | subject_id), data = res)
m0 <- brm(response ~  poly(modularity, 2) + (poly(modularity, 2) | subject_id), data = res)
bayestestR::sexit(m0)
summary(m0)
plot(ggpredict(m0, terms="modularity [all]"))

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
  theme_classic() +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se), size = .7, color = "red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, size = 1.8, color = "black") +
  labs(x = "Q", y = "Similarity") +
  scale_y_continuous(limits = c(25, 90))+ 
  # scale_x_continuous(limits = c(-0.6, 0.7))+ 
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  )

plot(subgraphs[[32]]) # highests score, lowest variance

plot(subgraphs[[73]]) # highests variance

plot(subgraphs[[2]]) # lowest score
