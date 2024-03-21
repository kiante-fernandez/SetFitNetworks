# utils_plotting.R - helper functions for plotting data using ggplpot

# Copyright (C) 2023 Kianté Fernandez, <kiantefernan@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 2023/01/09      Kianté  Fernandez                     coded up version one

#TODO you need to make functions out of these plotting commands
library(tidyverse)
source(here::here("src", "organize_group_data_v2.R"))
# df <- organize_group_data(experiment = 2, net_stat = "modularity")
df <- organize_group_data(experiment = 1)
# df <- organize_group_data(experiment = 3)

net_stats <- c("weighted_transitivity", "modularity", "conductance", "pca1", "pca2")

net_idx = 5

df$left_net <-   select(df,contains(net_stats[[net_idx]]))[[1]]
df$right_net <-   select(df,contains(net_stats[[net_idx]]))[[2]]

# df$left_net <-   df$left_sd
# df$right_net <-   df$right_sd
df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    nd = left_net - right_net
  ) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd, 7)) - 4,
  ) %>%
  group_by(binned_net_diff) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_net_diff, y = m_left)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "Network Difference (L-R)",
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

midRound <- function(x, base){
  base*round(x/base)
}

them <- theme_classic() + 
  theme(panel.background = element_rect(fill = "white", color = "black")) + 
  theme(panel.grid.major = element_line(color = "grey90")) + 
  theme(plot.title = element_text(size = 28)) + 
  theme(axis.title.x = element_text(size = 20)) + 
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text = element_text(size = 20)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 15))

a <- df %>%
  exlusions() %>%
  select(choice,left_rating,right_rating,subject_id) %>% 
  mutate(left_rating = midRound(left_rating, 35)) %>% 
  mutate(right_rating = midRound(right_rating, 35)) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = tidyselect::ends_with("rating"),
    names_to = "side",
    values_to = "rating",
  ) %>% group_by(rating, side) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = rating, y = m_left, group = side, color= side)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  # geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1", labels=c('Left', 'Right')) +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "Liking Rating",
    color = "Set"
  )+ them 

b <- df %>%
  exlusions() %>%
  select(choice,left_net_pca2,right_net_pca2, subject_id) %>% 
  group_by(subject_id) %>% 
  mutate(left_net_pca2 = midRound(left_net_pca2, 1.5)) %>% 
  mutate(right_net_pca2 = midRound(right_net_pca2, 1.5)) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = tidyselect::contains("net"),
    names_to = "side",
    values_to = "network",
  ) %>% group_by(network, side) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = network, y = m_left, group = side, color= side)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  # geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1", labels=c('Left', 'Right')) +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "PCA 2",
    color = "Set"
  )+ them 

c <- df %>%
  exlusions() %>%
  select(choice,left_net_pca1,right_net_pca1, subject_id) %>% 
  group_by(subject_id) %>% 
  mutate(left_net_pca1 = midRound(left_net_pca1, 1.75)) %>% 
  mutate(right_net_pca1 = midRound(right_net_pca1, 1.75)) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = tidyselect::contains("net"),
    names_to = "side",
    values_to = "network",
  ) %>% group_by(network, side) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = network, y = m_left, group = side, color= side)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  # geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1", labels=c('Left', 'Right')) +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "PCA 1",
    color = "Set"
  )+ them 

a | c/b



df %>%
  exlusions() %>%
  select(choice,left_sim,right_sim, subject_id) %>% 
  mutate(left_sim = midRound(left_sim, 6)) %>%
  mutate(right_sim = midRound(right_sim, 6)) %>%
  ungroup() %>% 
  pivot_longer(
    cols = tidyselect::contains("sim"),
    names_to = "side",
    values_to = "network",
  ) %>% group_by(network, side) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = network, y = m_left, group = side, color= side)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  # geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1", labels=c('Left', 'Right')) +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "Similarity Judgment",
    color = "Set"
  )+ them 


####response time plots
df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    nd = abs(left_net_pca1 - right_net_pca1)
  ) %>% 
  mutate(nd = midRound(nd, 2)) %>%
  group_by(nd) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = nd, y = m_rt)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    y = "RT(s)",
    x = "|PCA 1| (L-R)",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        # legend.position = c(0.25, 0.18),
        axis.title = element_text(face="bold")
  ) +
  geom_smooth(method= "lm", color = "red")

    
df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    nd = abs(left_net_pca2 - right_net_pca2)
  ) %>% 
  mutate(nd = midRound(nd, .8)) %>%
  group_by(nd) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = nd, y = m_rt)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    y = "RT(s)",
    x = "|PCA 2| (L-R)",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        # legend.position = c(0.25, 0.18),
        axis.title = element_text(face="bold")
  ) + scale_x_continuous(limits = c(0, 14))+
  geom_smooth(method= "lm", color = "red")



df %>%
  exlusions() %>%
  # group_by(subject_id) %>%
  mutate(
    vd = abs(left_rating - right_rating)
    # ov = left_rating + right_rating
  ) %>% 
  mutate(vd = midRound(vd, 30)) %>%
  group_by(vd) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>%
  filter(vd < 400) %>% 
  ggplot(aes(x = vd, y = m_rt)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    y = "RT(s)",
    x = "|Value difference| (L-R)",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  ) +
  geom_smooth(method= "lm", color = "red")

df %>%
  exlusions() %>%
  # group_by(subject_id) %>%
  mutate(
    # nd = abs(left_net_pca2 + right_net_pca2)
    ov = left_rating + right_rating
  ) %>% 
  mutate(ov = midRound(ov, 50)) %>%
  group_by(ov) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = ov, y = m_rt)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    y = "RT(s)",
    x = "sum value (L+R)",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        # legend.position = c(0.25, 0.18),
        axis.title = element_text(face="bold")
  ) +
  geom_smooth(method= "lm", color = "red")

df %>%
  exlusions() %>%
  dplyr::filter(correct == 0) %>%
  mutate(
    nd = left_net_pca2 - right_net_pca2,
    vd = left_rating - right_rating
  ) %>% 
  # dplyr::filter(vd == 0) %>%
  mutate(nd = midRound(nd, 2.3)) %>%
  group_by(subject_id, nd) %>% 
  mutate(
    q1 = quantile(rt, .1),
    q3 = quantile(rt, .3),
    q5 = quantile(rt, .5),
    q7 = quantile(rt, .7),
    q9 = quantile(rt, .9),
  ) %>%
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% ungroup() %>% 
  group_by(quantiles,nd) %>%
  mutate(n = n(),
         m_rt = mean(rts),
         se = sqrt(var(rts) / length(rts))
  ) %>% 
  ggplot(aes(x = nd, y = m_rt, color = factor(quantiles))) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Network Difference (L-R)"
  ) + theme(legend.position="none")+
  scale_x_continuous(limits = c(-16, 16))+
  scale_y_continuous(limits = c(900, 4600))


## make a plot of the vd:nd interaction
a <- df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    vd = left_rating - right_rating,
    nd = left_net - right_net
  ) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd, 7)) - 4,
  ) %>%
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd, 3)) - 2
  ) %>%
  group_by(binned_net_diff, binned_value_diff) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)",
    color = "Network Difference (L-R)"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

b <- df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    vd = left_rating - right_rating,
    nd = left_net - right_net
  ) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd, 7)) - 4,
  ) %>%
  group_by(subject_id, binned_net_diff) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd, 3)) - 2
  ) %>%
  group_by(binned_net_diff, binned_value_diff) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_net_diff, y = m_left, color = factor(binned_value_diff))) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    # title = paste0(net_stats[[net_idx]]),
    y = "Probability of Choosing Left",
    x = "Network Difference (L-R)",
    color = "Value Difference (L-R)"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.20, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))


c <- df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    vd = abs(left_rating - right_rating),
    nd = abs(left_net - right_net)
  ) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd, 5)) - 1,
  ) %>%
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd, 3)) - 1
  ) %>%
  group_by(binned_net_diff, binned_value_diff) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "|Value Difference| (L-R)",
    color = "|Network Difference| (L-R)"
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        legend.position = c(0.25, 0.18),
        axis.title = element_text(face="bold")
        )

# 
d <-  df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    vd = abs(left_rating - right_rating),
    nd = abs(left_net - right_net)
  ) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd, 5)) - 1,
  ) %>%
  group_by(subject_id, binned_net_diff) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(vd, 3)) - 1
  ) %>%
  group_by(binned_net_diff, binned_value_diff) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_net_diff, y = m_rt, color = factor(binned_value_diff))) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    y = "RT(s)",
    x = "|Network Difference| (L-R)",
    color = "|Value Difference| (L-R)"
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        # legend.position = c(0.25, 0.18),
        axis.title = element_text(face="bold")
  )

df %>% 
  exlusions() %>% 
  mutate(correct = factor(correct)) %>% 
  ggplot(aes(rt, fill = correct)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, bins = 20) +
  geom_density(lwd = 1, alpha = 0.25)+
  # facet_grid(~correct)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()



#model prediction plot. Can we just get the data on it?
# plot(ggeffects::ggpredict(models_choice[[3]], terms = c("zleft_rating [all]", "zleft_net[-1.5, 0 ,1.5]")))+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   geom_hline(yintercept = 0.5, linetype = "dashed")+
#   theme_classic()+
#   labs(
#     title = "",
#     y = "Probability of Choosing Left",
#     x = "left rating",
#     color = "network score"
#   ) +
#   theme(text = element_text(size = 15),
#         legend.position = c(0.25, 0.85),
#         axis.text = element_text(face="bold"),
#         axis.title = element_text(face="bold"))
# 


# 
# # correct absolute value plot
# print(df %>%
#         exlusions() %>%
#         group_by(subject_id) %>%
#         # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
#         # filter(eq == 1) %>%
#         mutate(
#           vd = abs(left_rating - right_rating),
#           nd = abs(left_net - right_net)
#         ) %>%
#         mutate(
#           binned_value_diff = as.numeric(cut_number(vd, 4)) - 1,
#         ) %>%
#         group_by(binned_value_diff) %>%
#         mutate(
#           binned_net_diff = as.numeric(cut_number(nd, 6)) - 1
#         ) %>%
#         group_by(binned_net_diff, binned_value_diff) %>%
#         mutate(
#           n = n(),
#           m_correct = mean(correct),
#           se = sqrt(var(correct) / length(correct))
#         ) %>%
#         ungroup() %>%
#         ggplot(aes(x = binned_net_diff, y = m_correct, color = factor(binned_value_diff))) +
#         geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
#         theme_classic() +
#         geom_line(size = 1) +
#         geom_hline(yintercept = .5, linetype = "dashed") +
#         scale_color_brewer(palette = "Set1") +
#         scale_y_continuous(limits = c(0, 1.01)) +
#         labs(
#           title = paste0(net_stats[[net_idx]]),
#           y = "Accuracy",
#           x = "Absolute Network Difference (L-R)",
#           color = "Absolute Value Difference (L-R)"
#         ) +
#         theme(legend.position = "top"))
# 
# 
# print(df %>%
#         exlusions() %>%
#         group_by(subject_id) %>%
#         # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
#         # filter(eq == 0) %>%
#         mutate(nd = abs(left_net - right_net)) %>%
#         mutate(
#           binned_net_diff = as.numeric(cut_number(nd, 10)) - 1
#         ) %>%
#         group_by(binned_net_diff) %>%
#         mutate(
#           n = n(),
#           m_correct = mean(correct),
#           se = sqrt(var(correct) / length(correct))
#         ) %>%
#         ungroup() %>%
#         ggplot(aes(x = binned_net_diff, y = m_correct)) +
#         geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
#         theme_classic() +
#         geom_line(size = 1) +
#         geom_hline(yintercept = .5, linetype = "dashed") +
#         scale_color_brewer(palette = "Set1") +
#         scale_y_continuous(limits = c(0, 1.01)) +
#         labs(
#           title = paste0(net_stats[[net_idx]]),
#           y = "Accuracy",
#           x = "Absolute Network Difference (L-R)"
#         ) +
#         theme(legend.position = "top"))

# ##make a plot of the vd:nd interaction
# plt <- df %>%
#   exlusions() %>%
#   group_by(subject_id) %>%
#   mutate(vd = left_rating - right_rating,
#          nd = left_net - right_net) %>%
#   mutate(
#     binned_value_diff = as.numeric(cut_number(vd,5)) - 3,
#   ) %>%
#   group_by(subject_id,binned_value_diff) %>%
#   mutate(
#     binned_net_diff = as.numeric(cut_number(nd,3 )) - 2
#   ) %>%
#   group_by(binned_net_diff,binned_value_diff) %>%
#   mutate(n = n(),
#          m_left = mean(choice),
#          se = sqrt(var(choice) / length(choice))
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
#   geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   geom_hline(yintercept = .5, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   scale_y_continuous(limits = c(0, 1.01)) +
#   labs(title = paste0(net_stats[[net_idx]]),
#     y = "Probability of Choosing Left",
#     x = "Value Difference (L-R) bins",
#     color = "Network Difference (L-R) bins"
#   )
# # print(plt)
# df %>%
#   exlusions() %>%
#   group_by(subject_id) %>%
#   mutate(vd = abs(left_rating - right_rating),
#          nd = abs(left_net - right_net)) %>%
#   mutate(
#     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
#       ) %>%
#   group_by(subject_id,binned_net_diff) %>%
#   mutate(
#     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
#   ) %>%
#   group_by(binned_net_diff,binned_value_diff) %>%
#   mutate(n = n(),
#          m_c = mean(correct),
#          se = sqrt(var(correct) / length(correct))
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = binned_value_diff, y = m_c, color = factor(binned_net_diff))) +
#   geom_pointrange(aes(ymin = m_c - se, ymax = m_c + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   geom_hline(yintercept = .5, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   labs(title = paste0(net_stats[[net_idx]]),
#        y = "Accuracy",
#        x = "Absolute Value Difference (L-R)",
#        color = "Absolute Network Difference (L-R)"
#   )
# df %>%
#   exlusions() %>%
#   group_by(subject_id) %>%
#   mutate(vd = abs(left_rating - right_rating),
#          nd = abs(left_net - right_net)) %>%
#   mutate(
#     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
#   ) %>%
#   group_by(subject_id,binned_value_diff) %>%
#   mutate(
#     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
#   ) %>%
#   group_by(binned_net_diff,binned_value_diff) %>%
#   mutate(n = n(),
#          m_rt = mean(rt),
#          se = sqrt(var(rt) / length(rt))
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
#   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   scale_color_brewer(palette = "Set1") +
#   labs(title = paste0(net_stats[[net_idx]]),
#     y = "RT(ms)",
#     x = "Absolute Value Difference (L-R)",
#     color = "Absolute Network Difference (L-R)"
#   )
# df %>%
#   exlusions() %>%
#   group_by(subject_id) %>%
#   mutate(vd = left_rating + right_rating,
#          nd = left_net + right_net) %>%
#   mutate(
#     binned_value_diff = as.numeric(cut_number(vd,5)) - 1,
#   ) %>%
#   group_by(subject_id,binned_value_diff) %>%
#   mutate(
#     binned_net_diff = as.numeric(cut_number(nd,3)) - 1
#   ) %>%
#   group_by(binned_net_diff,binned_value_diff) %>%
#   mutate(n = n(),
#          m_rt = mean(rt),
#          se = sqrt(var(rt) / length(rt))
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = binned_value_diff, y = m_rt, color = factor(binned_net_diff))) +
#   geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   scale_color_brewer(palette = "Set1") +
#   labs(title = paste0(net_stats[[net_idx]]),
#        y = "RT(ms)",
#        x = "Absolute Value Magnitude (L+R)",
#        color = "Absolute Network Magnitude (L+R)"
#   )
#
# print(plt)
#
# plts[[net_idx]] <- plt

# #correct absolute value plot
# print(df %>%
#   exlusions() %>%
#   # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
#   # filter(eq == 1) %>%
#   mutate(vd = abs(left_rating - right_rating),
#          nd = abs(left_net - right_net)) %>%
#   mutate(
#     binned_value_diff = as.numeric(cut_number(vd,4)) - 1,
#   ) %>%
#   group_by(binned_value_diff) %>%
#   mutate(
#     binned_net_diff = as.numeric(cut_number(nd,4)) - 1
#   ) %>%
#   group_by(binned_net_diff,binned_value_diff) %>%
#   mutate(n = n(),
#          m_correct = mean(correct),
#          se = sqrt(var(correct) / length(correct))
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = binned_net_diff, y = m_correct, color = factor(binned_value_diff))) +
#   geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   geom_hline(yintercept = .5, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   scale_y_continuous(limits = c(0, 1.01)) +
#   labs(title = paste0(net_stats[[net_idx]]),
#        y = "Accuracy",
#        x = "Absolute Network Difference (L-R)",
#        color = "Absolute Value Difference (L-R)"
#   ) +  theme(legend.position="top"))
# #
# #
# print(df %>%
#   exlusions() %>%
#   # mutate(eq = left_cluster_condition == right_cluster_condition) %>%
#   # filter(eq == 0) %>%
#   mutate(nd = abs(left_net - right_net)) %>%
#   mutate(
#     binned_net_diff = as.numeric(cut_number(nd,4)) - 1
#   ) %>%
#   group_by(binned_net_diff) %>%
#   mutate(n = n(),
#          m_correct = mean(correct),
#          se = sqrt(var(correct) / length(correct))
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x = binned_net_diff, y = m_correct)) +
#   geom_pointrange(aes(ymin = m_correct - se, ymax = m_correct + se)) +
#   theme_classic() +
#   geom_line(size = 1) +
#   geom_hline(yintercept = .5, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   scale_y_continuous(limits = c(0, 1.01)) +
#   labs(title = paste0(net_stats[[net_idx]]),
#        y = "Accuracy",
#        x = "Absolute Network Difference (L-R)"
#   ) +  theme(legend.position="top"))
(a + b)/(c + d )+ plot_annotation(tag_levels = 'A')












source("exploratory_graph_analysis.R")
source(here::here("src", "utils.R"))
library(RColorBrewer)

brewer.pal(n = 7, name = 'Dark2')

G <- g
E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))

net_degree <- calculate_net_stats(g)

l <- layout_nicely(G)
l <- layout_with_graphopt(G)
l <- layout.mds(G)

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
# net_degree <- net_degree%>% for coloring the consensuss 6 solution
#   mutate(colors =
#            case_when(
#              snack_type == 1 ~ "#D95F02",
#              snack_type == 2 ~ "#E6AB02",
#              snack_type == 3 ~ "#1B9E77",
#              snack_type == 4 ~ "#66A61E",
#              snack_type == 5 ~ "#7570B3",
#              snack_type == 6 ~ "#E7298A",
#            )
#   )
# net_degree <- net_degree%>% 
#   mutate(colors = 
#            case_when(  
#              snack_type == 1 ~ "#1B9E77",
#              snack_type == 2 ~ "#D95F02",
#              snack_type == 3 ~ "#7570B3",
#              snack_type == 4 ~ "#E7298A",
#              snack_type == 5 ~ "#66A61E"
#            )
#   )
V(g)$color <- net_degree$colors

E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

plot(g,
     layout = l,
     margin = .0,
     # vertex.label = V(g)$name,
     vertex.label = NA,
     vertex.label.color = "black",
     label.font = 2,
     vertex.frame.color=adjustcolor(net_degree$colors, alpha.f = .1),
     # vertex.label.degree = 0,
     vertex.label.dist	= 1,
     vertex.label.cex = 1,
     vertex.size = 13,
     vertex.label.family = "Times",
     # edge.curved = .1,
     edge.width = E(g)$weight * 5
)

legend(x=1.1, 
       y=1, 
       # c("Savory","Fruit","Dessert","Chocolate","Chip","Cracker","Bread"), 
       c("meat/cheese","fruits/vegetables","sweet pastries","chocolate","chips","crackers","bread"), 
       pch=21, 
       pt.bg=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", 
               "#A6761D"),
       pt.cex=2, 
       cex=.8, 
       bty="n", 
       ncol=1)


plot(g,
     layout = l,
     # margin = .0,
     vertex.shape="none", 
     vertex.label.cex=.8,
     vertex.label = V(g)$name,
     vertex.label.font = 2,
     # vertex.label = NA,
     vertex.label.color=net_degree$colors,
     vertex.size = NULL,
     vertex.label.family = "Times",
     # edge.curved = .1,
     # edge.color= "grey",
     edge.width = E(g)$weight,
)

legend(x=1, 
       y=0, 
       # c("Savory","Fruit","Dessert","Chocolate","Chip","Cracker","Bread"), 
       c("meat/cheese","fruits/vegetables","sweet pastries","chocolate","chips","crackers","bread"), 
       pch=21, 
       pt.bg=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", 
               "#A6761D"),
       pt.cex=2, 
       cex=.8, 
       bty="n", 
       ncol=1)

load(file = here::here("data", "modularity_100_6.RData"))

V(g)$color <- net_degree$colors

E(g)$color[E(g)$weight > 0] <- "forestgreen"
E(g)$color[E(g)$weight < 0] <- "red2"

V(subgraphs[[32]])$color <-  net_degree[net_degree$Name %in% V(subgraphs[[32]])$name,]$colors
V(subgraphs[[2]])$color <-  net_degree[net_degree$Name %in% V(subgraphs[[2]])$name,]$colors

plot(subgraphs[[2]],
     # layout = layout.circle(subgraphs[[32]]),
     margin = .0,
     # vertex.label = V(g)$name,
     vertex.label = NA,
     vertex.label.color = "black",
     label.font = 2,
     # vertex.frame.color=adjustcolor(net_degree$colors, alpha.f = .1),
     # vertex.label.degree = 0,
     vertex.label.dist	= 1,
     vertex.label.cex = 1,
     vertex.size = 30,
     vertex.label.family = "Times",
     edge.width = abs(E(subgraphs[[2]])$weight) * 100,
)



# ega_res
# 
# ega_res$summary.table
# ega_res$frequency
# 
# stab <- EGAnet::dimensionStability(ega_res)
# 
# stab$dimension.stability
# stab$item.stability

library(aricode)

NMI(cl,iris$Species)


df %>% 
  exlusions() %>%
  select(subject_id, left_rating,right_rating, left_net, right_net) %>% 
  group_by(subject_id) %>%
  #add constant
  mutate(left_net = left_net + c,
         right_net = right_net + c) %>% 
  mutate(
    vd = left_rating - right_rating,
    nd = left_net - right_net
  ) %>% View
  
c = 20
df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  #add constant
  mutate(left_net = left_net + c,
         right_net = right_net + c) %>% 
  mutate(
    vd = left_rating - right_rating,
    nd = left_net - right_net
  ) %>% 
  mutate(
    binned_value_diff = as.numeric(cut_number(vd, 9)) - 5,
  ) %>%
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(nd, 2)) - 1
  ) %>%
  group_by(binned_net_diff, binned_value_diff) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_left, color = factor(binned_net_diff))) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se), size = 1.1) +
  theme_classic() +
  geom_line(size = 2) +
  geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)",
    color = "Network Difference (L-R)"
  ) 



library(sjPlot)
library(magrittr)
library(ggplot2)
library(ggeffects)
library(rstantools)
pca1_exp_1_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/pca1_exp_1_fit_choice03.rds")
pca1_exp_2_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/pca1_exp_2_fit_choice03.rds")
pca1_exp_3_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/pca1_exp_3_fit_choice03.rds")
#set facilitation models. var name is wrong here
pca1_exp_1_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/strength_exp_1_fit_choice03.rds")
pca1_exp_2_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/strength_exp_2_fit_choice03.rds")
pca1_exp_3_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/strength_exp_3_fit_choice03.rds")


plot_model(pca1_exp_1_fit_choice03, type = "pred", terms = c("zleft_net1[-1,1]","zleft_net2[-1,1]", "zleft_rating [0]", "zleft_rating [0]"))
plot(ggeffects::ggpredict(pca1_exp_1_fit_choice03, terms = c("zleft_net2[all]", "zleft_rating [0]", "zright_rating [0]")))
plot(ggeffects::ggpredict(pca1_exp_2_fit_choice03, terms = c("zleft_net2[all]", "zleft_rating [0]", "zright_rating [0]")))
plot(ggeffects::ggpredict(pca1_exp_3_fit_choice03, terms = c("zleft_net2[all]", "zleft_rating [0]", "zright_rating [0]")))

plot(ggeffects::ggpredict(pca1_exp_1_fit_choice03, terms = c("zleft_net1[all]", "zleft_rating [0]", "zright_rating [0]")))
plot(ggeffects::ggpredict(pca1_exp_2_fit_choice03, terms = c("zleft_net1[all]", "zleft_rating [0]", "zright_rating [0]")))
plot(ggeffects::ggpredict(pca1_exp_3_fit_choice03, terms = c("zleft_net1[all]", "zleft_rating [0]", "zright_rating [0]")))


plot_model(pca1_exp_1_fit_choice03, type = "pred", terms = c("zleft_net2[all]", "zleft_rating [0]", "zright_rating [0]"))
plot_model(pca1_exp_1_fit_choice03, type = "pred", terms = c("zleft_net1[all]", "zleft_rating [0]", "zright_rating [0]"))

# tes <- posterior_predict(pca1_exp_1_fit_choice03)

plot_model(pca1_exp_2_fit_choice03, type = "pred", terms = c("zleft_rating [all]","zleft_net1[-1,1]","zleft_net2[-1,1]"))
plot_model(pca1_exp_3_fit_choice03, type = "pred", terms = c("zleft_rating [all]","zleft_net1[-1,1]","zleft_net2[-1,1]"))

plot_model(pca1_exp_3_fit_choice03, type = "pred", terms = c("zleft_rating [all]","zleft_net2[-2,0,2]"))
plot_model(pca1_exp_3_fit_choice03, type = "pred", terms = c("zright_rating [all]","zright_net2[-2,0,2]"))


test <- plot_models(pca1_exp_1_fit_choice03,
                    pca1_exp_2_fit_choice03,
                    pca1_exp_3_fit_choice03,
                    transform = NULL,
                    show.values = TRUE,
                    show.p = FALSE,
                    m.labels = c("Experiment One", "Experiment Two", "Experiment Three"),
                    ci.lvl = 0.95)
# write.csv(test$data, here::here("data","internal_meta_analysis_choice.csv"), row.names=FALSE)
     
pd <- position_dodge(.4)
plt_data <- test$data
plt_data <- plt_data[plt_data$term != "b_zleft_sim",]
plt_data <- plt_data[plt_data$term != "b_zright_sim",]
plt_data$term <- factor(plt_data$term)
dput(levels(plt_data$term))

levels(plt_data$term) <- c("right rating × net","left rating × net", 
                           "right net", "right liking rating", 
                           "left net", "left liking rating", 
                           "intercept")

# levels(plt_data$term) <- c("right rating × PCA2", "right rating × PCA1", 
#                            "left rating × PCA2", "left rating × PCA1", "right PCA2", 
#                            "right PCA1", "right liking rating", "left PCA2", "left PCA1", 
#                            "left liking rating", "intercept")

# plt_data <- plt_data[stringr::str_detect(plt_data$term, "PCA1") == FALSE,]
# plt_data <- plt_data[stringr::str_detect(plt_data$term, "net") == FALSE,]

plt_data <- plt_data[stringr::str_detect(plt_data$term, "×") == FALSE,]

plt_data %>% 
  dplyr::filter(term != "intercept") %>% 
  dplyr::mutate(estimate =  round(estimate, 2),
                conf.low = round(conf.low, 2),
                conf.high = round(conf.high, 2)) %>% 
  ggplot(aes(y = forcats::fct_reorder(term, estimate), color = group)) +
  theme_classic()+
  geom_point(aes(x=estimate), shape=15, size=3,position = pd) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), position = pd, size=.8)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .2)+
  scale_color_brewer(palette = "Set1")+
  labs(
    y = "terms",
    x = "estimate",
    color = ""
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 20),
        axis.title = element_text(face="bold")
  )
  # geom_text(aes( label = paste0(estimate, " [", conf.low,",",conf.high, "]"), 
  #                x = estimate, y = term, group = group, color = group), 
  #           position = pd, vjust = -0.7,size=3,
  #           show.legend = FALSE, check_overlap = FALSE)


pca1_exp_1_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/pca1_exp_1_fit_rt02.rds")
pca1_exp_2_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/pca1_exp_2_fit_rt02.rds")
pca1_exp_3_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/pca1_exp_3_fit_rt02.rds")
#note the wrong variable names
pca1_exp_1_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/strength_exp_1_fit_rt02.rds")
pca1_exp_2_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/strength_exp_2_fit_rt02.rds")
pca1_exp_3_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/strength_exp_3_fit_rt02.rds")



plot(ggeffects::ggpredict(pca1_exp_1_fit_rt02, terms = c("nd2[-2:2]", "vd [0]")))
plot(ggeffects::ggpredict(pca1_exp_1_fit_rt02, terms = c("nd1[-2:2]", "vd [0]")))

ggeffects::ggpredict(pca1_exp_1_fit_rt02, terms = c("nd2[all]", "vd [0]"))
ggeffects::ggpredict(pca1_exp_1_fit_rt02, terms = c("nd1[all]", "vd [0]"))

test <- plot_models(pca1_exp_1_fit_rt02,
                    pca1_exp_2_fit_rt02,
                    pca1_exp_3_fit_rt02,
                    transform = NULL,
                    show.values = TRUE,
                    show.p = FALSE,
                    m.labels = c("Experiment one", "Experiment two","Experiment three"),
                    ci.lvl = 0.95)

pd <- position_dodge(.4)
plt_data <- test$data
dput(levels(plt_data$term))
plt_data <- plt_data[plt_data$term != "b_sd",]
plt_data$term <- factor(plt_data$term)
levels(plt_data$term) <- c("Set-Similarity Difference", 
                           "Overall Value", "Value Difference", "intercept")

levels(plt_data$term) <- c("PCA2 Difference", "PCA1 Difference", 
                           "Overall Value", "Value Difference", "intercept")

plt_data <- plt_data[stringr::str_detect(plt_data$term, "PCA1") == FALSE,]
plt_data <- plt_data[stringr::str_detect(plt_data$term, "Overall") == FALSE,]

plt_data %>% 
  dplyr::filter(term != "intercept") %>% 
  dplyr::mutate(estimate =  round(estimate, 2),
                conf.low = round(conf.low, 2),
                conf.high = round(conf.high, 2)) %>% 
  ggplot(aes(y = forcats::fct_reorder(term, estimate), color = group)) +
  theme_classic()+
  geom_point(aes(x=estimate), shape=15, size=2,position = pd) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), position = pd, size=.8)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .2)+
  scale_color_brewer(palette = "Set1")+
  labs(
    y = "terms",
    x = "estimate",
    color = ""
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 20),
        axis.title = element_text(face="bold")
  )
  # geom_text(aes( label = paste0(estimate, " [", conf.low,",",conf.high, "]"), 
  #                x = estimate, y = term, group = group, color = group), 
  #           position = pd, vjust = -0.7,size=3,
  #           show.legend = FALSE, check_overlap = FALSE)


#####ISDN poster plot
# library(tidyverse)

ISDN_poster_sim_rating_exp2 <- read_csv("data/ISDN_poster_sim_rating_exp2.csv")
ISDN_poster_sim_rating_exp3 <- read_csv("data/ISDN_poster_sim_rating_exp3.csv")


poster_plot_data <- rbind(ISDN_poster_sim_rating_exp2, ISDN_poster_sim_rating_exp3)

poster_plot_data$study <- factor(poster_plot_data$experiment)

ggplot(poster_plot_data, aes(st, subgraph_mean, group = study, color = study)) +
  theme_classic() +
  geom_pointrange(aes(ymin = subgraph_mean - se, ymax = subgraph_mean + se,shape = study), size = .7) +
  geom_smooth(aes(linetype = study, fill = study), method = "lm", se = T, size = 1.8) +
  labs(x = "Subgraph Connectedness", y = "Similarity") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 30),
    axis.title = element_text(face = "bold")
  )+ 
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1") 

ISDN_poster_exp1 <- readr::read_csv("data/ISDN_poster_exp1.csv")
ISDN_poster_exp2 <- readr::read_csv("data/ISDN_poster_exp2.csv")
ISDN_poster_exp3 <- readr::read_csv("data/ISDN_poster_exp3.csv")

ISDN_poster_exp1$subject_id <- ISDN_poster_exp1$subject_id + 100
ISDN_poster_exp2$subject_id <- ISDN_poster_exp2$subject_id + 200
ISDN_poster_exp3$subject_id <- ISDN_poster_exp3$subject_id + 300

ISDN_poster_exp1$study <- 1
ISDN_poster_exp2$study <- 2
ISDN_poster_exp3$study <- 3

cols_select <- c("study","left", "right", "subject_id", "rt", "choice", "network_statistic", 
                 "left_rating", "right_rating", "left_wtrating", "right_wtrating", 
                 "left_net_degree", "right_net_degree", "left_net_strength", "right_net_strength", 
                 "left_net_eigen", "right_net_eigen", "left_net_betweenness", 
                 "right_net_betweenness", "left_net_closeness", "right_net_closeness", 
                 "left_net_weighted_transitivity", "right_net_weighted_transitivity", 
                 "left_net_edge_density", "right_net_edge_density", "left_net_modularity", 
                 "right_net_modularity", "left_net_conductance", "right_net_conductance", 
                 "left_net_pca1", "right_net_pca1", "left_net_pca2", "right_net_pca2",
                 "left_net_set_pca1","right_net_set_pca1","left_net_set_pca2","right_net_set_pca2")

poster_plot_data <- rbind(ISDN_poster_exp1[,cols_select], ISDN_poster_exp2[,cols_select], ISDN_poster_exp3[,cols_select])

poster_plot_data$study <- factor(poster_plot_data$study)

poster_plot_data$study  <- relevel(poster_plot_data$study , ref = "2")
poster_plot_data$study  <- relevel(poster_plot_data$study , ref = "3")
poster_plot_data$study  <- relevel(poster_plot_data$study , ref = "2")

midRound <- function(x, base){
  base*round(x/base)
}


poster_plot_data %>%
  group_by(subject_id) %>%
  mutate(
    vd = left_rating - right_rating
  ) %>%
  mutate(
    binned_net_diff = midRound(vd, 80)
  ) %>%
  filter(binned_net_diff < 350) %>%  #for clear plotting of effect
  filter(binned_net_diff > -350) %>%  #for clear plotting of effect
  group_by(binned_net_diff, study) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = binned_net_diff, y = m_left, fill = study, group = study)) +
  geom_hline(yintercept = .5, size = .25) +
  geom_vline(xintercept = 0, size = .25) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se, color = study), size = 1.3) +
  theme_classic() +
  geom_line(aes(color = study),size = .8) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    y = "P(Left Choosen)",
    x = "Left Set-Liking - Right Set-Liking"
  ) +
  theme(text = element_text(size = 20),
        legend.position = NULL,
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

poster_plot_data %>%
  # filter(subject_id != 235) %>% 
  # filter(subject_id != 228) %>% 
  # filter(subject_id != 375) %>% 
  group_by(subject_id) %>%
  mutate(
    # nd = left_net_weighted_transitivity - right_net_weighted_transitivity,
    nd = left_net_pca2 - right_net_pca2
  ) %>%
  mutate(
    # binned_net_diff = as.numeric(cut_number(nd, 7)) - 4,
    binned_net_diff = midRound(nd, 7)
  ) %>%
  group_by(binned_net_diff, study) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = binned_net_diff, y = m_left, fill = study, group = study)) +
  geom_hline(yintercept = .5, size = .25) +
  geom_vline(xintercept = 0, size = .25) +
  # geom_point(aes(fill = study, color = study, group = study, shape = study),size = 5)+
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se, color = study), size = 1.1) +
  # geom_ribbon(aes(ymin = m_left - se, ymax = m_left + se), alpha = .6)+
  theme_classic() +
  geom_line(aes(color = study),size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0.20, .9)) +
  labs(
    y = "P(Left Choosen)",
    x = "Left Item-Score - Right Item-Score"
  ) +
  theme(text = element_text(size = 20),
        legend.position = NULL,
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

poster_plot_data %>%
  # filter(subject_id != 235) %>% 
  # filter(subject_id != 113) %>% 
  group_by(subject_id) %>%
  mutate(
    vd = left_net_set_pca1 - right_net_set_pca1
  ) %>%
  mutate(
    binned_net_diff = midRound(vd, 3)
  ) %>%
  group_by(binned_net_diff, study) %>%
  mutate(
    n = n(),
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>% 
  ungroup() %>%
  ggplot(aes(x = binned_net_diff, y = m_left, fill = study, group = study)) +
  # geom_point(aes(fill = study, color = study, group = study, shape = study),size = 5)+
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se, color = study), size = 1.1) +
  geom_hline(yintercept = .5, size = .25) +
  geom_vline(xintercept = 0, size = .25) +
  # geom_errorbar(aes(ymin = m_left - se, ymax = m_left + se), alpha = .6)+
  theme_classic() +
  geom_line(aes(color = study),size = 1) +
  # geom_hline(yintercept = .5, linetype = "dashed", size = .25) +
  # geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0.20, .9)) +
  labs(
    y = "P(Left Choosen)",
    x = "Left Set-Score - Right Set-Score"
  ) +
  theme(text = element_text(size = 20),
        legend.position = NULL,
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))


poster_plot_data %>%
  group_by(subject_id) %>%
  mutate(
    vd = abs(left_rating - right_rating)
  ) %>% 
  mutate(vd = midRound(vd, 60)) %>%
  filter(vd < 350) %>%  #for clear plotting of effect
  group_by(vd, study) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = vd, y = m_rt, color = study, group = study)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.5) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "Response Time(s)",
    x = "|Left Set-Liking - Right Set-Liking|",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  ) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1)
  # scale_y_continuous(limits = c(1.9, 3.4))

poster_plot_data %>%
  group_by(subject_id) %>%
  mutate(
    nd = abs(left_net_pca2 - right_net_pca2)
  ) %>% 
  mutate(nd = midRound(nd, 2)) %>%
  filter(nd <= 13) %>%  #for clear plotting of effect
  group_by(nd, study) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = nd, y = m_rt, color = study, group = study)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.5) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "Response Time(s)",
    x = "|Left Item-Score - Right Item-Score|",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  ) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1)+
  scale_y_continuous(limits = c(1.9, 3.4))
  

poster_plot_data %>%
  group_by(subject_id) %>%
  mutate(
    nd = abs(left_net_set_pca1 - right_net_set_pca1)
  ) %>% 
  mutate(nd = midRound(nd, 1)) %>%
  filter(nd < 6) %>%  #for clear plotting of effect
  group_by(nd, study) %>%
  mutate(
    n = n(),
    rt = rt/1000,
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = nd, y = m_rt, color = study, group = study)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se), size = 1.5) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "Response Time(s)",
    x = "|Left Set-Score - Right Set-Score|",
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        axis.title = element_text(face="bold")
  ) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1)+
  scale_y_continuous(limits = c(1.9, 3.4))

# # Create a combined data frame with both vd and nd, and their corresponding response times (rt)
# combined_data <- poster_plot_data %>%
#   mutate(
#     nd = abs(left_net_pca2 - right_net_pca2),
#     vd = abs(left_rating - right_rating),
#     rt_inverted = rt / 1000  # Inverting response times
#   )
# ggplot(combined_data, aes(x = nd, y = vd, z = rt_inverted)) +
#   stat_density_2d(aes(fill = ..level..), geom = "polygon") +
#   scale_fill_gradient(low = "blue", high = "red", 
#                       name = "Response Time\n(Slower ← → Faster)") +  # Adjusted label
#   labs(
#     x = "|Left Set-Score - Right Set-Score|",
#     y = "|Left Set-Liking - Right Set-Liking|"
#   ) +
#   theme_classic() +
#   theme(
#     axis.text = element_text(face="bold"),
#     text = element_text(size = 15),
#     axis.title = element_text(face="bold")
#   )
  

#### stratedy plotting
set_strategy_winner <- set_strategy_winner[set_strategy_winner != "0"]

data_frame <- data.frame(set_strategy_winner) %>% 
  group_by(set_strategy_winner) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))
# Reorder set_strategy_winner by Proportion in descending order
data_frame$set_strategy_winner <- factor(data_frame$set_strategy_winner,
                                         levels = c("temp_res0", "temp_res1", "temp_res2", "temp_res3", "temp_res4"),
                                         labels = c("Higher Average", "Maximum Value", "Excluding Minimum", "Range", "More Fruit"))
data_frame$set_strategy_winner <- factor(data_frame$set_strategy_winner, levels = data_frame$set_strategy_winner[order(data_frame$Proportion)])

# Now plotting
ggplot(data_frame, aes(x = set_strategy_winner, y = Proportion, fill = set_strategy_winner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count, y = Proportion), position = position_stack(vjust = 0.5), size = 6) + # Add counts as text
  theme_classic() +
  labs(x = "Strategy", y = "Proportion", title = "Best Fitting Set Strategy Identifed Per Subject") +
  # scale_x_discrete(labels = c("Maximum Value", "Excluding Minimum", "Range", "More Fruit", "Higher Average")) +
  scale_fill_discrete(name = "Category") +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "none")+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 20),
        axis.title = element_text(face="bold")
  )
  
  
  