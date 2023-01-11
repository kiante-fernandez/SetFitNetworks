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
    color = "Modularity Difference (L-R)"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))

b <- df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    ov = left_rating + right_rating,
    on = left_net + right_net
  ) %>%
  mutate(
    binned_value_diff = as.numeric(cut_number(ov, 7)) - 4,
  ) %>%
  group_by(subject_id, binned_value_diff) %>%
  mutate(
    binned_net_diff = as.numeric(cut_number(on, 3)) - 2
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
    x = "Sum of Ratings (L+R)",
    color = "Sum of Modularity (L+R)"
  ) +
  theme(text = element_text(size = 15),
        legend.position = c(0.25, 0.85),
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
    color = "|Modularity Difference| (L-R)"
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        legend.position = c(0.25, 0.18),
        axis.title = element_text(face="bold")
        )

# 
d <- df %>%
  exlusions() %>%
  group_by(subject_id) %>%
  mutate(
    vd = left_rating + right_rating,
    nd = left_net + right_net
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
    x = "Sum of Ratings (L+R)",
    color = "Sum of Modularity (L+R)"
  )+
  theme(axis.text = element_text(face="bold"),
        text = element_text(size = 15),
        legend.position = c(0.8, 0.8),
        axis.title = element_text(face="bold")
  )
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

