#draft plot scrirts
tune <- .7
df[df$cluster_condition == T,] %>% 
  mutate(eq = left_cluster_condition == right_cluster_condition) %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  ungroup() %>%
  mutate(vd = left_rating - right_rating) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(vd)) %>%
  ungroup() %>%
  mutate(
    binned_value_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(binned_value_diff, eq) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = binned_value_diff, y = m_left, color = eq)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)"
  )

df[df$cluster_condition == T,] %>% 
  mutate(eq = left_cluster_condition == right_cluster_condition) %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  ungroup() %>%
  mutate(nd = left_net - right_net) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(nd)) %>%
  ungroup() %>%
  mutate(
    binned_net_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(binned_net_diff, eq) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>% 
  ggplot(aes(x = binned_net_diff, y = m_left, color = eq)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Network Difference (L-R)"
  )

tune <- 80
plt_dat <- df %>%
  mutate(eq = left_cluster_condition == right_cluster_condition) %>% 
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  filter(subject_id != 4) %>% 
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(avd = abs(left_rating - right_rating)) %>%
  # mutate(avd = abs(left_net - right_net)) %>% 
  group_by(subject_id, cluster_condition, eq) %>%
  ungroup() %>%
  mutate(
    binned_value_diff = tune * round(avd / tune)
    # binned_value_diff = cut(avd, quantile(avd, c(.1,.3,.5,.7,.9)), include.lowest = T)
    # binned_value_diff = cut(avd, 5, include.lowest = T)
  ) %>%
  ungroup() 
plt_dat$binned_value_diff[plt_dat$binned_value_diff > 240] <- 240
plt_dat$cluster_condition <- factor(plt_dat$cluster_condition,labels = c("Not Cluster Comparison", "Cluster Comparison"))
plt_dat$eq <- factor(plt_dat$eq,labels = c("Distinct", "Same"))
plt_dat %>% 
  group_by(binned_value_diff, cluster_condition, eq) %>%
  summarise(n = n(),
            q1 = quantile(rt, .1),
            q3 = quantile(rt, .3),
            q5 = quantile(rt, .5),
            q7 = quantile(rt, .7),
            q9 = quantile(rt, .9)) %>% 
  pivot_longer(cols = q1:q9,
               names_to = "quantiles",
               values_to = "rts"
  ) %>% ungroup() %>% 
  ggplot(aes(x = binned_value_diff, y = rts, group = quantiles, shape = eq, color = quantiles)) +
  theme_classic() +
  geom_point(size = 4) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Absolute Value Difference",
    shape = "cluster compatibility"
  ) + facet_grid(~cluster_condition + eq)


#####group plots
tune <- .9

gp1 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  filter(subject_id != 4) %>% 
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating - right_rating) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(vd)) %>%
  ungroup() %>%
  mutate(
    binned_value_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(binned_value_diff) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_left)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Value Difference (L-R)"
  )

gp2 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  filter(subject_id != 4) %>% 
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(nd = left_net - right_net) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(nd)) %>%
  ungroup() %>%
  mutate(
    binned_network_diff = tune * round(Dif / tune),
  ) %>%
  group_by(binned_network_diff) %>%
  mutate(
    m_left = mean(choice),
    se = sqrt(var(choice) / length(choice))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_network_diff, y = m_left)) +
  geom_pointrange(aes(ymin = m_left - se, ymax = m_left + se)) +
  theme_classic() +
  geom_line(size = 1) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    y = "Probability of Choosing Left",
    x = "Network Difference (L-R)"
  )

tune <- .8


gp3 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  filter(subject_id != 4) %>% 
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(nd = left_net - right_net) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(nd)) %>%
  ungroup() %>%
  mutate(
    binned_network_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(binned_network_diff) %>%
  mutate(
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_network_diff, y = m_rt)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Network Difference (L-R)"
  )

gp4 <- df %>%
  group_by(subject_id) %>%
  mutate(Q1 = quantile(rt, .25),
         Q3 = quantile(rt, .75),
         IQR = IQR(rt)) %>% 
  filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
  filter(subject_id != 8) %>% 
  filter(subject_id != 9) %>% 
  filter(subject_id != 13) %>% 
  filter(subject_id != 4) %>% 
  ungroup() %>%
  filter(!rt <= 250) %>% 
  filter(!rt >= 10000) %>% 
  mutate(vd = left_rating - right_rating) %>%
  group_by(subject_id) %>%
  mutate(Dif = scale(vd)) %>%
  ungroup() %>%
  mutate(
    binned_value_diff = tune * round(Dif / tune),
  ) %>%
  ungroup() %>%
  group_by(binned_value_diff) %>%
  mutate(
    m_rt = mean(rt),
    se = sqrt(var(rt) / length(rt))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = binned_value_diff, y = m_rt)) +
  geom_pointrange(aes(ymin = m_rt - se, ymax = m_rt + se)) +
  theme_classic() +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    y = "RT(s)",
    x = "Value Difference (L-R)"
  )

(gp1 + gp2)/ (gp4 + gp3) +
  plot_annotation(title = paste0("network measure: ", "degree"))
# quantiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)
# ## aggregate data for quantile plot
# TEST <- df %>%
#   group_by(subject_id) %>%
#   arrange(desc(rt)) %>%
#   slice(-(1:remove)) %>%
#   ungroup() %>%
#   mutate(vd = left_rating - right_rating) %>% 
#   mutate(vd_bin = cut(vd, breaks = 5, include.lowest = TRUE)) 
# 
# levels(TEST$vd_bin) <- as.character(-2:2)
# 
# TEST  %>% 
#   group_by(subject_id, vd_bin) %>% 
#   nest() %>% 
#   mutate(quantiles = map(data, ~ as.data.frame(t(quantile(.x$rt, probs = quantiles))))) %>% 
#   unnest(quantiles) %>% 
#   gather("quantile", "rt",`10%`:`90%`) %>% 
#   arrange(subject_id, vd_bin) %>% 
#   group_by(subject_id, vd_bin) %>% 
#   summarise(n = n()) %>% 
#   spread(vd_bin, n) 


# df %>%
#   group_by(subject_id) %>%
#   mutate(Q1 = quantile(rt, .25),
#          Q3 = quantile(rt, .75),
#          IQR = IQR(rt)) %>% 
#   filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
#   filter(subject_id != 8) %>% 
#   filter(subject_id != 9) %>% 
#   filter(subject_id != 13) %>% 
#   filter(subject_id != 4) %>% 
#   ungroup() %>%
#   filter(!rt <= 250) %>% 
#   filter(!rt >= 10000) %>% 
#   mutate(vd = left_rating - right_rating,
#          nd = left_net - right_net) %>% 
#   group_by(subject_id) %>%
#   mutate(vd = scale(vd),
#          nd = scale(nd)) %>%
#   ungroup() %>%
#   select(vd, nd, rt) %>% 
#   ggplot(aes(vd,nd, color = rt, size = rt))+
#   geom_point()+
#   theme_classic() +
#   geom_vline(xintercept = 0, linetype = "dashed", size = .4)+
#   geom_hline(yintercept = 0, linetype = "dashed", size = .4)+
#   labs(
#     y = "Network Difference (L-R)",
#     x = "Value Difference (L-R)",
#     size = "RT(s)",
#     color = "")+
#   scale_colour_gradient(low = "orange", high = "blue")+
#   geom_smooth(method = "lm", color = "red", se = F, linetype = "dashed",size = 2)
# 
#   df %>%
#     group_by(subject_id) %>%
#     mutate(Q1 = quantile(rt, .25),
#            Q3 = quantile(rt, .75),
#            IQR = IQR(rt)) %>% 
#     filter(rt > (Q1 - 2*IQR) & rt < (Q3 + 2*IQR)) %>% 
#     filter(subject_id != 8) %>% 
#     filter(subject_id != 9) %>% 
#     filter(subject_id != 13) %>% 
#     filter(subject_id != 4) %>% 
#     ungroup() %>%
#     filter(!rt <= 250) %>% 
#     filter(!rt >= 10000) %>% 
#   mutate(vd = left_rating - right_rating,
#          nd = left_net - right_net) %>% 
#     group_by(subject_id) %>%
#     mutate(vd = scale(vd),
#            nd = scale(nd)) %>%
#     ungroup() %>%
#   select(vd, nd, choice) %>% 
#   ggplot(aes(vd,nd, color = factor(choice)))+
#   scale_color_brewer(palette = "Set1")+
#   geom_point(size = 2)+
#   theme_classic() +
#   geom_vline(xintercept = 0, linetype = "dashed", size = .6)+
#   geom_hline(yintercept = 0, linetype = "dashed", size = .6)+
#   labs(
#     y = "Network Difference (L-R)",
#     x = "Value Difference (L-R)",
#     color = "Choice Left")

#testing quantile regression
# library(quantreg)
# #fit model
# model <- rq(log(rt) ~ vd*nd, data = model_dat, tau = c(0.1, 0.3, 0.5, 0.7, 0.9))
# #view summary of model
# summary(model)

