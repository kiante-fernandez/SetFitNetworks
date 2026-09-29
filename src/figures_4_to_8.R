# figures_4_to_8.R - Figures 4-8. Usage: Rscript src/figures_4_to_8.R [figure numbers], e.g. 7 8 (default: all).
# Figure 4: value-adjusted P(choose left) by left-set centrality tercile in each set-choice study.
# Figures 5-8 are forest plots of regression coefficients with one shared layout.
# Figure 5 (set choice): panel a = set-level similarity (average strength within set), panel b = item-level
# centrality (PC2). Figure 6 (set-choice RT): absolute similarity difference / absolute centrality difference.
# Figure 7 (single-choice Studies 1-3): panel a = PC2 terms on choice, panel b = absolute PC2 difference on RT.
# Figure 8 (multi-alternative Studies 1-4, nine item-level datasets): PC2 and value x PC2 on choice (a) and RT (b).
# Per-study rows are the posterior draws of the published fits. "Average" rows are per-term
# random-effects meta-analyses (estimate | se ~ 1 + (1 | study), same priors as the internal
# meta-analysis), fit separately per term so left and right terms are not pooled together.
suppressMessages({library(tidyverse); library(brms); library(cmdstanr); library(ggdist); library(patchwork); library(here)})
run <- if (length(commandArgs(TRUE))) as.integer(commandArgs(TRUE)) else 4:8
prior <- c(prior(normal(0, .25), class = Intercept), prior(cauchy(0, .25), class = sd))
study_colors <- c("Study 1" = "#4DAF4A", "Study 2" = "#E41A1C", "Study 3" = "#377EB8", "Average" = "#FF8C00")

# panels: named list of list(fits, label, terms?, xlim?, breaks?); a panel's own terms/xlim/breaks override the defaults.
# fits: character vector of fit file stems; names, if given, are the row labels (else "Study 1", "Study 2", ...).
build_forest <- function(panels, terms = NULL, xlim = NULL, breaks = NULL, file, width = 13, height = 7,
                         colors = study_colors, legend = "Experiment",
                         compose = function(a, b) a + b + plot_layout(guides = "collect")) {
  set.seed(2025)  # per figure, so each figure's draws are the same whichever figures are run
  opt <- function(p, what, default) if (is.null(p[[what]])) default else p[[what]]
  draws <- imap_dfr(panels, function(p, pname) {
    tm <- opt(p, "terms", terms)
    study <- imap_dfr(p$fits, function(f, e) as_draws_df(readRDS(here("fits", paste0(f, ".rds"))), variable = names(tm)) %>%
      select(all_of(names(tm))) %>% slice_sample(n = 4000) %>% pivot_longer(everything(), names_to = "term", values_to = "beta") %>%
      mutate(study = if (is.character(e)) e else paste("Study", e)))
    avg <- map_dfr(names(tm), function(t) {
      d <- study %>% filter(term == t) %>% group_by(study) %>% summarise(estimate = mean(beta), se = sd(beta), .groups = "drop")
      m <- brm(estimate | se(se) ~ 1 + (1 | study), data = d, prior = prior, iter = 10000, cores = 4, backend = "cmdstanr",
               control = list(adapt_delta = 0.999, max_treedepth = 20), refresh = 0, silent = 2, seed = 2025)
      tibble(term = t, beta = sample(as_draws_df(m)$b_Intercept, 4000), study = "Average") })
    bind_rows(study, avg) %>% mutate(panel = pname, term_label = sprintf(tm[term], p$label), term_order = match(term, rev(names(tm)))) })

  plot_df <- draws %>%
    mutate(block = ifelse(study == "Average", "Average", term_label),
           row = ifelse(study == "Average", term_label, study)) %>%
    mutate(study = factor(study, levels = names(colors)))
  labels <- plot_df %>% group_by(panel, block, row, study, term_order) %>%
    mutate(pd = max(mean(beta > 0), mean(beta < 0))) %>% group_by(pd, .add = TRUE) %>% mean_qi(beta) %>% ungroup() %>%
    mutate(lab = sprintf("%.3f [%.3f, %.3f]", beta, .lower, .upper))

  forest <- function(pname) {
    p <- panels[[pname]]; tm <- opt(p, "terms", terms); xl <- opt(p, "xlim", xlim); br <- opt(p, "breaks", breaks)
    d <- plot_df %>% filter(panel == pname); l <- labels %>% filter(panel == pname)
    lv <- c(setdiff(unique(d$block[order(d$term_order)]), "Average"), "Average")
    rows <- c(rev(setdiff(names(colors), "Average")), unique(d$row[d$study == "Average"][order(d$term_order[d$study == "Average"])]))
    d <- d %>% mutate(block = factor(block, levels = lv), row = factor(row, levels = rows))
    l <- l %>% mutate(block = factor(block, levels = lv), row = factor(row, levels = rows))
    divider <- tibble(block = factor("Average", levels = lv), y = length(tm) + 0.7)
    ggplot(d, aes(x = beta, y = row, fill = study)) +
      geom_vline(xintercept = 0, linetype = 2, linewidth = .25) +
      geom_hline(data = divider, aes(yintercept = y), linewidth = .5, color = "gray30") +
      stat_halfeye(.width = c(.5, .95), slab_alpha = .9, point_size = 1.2) +
      geom_text(data = l, aes(x = xl[2], y = row, label = lab), hjust = 1, size = 2.6, nudge_y = .25, inherit.aes = FALSE) +
      facet_grid(rows = vars(block), scales = "free_y", space = "free_y", switch = "y") +
      scale_fill_manual(values = colors, name = legend) +
      scale_x_continuous(breaks = br) + coord_cartesian(xlim = xl) +
      labs(x = "Standardized β Weights", y = NULL) +
      theme_classic(base_size = 10) +
      theme(strip.placement = "outside", strip.background = element_blank(), strip.text.y.left = element_text(face = "italic", angle = 0, hjust = 1),
            axis.text.y = element_text(size = 7), panel.spacing.y = unit(2, "pt"), legend.position = "right")
  }
  fig <- compose(forest(names(panels)[1]), forest(names(panels)[2])) + plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold"))
  ggsave(here("output", paste0(file, ".pdf")), fig, width = width, height = height, device = cairo_pdf)
  ggsave(here("output", paste0(file, ".png")), fig, width = width, height = height, dpi = 200)
  write_csv(labels %>% select(panel, block, row, study, beta, .lower, .upper, pd), here("results", paste0(file, "_values.csv")))
  cat("saved output/", file, ".{pdf,png}\n", sep = "")
  print(as.data.frame(labels %>% select(panel, row, study, lab)), row.names = FALSE)
}

# Figure 4: P(choose left) from a logistic model with both sets' within-participant z-scored values and centralities
# (PC2), evaluated at each left-set centrality tercile's mean with the values and right-set centrality at their means.
# Trials are those of the main-text models: RT exclusions, then the participants retained in the published fits.
if (4 %in% run) {
  source(here("src", "utils.R")); source(here("src", "exploratory_graph_analysis.R"))
  net_degree <- calculate_net_stats(g)  # read by organize_group_data()
  fig4 <- map_dfr(1:3, function(e) {
    kept <- unique(readRDS(here("fits", sprintf("pca2_exp_%d_fit_choice03.rds", e)))$data$subject_id)
    d <- organize_group_data(experiment = e) %>%
      group_by(subject_id) %>%
      filter(rt > quantile(rt, .25) - 2 * IQR(rt), rt < quantile(rt, .75) + 2 * IQR(rt)) %>%
      ungroup() %>%
      filter(rt > 250, rt < 9000, subject_id %in% kept) %>%
      group_by(subject_id) %>%
      mutate(zl = as.numeric(scale(left_rating)), zr = as.numeric(scale(right_rating)),
             zn = as.numeric(scale(left_net_pca2)), zrn = as.numeric(scale(right_net_pca2))) %>%
      ungroup() %>%
      filter(complete.cases(zl, zr, zn, zrn)) %>%
      mutate(tercile = cut(zn, quantile(zn, c(0, 1/3, 2/3, 1)), include.lowest = TRUE,
                           labels = c("Low Centrality", "Medium Centrality", "High Centrality")))
    m <- glm(choice ~ zl + zr + zn + zrn, data = d, family = binomial)
    nd <- d %>% group_by(tercile) %>% summarise(zn = mean(zn), n = n(), .groups = "drop") %>% mutate(zl = 0, zr = 0, zrn = 0)
    pr <- predict(m, nd, type = "link", se.fit = TRUE)
    tibble(study = paste("Set-Choice Study", e), tercile = nd$tercile, n = nd$n,
           p = plogis(pr$fit), lo = plogis(pr$fit - pr$se.fit), hi = plogis(pr$fit + pr$se.fit))
  })
  p4 <- ggplot(fig4, aes(x = tercile, y = p, color = study)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.6), width = 0.2, linewidth = 0.8) +
    geom_point(position = position_dodge(width = 0.6), size = 3.5) +
    scale_color_manual(values = unname(study_colors[1:3]), name = "Study") +
    scale_y_continuous(limits = c(0.4, 0.6), breaks = seq(0.4, 0.6, 0.05)) +
    labs(x = "Left Set Centrality (PC2 terciles)", y = "P(Choose Left) at equal set values") +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom")
  ggsave(here("output", "figure4_marginal_effects.pdf"), p4, width = 8, height = 6)
  ggsave(here("output", "figure4_marginal_effects.png"), p4, width = 8, height = 6, dpi = 200)
  write_csv(fig4, here("results", "figure4_marginal_effects_values.csv"))
  print(as.data.frame(fig4), digits = 3)
}

choice_terms <- c("Left %s", "Right %s", "Left rating × %s", "Right rating × %s")

# Figure 5: set choice
if (5 %in% run) build_forest(
  panels = list(similarity = list(fits = paste0("strength_exp_", 1:3, "_fit_choice03"), label = "similarity"),
                centrality = list(fits = paste0("pca2_exp_", 1:3, "_fit_choice03"), label = "centrality")),
  terms = setNames(choice_terms, c("b_zleft_net1", "b_zright_net1", "b_zleft_rating:zleft_net1", "b_zright_rating:zright_net1")),
  xlim = c(-0.45, 0.5), breaks = seq(-0.2, 0.4, 0.2), file = "figure5_forest_choice")

# Figure 6: set-choice RT (absolute network difference term; value difference and overall value are in the models but not shown)
if (6 %in% run) build_forest(
  panels = list(similarity = list(fits = paste0("strength_exp_", 1:3, "_fit_rt02"), label = "similarity"),
                centrality = list(fits = paste0("pca2_exp_", 1:3, "_fit_rt02"), label = "centrality")),
  terms = c("b_nd1" = "Absolute %s difference"),
  xlim = c(-0.07, 0.07), breaks = seq(-0.05, 0.05, 0.05), file = "figure6_forest_rt", height = 4)

# Figure 7: single-choice Studies 1-3 (Lee & Hare 2023, Lee & Holyoak 2021, Smith & Krajbich 2018).
# These fits hold PC1 as net1/nd1 and PC2 as net2/nd2; only PC2 is shown. Same axes as Figures 5 and 6.
sc <- c("Study 1 (Lee & Hare)" = "PCA_Lee_Hare_2023_choice_data_exp2", "Study 2 (Lee & Holyoak)" = "PCA_Lee_Holyoak_2021_choice_data_exp2_5",
        "Study 3 (Smith & Krajbich)" = "PCA_Smith_Krajbich_2018_choice_data")
sc_colors <- setNames(c("#66C2A5", "#8DA0CB", "#A6D854", "#FF8C00"), c(names(sc), "Average"))
if (7 %in% run) build_forest(
  panels = list(
    choice = list(fits = setNames(paste0(sc, c("_fit_choice03", "_fit_choice03", "_fit_choice")), names(sc)), label = "centrality",
                  terms = setNames(choice_terms, c("b_zleft_net2", "b_zright_net2", "b_zleft_rating:zleft_net2", "b_zright_rating:zright_net2")),
                  xlim = c(-0.45, 0.5), breaks = seq(-0.2, 0.4, 0.2)),
    rt = list(fits = setNames(paste0(sc, c("_fit_rt02", "_fit_rt02", "_fit_rt")), names(sc)), label = "centrality",
              terms = c("b_nd2" = "Absolute %s difference"), xlim = c(-0.07, 0.07), breaks = seq(-0.05, 0.05, 0.05))),
  colors = sc_colors, legend = "Dataset", file = "figure7_forest_single_choice",
  compose = function(a, b) a + b + plot_layout(design = "A#\nAB\nA#", heights = c(2.3, 2.4, 2.3), guides = "collect"))

# Figure 8: multi-alternative Studies 1-4, item-level fits (chosen ~ value + PC1 + PC2 + interactions; PC1 not shown).
# Each set size of Studies 3 and 4 is its own dataset, as in the item-level analysis. Average = meta over the nine datasets.
ma <- c("Leng et al." = "leng", "Fernandez et al. Exp. 1" = "fernandez_exp1",
        "Fernandez et al. Exp. 2 (Set 4)" = "fernandez_exp2_ss4", "Fernandez et al. Exp. 2 (Set 8)" = "fernandez_exp2_ss8",
        "Fernandez et al. Exp. 2 (Set 12)" = "fernandez_exp2_ss12", "Thomas et al. (Set 9)" = "thomas_ss9",
        "Thomas et al. (Set 16)" = "thomas_ss16", "Thomas et al. (Set 25)" = "thomas_ss25", "Thomas et al. (Set 36)" = "thomas_ss36")
ma_colors <- setNames(c("#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85", "#FF8C00"),
                      c(names(ma), "Average"))
if (8 %in% run) build_forest(
  panels = list(
    choice = list(fits = setNames(paste0("item_level_", ma, "_accuracy"), names(ma)), label = "centrality",
                  terms = c("b_PC2_z" = "Item %s", "b_item_value_z:PC2_z" = "Value × %s"),
                  xlim = c(-0.5, 0.95), breaks = seq(-0.4, 0.8, 0.4)),
    rt = list(fits = setNames(paste0("item_level_", ma, "_rt"), names(ma)), label = "centrality",
              terms = c("b_chosen_PC2_z" = "Item %s", "b_chosen_value_z:chosen_PC2_z" = "Value × %s"),
              xlim = c(-0.07, 0.1), breaks = seq(-0.05, 0.05, 0.05))),
  colors = ma_colors, legend = "Dataset", file = "figure8_forest_multi_alternative", width = 15, height = 9)
