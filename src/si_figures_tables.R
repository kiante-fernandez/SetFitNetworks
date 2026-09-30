# si_figures_tables.R - Supplementary figures and tables. Usage: Rscript src/si_figures_tables.R [items], e.g. fig3 table6
# (default: all). Figures go to output/, tables to results/. Table S7 reads the item-level fits (about 9 GB RAM).
# Tables S4-S5 come from set_variance_regression.R, S8-S9 from choice_value_parametrization.R and
# rt_individual_values_regression.R, the cross-network table (SI section 13) from centrality_validation_analysis.R.
suppressMessages({library(tidyverse); library(here); library(patchwork); library(bayestestR)})
run <- if (length(commandArgs(TRUE))) commandArgs(TRUE) else c(paste0("fig", 1:6), paste0("table", c(1:3, 10, 6, 7)))
source(here("src", "utils.R"))
source(here("src", "exploratory_graph_analysis.R"))  # Rating Study 1 network: ega_res, g
save_fig <- function(p, name, width, height) {
  ggsave(here("output", paste0(name, ".pdf")), p, width = width, height = height, device = cairo_pdf)
  ggsave(here("output", paste0(name, ".png")), p, width = width, height = height, dpi = 200)
}
bold_theme <- theme(axis.text = element_text(face = "bold"), axis.title = element_text(face = "bold"))
rs1_colors <- c("meat/cheese" = "#1B9E77", "fruits/vegetables" = "#D95F02", "sweet pastries" = "#7570B3", "chocolate" = "#E7298A",
                "chips" = "#66A61E", "crackers" = "#E6AB02", "bread" = "#A6761D")
rs1_markers <- c("meat/cheese" = "deli turkey", "fruits/vegetables" = "orange", "sweet pastries" = "churro",
                 "chocolate" = "dark chocolate", "chips" = "potato chips", "crackers" = "ritz cracker", "bread" = "baguette")
A <- ega_res$typicalGraph$graph

# Supp. Figs. 1-2: consensus clustering (Lancichinetti & Fortunato, 2012) of the Rating Study 1 network ------------------
if (any(c("fig1", "fig2") %in% run)) {
  set.seed(2025)
  consensus <- setNames(EGAnet::community.consensus(A, consensus.method = "iterative", consensus.iter = 10000), colnames(A))
  cons_markers <- c(head(rs1_markers, 5), "crackers/breads" = "ritz cracker")
  cons_colors <- setNames(unname(rs1_colors[1:6]), names(cons_markers))
  labels <- community_labels(consensus, cons_markers)
  pdf(here("output", "supp_fig1_consensus_network.pdf"), width = 9, height = 6, bg = "white")
  layout(matrix(1:2, 1), widths = c(3, 1.3)); par(mar = c(0, 0, 0, 0))
  plot_network(A, labels, cons_colors)
  plot.new(); legend("left", names(cons_colors), pch = 21, pt.bg = cons_colors, pt.cex = 2.5, cex = 1.2, bty = "n")
  invisible(dev.off())
  p <- community_table(colnames(A), labels, cons_colors)
  ggsave(here("output", "supp_fig2_consensus_communities.pdf"), p, width = 10, height = 0.3 * max(p$data$row) + 1)
}

# Supp. Figs. 3-4: PCA of the Rating Study 1 centrality measures ---------------------------------------------------------
net_degree <- calculate_net_stats(g)
measures <- c("degree", "strength", "eigen", "weighted_transitivity", "closeness", "betweenness")
pca_res <- prcomp(net_degree[, measures], center = TRUE, scale. = TRUE)
if ("fig3" %in% run) {
  scree <- factoextra::fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 70)) + theme_classic() + labs(x = "PC") + bold_theme
  vars <- factoextra::fviz_pca_var(pca_res, col.var = "black") + theme_classic() + labs(x = "PC1", y = "PC2", title = "") + bold_theme
  items <- tibble(item = net_degree$Name, PC1 = pca_res$x[, 1], PC2 = pca_res$x[, 2],
                  community = factor(community_labels(setNames(net_degree$snack_type, net_degree$Name), rs1_markers), names(rs1_colors)))
  nodes <- ggplot(items, aes(PC1, PC2, color = community, label = item)) +
    geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) + geom_point() +
    ggrepel::geom_text_repel(size = 3.5, max.overlaps = Inf) + scale_color_manual(values = rs1_colors, guide = "none") +
    theme_classic() + bold_theme
  save_fig((scree + vars) / nodes + plot_annotation(tag_levels = "A"), "supp_fig3_pca", 9, 11)
}
if ("fig4" %in% run) {
  p <- net_degree %>% select(all_of(measures), PCA1:PCA6) %>% cor() %>% ggcorrplot::ggcorrplot(type = "upper", lab = TRUE) +
    theme_classic() + labs(x = "", y = "") + bold_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_fig(p, "supp_fig4_pc_correlations", 10, 8)
}

# Supp. Fig. 5: best-fitting set strategy per participant (participants whose strategy models were fit; results/strategy_exp*.csv
# from exp_*_network_difference_regression.R) ---------------------------------------------------------------------------------
if ("fig5" %in% run) {
  strategy_labels <- c(temp_res0 = "Higher Average", temp_res1 = "Maximum Value", temp_res2 = "Excluding Minimum", temp_res3 = "Range")
  panels <- map(1:3, function(e) {
    d <- read_csv(here("results", sprintf("strategy_exp%d.csv", e)), show_col_types = FALSE) %>% filter(strategy != "0") %>%
      count(strategy = factor(strategy_labels[strategy], strategy_labels), .drop = FALSE) %>%
      mutate(Proportion = n / sum(n), strategy = fct_reorder(strategy, Proportion))
    ggplot(d, aes(strategy, Proportion, fill = strategy)) + geom_col() + geom_text(aes(label = n), position = position_stack(vjust = .5)) +
      coord_flip() + scale_fill_brewer(palette = "Dark2", guide = "none") +
      labs(x = "Strategy", title = "Best Fitting Set Strategy Identified Per Subject") + theme_classic() + bold_theme
  })
  save_fig(wrap_plots(panels, ncol = 2) + plot_annotation(tag_levels = "a"), "supp_fig5_strategies", 14, 9)
}

# Supp. Fig. 6: choice and RT as a function of set values (trials after exclusions, data/ISDN_poster_exp*.csv) --------------
if ("fig6" %in% run) {
  trials <- map_dfr(1:3, ~ read_csv(here("data", sprintf("ISDN_poster_exp%d.csv", .x)), show_col_types = FALSE) %>% mutate(study = factor(.x)))
  bin <- function(x, width) width * round(x / width)
  cols <- c("1" = "#4DAF4A", "2" = "#E41A1C", "3" = "#377EB8")
  pa <- trials %>% mutate(x = bin(left_rating - right_rating, 80)) %>% filter(abs(x) < 350) %>%
    group_by(study, x) %>% summarise(m = mean(choice), se = sd(choice) / sqrt(n()), .groups = "drop") %>%
    ggplot(aes(x, m, color = study)) + geom_hline(yintercept = .5, linewidth = .25) + geom_vline(xintercept = 0, linewidth = .25) +
    geom_pointrange(aes(ymin = m - se, ymax = m + se)) + geom_line() + scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Left Set-Liking – Right Set-Liking", y = "P(Left Chosen)", color = "Experiment")
  pb <- trials %>% mutate(x = bin(abs(left_rating - right_rating), 60), rt = rt / 1000) %>% filter(x < 350) %>%
    group_by(study, x) %>% summarise(m = mean(rt), se = sd(rt) / sqrt(n()), n = n(), .groups = "drop") %>%
    ggplot(aes(x, m, color = study)) + geom_pointrange(aes(ymin = m - se, ymax = m + se)) +
    geom_smooth(aes(weight = n), method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", linewidth = .8) +  # trial-weighted
    guides(color = "none") +
    labs(x = "|Left Set-Liking – Right Set-Liking|", y = "Response time (s)", color = "Experiment")
  p <- (pa + pb) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") &
    scale_color_manual(values = cols) & theme_classic(base_size = 14) & bold_theme
  save_fig(p, "supp_fig6_manipulation_check", 12, 5)
}

# Supp. Tables 1-3: item stability across the bootstrap samples (Rating Studies 1-3) ---------------------------------------
# EGAnet's itemStability() computation without its plot, which fails for the Rating Study 1 object saved by EGAnet 1.0.
# EGAnet 2.x aligns bootstrap communities slightly differently, so a few Rating Study 1 values differ from the SI by up to ~.04.
stability <- function(file, obj, out) {
  e <- new.env(); load(here("data", file), envir = e); b <- e[[obj]]
  if (is.list(b$boot.wc)) b$boot.wc <- `colnames<-`(do.call(rbind, b$boot.wc), colnames(b$typicalGraph$graph))  # EGAnet 1.x layout
  s <- EGAnet:::itemStability_core(EGAnet:::get_EGA_object(b), NULL, b$boot.wc, b$iter)$item.stability$all.dimensions
  write_csv(as_tibble(round(s, 3), rownames = "item"), here("results", out))
}
if ("table1" %in% run) stability("rating_network_graph.RData", "ega_res", "supp_table1_item_stability_rating_study1.csv")
if ("table2" %in% run) stability("shenhav_rating_network_graphV2.RData", "ega_res2", "supp_table2_item_stability_rating_study2.csv")
if ("table3" %in% run) stability("rangel_rating_network_graph.RData", "boot_ega_results", "supp_table3_item_stability_rating_study3.csv")

# Supp. Tables 10-15: each centrality measure entered in place of PC2 (Set-Choice Studies 1-3; 10-12 choice, 13-15 RT).
# Cells: posterior median [95% HDI]; * marks pd > .97 (bold in the SI) --------------------------------------------------------
if ("table10" %in% run) {
  term_labels <- c(b_zleft_rating = "left liking rating", b_zright_rating = "right liking rating", b_zleft_net1 = "left network estimate",
                   b_zright_net1 = "right network estimate", "b_zleft_rating:zleft_net1" = "left rating × network estimate",
                   "b_zright_rating:zright_net1" = "right rating × network estimate", b_vd = "Liking Value Difference",
                   b_ov = "Overall Value", b_nd1 = "Network Score Difference", b_sd = "Similarity Difference")
  measure_labels <- c(strength = "strength", betweenness = "betweenness", closeness = "closeness",
                      weighted_transitivity = "weighted transitivity", eigen = "eigenvector", edge_density = "edge density",
                      modularity = "modularity")
  for (e in 1:3) for (type in c("choice03", "rt02")) {
    tab <- imap_dfr(measure_labels, function(lab, m) {
      d <- describe_posterior(readRDS(here("fits", sprintf("%s_exp_%d_fit_%s.rds", m, e, type))), ci_method = "hdi",
                              centrality = "median", test = "p_direction")
      tibble(measure = lab, term = term_labels[d$Parameter],
             cell = sprintf("%.3f [%.3f, %.3f]%s", d$Median, d$CI_low, d$CI_high, ifelse(d$pd > .97, "*", "")))
    }) %>% filter(!is.na(term)) %>% pivot_wider(names_from = measure, values_from = cell)
    write_csv(tab, here("results", sprintf("supp_table%d_centrality_measures.csv", e + if (type == "choice03") 9 else 12)))
  }
}

# Supp. Tables 6-7: sensitivity. MDE = (z_.975 + z_.80) x SE, with SE the posterior SD of the PC2 coefficient; datasets pooled
# with fixed-effect and DerSimonian-Laird random-effects meta-analyses -----------------------------------------------------------
mde_factor <- qnorm(.975) + qnorm(.80)
pool <- function(b, se) {
  w <- 1 / se^2; fe <- sum(w * b) / sum(w); q <- sum(w * (b - fe)^2); df <- length(b) - 1
  tau2 <- max(0, (q - df) / (sum(w) - sum(w^2) / sum(w))); wr <- 1 / (se^2 + tau2)
  tibble(model = c("fixed-effects", "random-effects"), beta = c(fe, sum(wr * b) / sum(wr)), se = sqrt(c(1 / sum(w), 1 / sum(wr))),
         Q = q, df = df, p_Q = pchisq(q, df, lower.tail = FALSE), I2 = max(0, (q - df) / q))
}
sensitivity <- function(d, n_pooled = sum(d$n)) {  # n_pooled = NA when datasets share participants
  bind_rows(d, pool(d$beta, d$se) %>% mutate(dataset = paste0("Meta-analysis (", model, ")"), n = n_pooled) %>% select(-model)) %>%
    mutate(lower = beta - qnorm(.975) * se, upper = beta + qnorm(.975) * se, mde = mde_factor * se)
}
if ("table6" %in% run) {
  single <- c("Lee & Hare (2023)" = "PCA_Lee_Hare_2023_choice_data_exp2_fit_choice03",
              "Lee & Holyoak (2021)" = "PCA_Lee_Holyoak_2021_choice_data_exp2_5_fit_choice03",
              "Smith & Krajbich (2018)" = "PCA_Smith_Krajbich_2018_choice_data_fit_choice")
  d <- imap_dfr(single, function(f, lab) {
    m <- readRDS(here("fits", paste0(f, ".rds"))); b <- brms::as_draws_df(m, variable = "b_zleft_net2")$b_zleft_net2
    tibble(dataset = lab, n = n_distinct(m$data[[ncol(m$data)]]), beta = mean(b), se = sd(b))
  })
  z12 <- (d$beta[1] - d$beta[2]) / sqrt(d$se[1]^2 + d$se[2]^2)
  write_csv(sensitivity(d) %>% mutate(z_study1_vs_2 = z12, p_study1_vs_2 = 2 * pnorm(-abs(z12))),
            here("results", "supp_table6_single_choice_sensitivity.csv"))
}
if ("table7" %in% run) {
  multi <- c("Leng et al. (2025)" = "leng", "Fernandez Exp 1" = "fernandez_exp1", "Fernandez Exp 2, set size 4" = "fernandez_exp2_ss4",
             "Fernandez Exp 2, set size 8" = "fernandez_exp2_ss8", "Fernandez Exp 2, set size 12" = "fernandez_exp2_ss12",
             "Thomas et al., set size 9" = "thomas_ss9", "Thomas et al., set size 16" = "thomas_ss16",
             "Thomas et al., set size 25" = "thomas_ss25", "Thomas et al., set size 36" = "thomas_ss36")
  d <- imap_dfr(multi, function(f, lab) {
    m <- readRDS(here("fits", sprintf("item_level_%s_accuracy.rds", f)))
    n <- tibble(n = n_distinct(m$data$subject_id), n_trials = n_distinct(m$data$trial_id)); rm(m); gc()
    read_csv(here("results", sprintf("item_level_%s_accuracy_results.csv", f)), show_col_types = FALSE) %>%
      filter(effect %in% c("PC2", "value_x_PC2")) %>% transmute(dataset = lab, effect, beta = estimate, se) %>% bind_cols(n)
  })
  write_csv(map_dfr(c("PC2", "value_x_PC2"), ~ sensitivity(filter(d, effect == .x), NA) %>% mutate(effect = .x)),
            here("results", "supp_table7_multi_alternative_sensitivity.csv"))
}
