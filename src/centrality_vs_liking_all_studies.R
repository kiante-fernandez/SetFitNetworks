# centrality_vs_liking_all_studies.R - Is item centrality the same thing as being liked?
# For every dataset in the paper, correlates each item's mean liking rating (from that dataset's
# own raters) with the item's centrality (PC1/PC2, canonical loadings applied to the network used
# for that dataset). Also asks whether the most-liked items are the most central.
# Outputs: results/centrality_vs_liking_all_studies.csv, results/centrality_vs_liking_items.csv,
#          output/centrality_vs_liking_all_studies.{pdf,png}, output/centrality_vs_liking_all_studies_pc1.{pdf,png}
suppressMessages({library(tidyverse); library(igraph); library(patchwork); library(here)})
source(here("src", "utils.R"))
L <- readRDS(here("output", "canonical_pca_loadings.rds"))

six <- function(g) { G <- g; E(G)$weight <- 2**((E(G)$weight - min(E(G)$weight)) / diff(range(E(G)$weight)))
  data.frame(Name = V(g)$name, degree = degree(g, normalized = TRUE), strength = strength(g), eigen = eigen_centrality(G)$vector,
             transitivity = transitivity(g, type = "weighted"), closeness = closeness(G, normalized = TRUE, cutoff = -1),
             betweenness = betweenness(G, normalized = TRUE)) }
pcs <- function(df) apply_pca_weights(df, L) %>% select(Name, PC1, PC2)

# --- Networks -------------------------------------------------------------------
FoodNames <- readxl::read_excel(here("data", "snackitemnames_nicholas", "item_image_numbers_exp2_5_nicholas.xlsx"))
load(here("data", "rating_network_graph.RData")); lee_g <- upgrade_graph(g); V(lee_g)$name <- FoodNames$Name
lee_net <- pcs(six(lee_g)) %>% left_join(FoodNames %>% select(Name, Image), by = "Name")

load(here("data", "rangel_rating_network_graph.RData")); rangel_net <- pcs(six(upgrade_graph(g)))

e <- new.env(); load(here::here("data", "leng_2024_networkmetrics.RData"), envir = e)
leng_net <- pcs(e$net_degree %>% rename(transitivity = weighted_transitivity)) %>% mutate(key = tolower(gsub("_", " ", Name)))

thomas_net <- read_csv(here("data", "thomas2021_network_stats.csv"), show_col_types = FALSE) %>%
  rename(transitivity = weighted_transitivity) %>% select(-starts_with("PCA")) %>% pcs()
thomas_map <- read_csv(here("data", "thomas2021_stimulus_mapping.csv"), show_col_types = FALSE) %>%
  mutate(item_name = recode(item_name, goldfishpretzel = "goldfish", milanomintchocolate = "milano", pringles = "pringlesred", pringlesbbq = "pringlesred")) %>%
  left_join(thomas_net, by = c("item_name" = "Name"))

# --- Item mean liking per dataset ------------------------------------------------
matrix_means <- function(r) tibble(Name = names(r), mean_rating = colMeans(r, na.rm = TRUE), n_raters = nrow(r))
pair_means <- function(d, subj, li, ri, lv, rv) bind_rows(tibble(s = d[[subj]], item = d[[li]], v = d[[lv]]), tibble(s = d[[subj]], item = d[[ri]], v = d[[rv]])) %>%
  distinct(s, item, .keep_all = TRUE) %>% group_by(item) %>% summarise(mean_rating = mean(v, na.rm = TRUE), .groups = "drop") %>% mutate(n_raters = n_distinct(d[[subj]]))  # n = participants in the dataset
lee1 <- read_csv(here("data", "lee_2021_rating1.csv"), col_names = FoodNames$Name, show_col_types = FALSE)

ds <- list()
ds[["Rating Study 1 / Single-Choice 2\n(Lee & Holyoak 2021)"]] <- matrix_means(lee1) %>% left_join(lee_net, by = "Name")
for (i in 1:3) ds[[sprintf("Set-Choice %d", i)]] <- matrix_means(read_csv(here("data", c("fernandez_2022_rating_exp1.csv", "fernandez_2022_rating_exp2.csv", "fernandez_2023_rating_exp3.csv")[i]), show_col_types = FALSE)) %>% left_join(lee_net, by = "Name")
lh <- read_csv(here("data", "Lee_Hare_2023_OSF", "Lee_Hare_2023_choice_data_exp2.csv"), show_col_types = FALSE)
ds[["Single-Choice 1\n(Lee & Hare 2023)"]] <- pair_means(lh, "subject_id", "item_number_left", "item_number_right", "item_value_left", "item_value_right") %>% inner_join(lee_net, by = c("item" = "Image"))
# Lee & Holyoak 2021 choice values are the Rating Study 1 ratings; verify and fold into that panel
lk <- read_csv(here("data", "lee_2021_exp2_5_v2.csv"), show_col_types = FALSE)
lk_means <- pair_means(lk, "subject_id", "item_number_left", "item_number_right", "item_value_left", "item_value_right") %>% inner_join(lee_net, by = c("item" = "Image"))
chk <- inner_join(lk_means, ds[[1]], by = "Name"); cat(sprintf("Lee&Holyoak choice item means vs Rating Study 1: r = %.3f (n = %d)\n", cor(chk$mean_rating.x, chk$mean_rating.y), nrow(chk)))
sk <- new.env(); load(here::here("data", "smith_krajbich_2018", "ACADchoiceandeyedata.RData"), envir = sk)
imgs <- tibble(Picture = seq_along(readr::read_csv(here::here("data", "smith_krajbich_2018", "food_stimuli.csv"), show_col_types = FALSE)$filename), Name = tolower(str_remove(str_remove(readr::read_csv(here::here("data", "smith_krajbich_2018", "food_stimuli.csv"), show_col_types = FALSE)$filename, "\\.[^.]+$"), "img_")))
skd <- sk$twofoodchoicedata %>% mutate(ln = imgs$Name[FoodLeft], rn = imgs$Name[FoodRight])
sk_means <- pair_means(skd, "SubjectNumber", "ln", "rn", "ValueLeft", "ValueRight")
cat(sprintf("Smith & Krajbich: %d rated items, %d matched to Rangel network\n", nrow(sk_means), sum(sk_means$item %in% rangel_net$Name)))
ds[["Single-Choice 3\n(Smith & Krajbich 2018)"]] <- sk_means %>% inner_join(rangel_net, by = c("item" = "Name")) %>% mutate(Name = item)
s2 <- read_csv(here::here("data", "leng_2025", "Study3a_2.csv"), show_col_types = FALSE)
pic <- read_csv(here("data", "shenhav_item_list.csv"), show_col_types = FALSE); names(pic) <- c("pic_name", "pic_path")
leng_means <- s2 %>% distinct(participant, pic_path, .keep_all = TRUE) %>% group_by(pic_path) %>% summarise(mean_rating = mean(value, na.rm = TRUE), .groups = "drop") %>% mutate(n_raters = n_distinct(s2$participant)) %>%
  left_join(pic, by = "pic_path") %>% mutate(key = tolower(pic_name)) %>% inner_join(leng_net, by = "key")
cat(sprintf("Leng: %d rated pictures, %d matched to Leng network\n", n_distinct(s2$pic_path), nrow(leng_means)))
ds[["Single/Multi-Alternative\n(Leng et al. 2025)"]] <- leng_means
ck <- read_csv(here::here("data", "choose_k", "choosek_R.csv"), show_col_types = FALSE)
ck_long <- map_dfr(0:3, ~tibble(s = ck$subject_id, item = ck[[paste0("item_name_", .x)]], v = ck[[paste0("item_value_", .x)]]))
ck1 <- ck_long %>% distinct(s, item, .keep_all = TRUE) %>% group_by(item) %>% summarise(mean_rating = mean(v), .groups = "drop") %>% mutate(n_raters = n_distinct(ck_long$s)) %>% inner_join(lee_net, by = c("item" = "Image"))
e2 <- read_csv(here::here("data", "choose_k", "exp_2_processed_V2.csv"), show_col_types = FALSE)
e2_long <- map_dfr(1:12, ~tibble(s = e2$subject_id, item = e2[[paste0("item_name_", .x)]], v = e2[[paste0("item_value_", .x)]])) %>% filter(!is.na(item), !is.na(v))
ck2 <- e2_long %>% distinct(s, item, .keep_all = TRUE) %>% group_by(item) %>% summarise(mean_rating = mean(v), .groups = "drop") %>% mutate(n_raters = n_distinct(e2_long$s)) %>% inner_join(lee_net, by = c("item" = "Image"))
chk2 <- inner_join(ck1, ck2, by = "Name"); cat(sprintf("Fernandez choose-k Exp 1 vs Exp 2 item means: r = %.3f\n", cor(chk2$mean_rating.x, chk2$mean_rating.y)))
ds[["Multi-Alternative 2\n(Fernandez choose-k Exp 1)"]] <- ck1
ds[["Multi-Alternative 3\n(Fernandez choose-k Exp 2)"]] <- ck2
th <- map_dfr(c(9, 16, 25, 36), function(ss) { d <- read_csv(here::here("data", "thomas2021", "summary_files", sprintf("setsize-%d_desc-data.csv", ss)), show_col_types = FALSE)
  map_dfr(0:(ss - 1), ~tibble(s = d$subject, item = d[[paste0("stimulus_", .x)]], v = d[[paste0("item_value_", .x)]])) })
th_means <- th %>% filter(!is.na(item), !is.na(v)) %>% distinct(s, item, .keep_all = TRUE) %>% group_by(item) %>% summarise(mean_rating = mean(v), .groups = "drop") %>% mutate(n_raters = n_distinct(th$s)) %>%
  inner_join(thomas_map, by = c("item" = "stimulus")) %>% filter(!is.na(PC2)) %>% mutate(Name = item_name)
cat(sprintf("Thomas: %d rated stimuli, %d matched to network\n", n_distinct(th$item), nrow(th_means)))
ds[["Multi-Alternative 4\n(Thomas et al. 2021)"]] <- th_means

items <- imap_dfr(ds, ~select(.x, Name, mean_rating, n_raters, PC1, PC2) %>% mutate(dataset = .y, .before = 1)) %>% filter(!is.na(PC2))
write_csv(items, here("results", "centrality_vs_liking_items.csv"))

# --- Correlations and "is the most liked item the most central?" -----------------
res <- items %>% group_by(dataset) %>% group_modify(function(d, k) {
  t2 <- cor.test(d$mean_rating, d$PC2); t1 <- cor.test(d$mean_rating, d$PC1); n <- nrow(d)
  top <- d %>% slice_max(mean_rating, n = 1, with_ties = FALSE); cen <- d %>% slice_max(PC2, n = 1, with_ties = FALSE)
  tibble(n_items = n, n_raters = d$n_raters[1],
         r_PC2 = t2$estimate, ci_lo_PC2 = t2$conf.int[1], ci_hi_PC2 = t2$conf.int[2], p_PC2 = t2$p.value,
         r_PC1 = t1$estimate, ci_lo_PC1 = t1$conf.int[1], ci_hi_PC1 = t1$conf.int[2], p_PC1 = t1$p.value,
         rho_PC2 = cor(d$mean_rating, d$PC2, method = "spearman"),
         most_liked = top$Name, most_liked_PC2_rank = rank(-d$PC2)[d$Name == top$Name],
         most_central = cen$Name, most_central_liking_rank = rank(-d$mean_rating)[d$Name == cen$Name],
         top5_liked_mean_PC2_pctile = mean(percent_rank(d$PC2)[order(-d$mean_rating)[1:5]])) }) %>% ungroup()
write_csv(res, here("results", "centrality_vs_liking_all_studies.csv"))
print(as.data.frame(res %>% mutate(dataset = gsub("\n", " ", dataset), across(where(is.numeric), ~round(., 2)))), row.names = FALSE)

# --- Figure (same style as the attribute scatters in centrality_validation_analysis.R) -----
panel <- function(d, lab, pc) { t <- cor.test(d$mean_rating, d[[pc]])
  ggplot(d, aes(x = mean_rating, y = .data[[pc]])) +
    geom_point(size = 2.5, alpha = 0.7, color = "#377EB8") +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 0.8, formula = y ~ x) +
    labs(title = sprintf("%s (n = %d)", lab, d$n_raters[1]),
         subtitle = sprintf("r = %.2f [%.2f, %.2f], p %s", t$estimate, t$conf.int[1], t$conf.int[2], ifelse(t$p.value < 0.001, "< .001", sprintf("= %.3f", t$p.value))),
         x = "Mean liking rating", y = sprintf("Centrality (%s)", pc)) +
    theme_classic() + theme(plot.title = element_text(size = 9, face = "bold"), plot.subtitle = element_text(size = 8)) }
for (pc in c("PC2", "PC1")) {
  fig <- wrap_plots(imap(ds, ~panel(filter(.x, !is.na(PC2)), .y, pc)), ncol = 4) +
    plot_annotation(title = sprintf("Item centrality (%s) versus mean liking in every dataset", pc),
                    subtitle = "Each point is an item; liking is the mean rating from that dataset's own participants; centrality comes from the network used for that dataset",
                    theme = theme(plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(size = 9)))
  f <- here("output", paste0("centrality_vs_liking_all_studies", ifelse(pc == "PC1", "_pc1", "")))
  ggsave(paste0(f, ".pdf"), fig, width = 15, height = 11); ggsave(paste0(f, ".png"), fig, width = 15, height = 11, dpi = 200) }
cat("Saved figures to output/centrality_vs_liking_all_studies*.{pdf,png}\n")
