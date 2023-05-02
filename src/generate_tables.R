# generate_tables.R - creates the tables from the models outputs

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
# 2023/02/15      Kianté  Fernandez                     coded up version one

# Libraries
library(modelsummary)
library(purrr)
#load datasets 

## choice

exp_1_fit_choice01 <- readRDS("~/Documents/SetFitNetworks/fits/exp_1_fit_choice01.rds")
exp_1_fit_choice02 <- readRDS("~/Documents/SetFitNetworks/fits/exp_1_fit_choice02.rds")
exp_1_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/exp_1_fit_choice03.rds")

exp_2_fit_choice01 <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_choice01.rds")
exp_2_fit_choice02A <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_choice02A.rds")
exp_2_fit_choice02B <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_choice02B.rds")
exp_2_fit_choice03 <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_choice03.rds")
exp_2_fit_choice04 <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_choice04.rds")

ms1 <- list(pca1_exp_1_fit_choice03)
ms2 <- list(pca1_exp_2_fit_choice03)

# map(ms1, bayestestR::sexit)
map(list(exp_1_fit_choice03, exp_2_fit_choice04), bayestestR::sexit)

cm <- c('b_zleft_rating'    = 'left liking rating',
        'b_zright_rating'    = 'right liking rating',
        'b_zleft_net' = 'left network estimate',
        'b_zright_net' = 'right network estimate',
        'b_zleft_sim' = 'left similarity judgment',
        'b_zright_sim' = 'right similarity judgment',
        'b_zleft_rating:zleft_net' = 'left rating × network estimate',
        'b_zright_rating:zright_net' = 'right rating × network estimate'
)


cm <- c('zleft_rating'    = 'left liking rating',
        'zright_rating'    = 'right liking rating',
        'zleft_net' = 'left network estimate',
        'zright_net' = 'right network estimate',
        'zleft_sim' = 'left similarity judgment',
        'zright_sim' = 'right similarity judgment',
        'zleft_rating:zleft_net' = 'left rating × network estimate',
        'zright_rating:zright_net' = 'right rating × network estimate'
)
rm <- c('vd'    = 'abs value difference',
        'nd'    = 'abs network difference',
        'ov' = 'overall value'
)
#exp two
names(res_netstats) <- c("strength","betweenness","closeness","transitivity","eigen", "edge density", "modularity","PC1", "PC2")
# names(res_netstats) <- c("edge_density", "modularity", "PC1", "PC2")
names(res_netstats2) <- c("strength","betweenness","closeness","transitivity","eigen", "edge density", "modularity","PC1", "PC2")

modelsummary(res_netstats,
             # shape = term ~ model + statistic,
             fmt = 2,
             estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
             statistic = NULL,
             coef_omit = "Intercept|.*subject_id",
             coef_map = cm,
             gof_map = NA)

# net_stat = "edge_density"
# file_name <- here::here("tables", paste0("choice", "_", net_stat, ".html"))

panels <- list("Experiment one:" = ms1,
               "Experiment two:" = ms2)
modelsummary::modelsummary(panels, 
             shape = "rbind",
             fmt = 2,
             centrality = "median", 
             statistic = "[{conf.low} {conf.high}]",
             coef_omit = "Intercept|.*subject_id",
             gof_map = NA,
             # coef_map = cm,
             mc.cores = 10,
             # output = file_name
             )

# library(ggplot2)
# 
# b <- list(geom_vline(xintercept = 0, color = 'orange'),
#           annotate("rect", alpha = .1,
#                    xmin = -.5, xmax = .5, 
#                    ymin = -Inf, ymax = Inf),
#           geom_point(aes(y = term, x = estimate), alpha = .3, 
#                      size = 10, color = 'red'))
# #to use model summary we need some say to ignore std.effor
# modelplot(ms1,
#           estimate <- "{estimate}|{conf.low}|{conf.high}",
#           conf_level =.95,
#           coef_omit = "Intercept|.*subject_id",
#           background = b,
#           coef_map = cm)

#response times
exp_1_fit_rt01 <- readRDS("~/Documents/SetFitNetworks/fits/exp_1_fit_rt01.rds")
exp_1_fit_rt02 <- readRDS("~/Documents/SetFitNetworks/fits/exp_1_fit_rt02.rds")

exp_2_fit_rt01 <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_rt01.rds")
exp_2_fit_rt02A <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_rt02A.rds")
exp_2_fit_rt02B <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_rt02B.rds")
exp_2_fit_rt03 <- readRDS("~/Documents/SetFitNetworks/fits/exp_2_fit_rt03.rds")

rtms1 <- list(exp_1_fit_rt01,exp_1_fit_rt02)
rtms2 <- list(exp_2_fit_rt01, exp_2_fit_rt02A, exp_2_fit_rt02B, exp_2_fit_rt03)

map(rtms1, bayestestR::sexit)
map(rtms2, bayestestR::sexit)

# TODO labels need to be changed
cm <- c('b_zleft_rating'    = 'left liking rating',
        'b_zright_rating'    = 'right liking rating',
        'b_zleft_net' = 'left network estimate',
        'b_zright_net' = 'right network estimate',
        'b_zleft_sim' = 'left similarity judgment',
        'b_zright_sim' = 'right similarity judgment',
        'b_zleft_rating:zleft_net' = 'left rating × network estimate',
        'b_zright_rating:zright_net' = 'right rating × network estimate'
)

file_name <- here::here("tables", paste0("rt", "_", net_stat, ".html"))

panels <- list("Experiment one:" = rtms1,
               "Experiment two:" = rtms2)
modelsummary(panels, 
             shape = "rbind",
             fmt = 2,
             centrality = "median", 
             statistic = "[{conf.low} {conf.high}]",
             coef_omit = "Intercept|.*subject_id",
             coef_map = cm,
             mc.cores = 10,
             output = file_name
)

 
