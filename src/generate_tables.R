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

ms1 <- list(exp_1_fit_choice01, exp_1_fit_choice02, exp_1_fit_choice03)
ms2 <- list(exp_2_fit_choice01, exp_2_fit_choice02A, exp_2_fit_choice02B, exp_2_fit_choice03, exp_2_fit_choice04)

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

#exp two
# modelsummary(ms, 
#              shape = term ~ model + statistic,
#              fmt = 2,
#              centrality = "median", 
#              statistic = "[{conf.low}, {conf.high}]",
#              coef_omit = "Intercept|.*subject_id",
#              coef_map = cm,
#              gof_map = NA)

net_stat = "modularity"
file_name <- here::here("tables", paste0("choice", "_", net_stat, ".html"))

panels <- list("Experiment one:" = ms1,
               "Experiment two:" = ms2)
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


#exp1                                                                                                                                                                                                                                                                                                                  

  Parameter                | Median |         95% CI | Direction | Significance (> |0.09|) | Large (> |0.54|)
  -----------------------------------------------------------------------------------------------------------
  Intercept                |   0.02 |  [-0.09, 0.13] |      0.63 |                    0.10 |             0.00
  zleft_rating             |   0.94 |   [0.74, 1.16] |      1.00 |                    1.00 |             1.00
  zleft_net                |   0.09 |  [-0.02, 0.20] |      0.95 |                    0.49 |             0.00
  zright_rating            |  -0.80 | [-1.03, -0.60] |      1.00 |                    1.00 |             0.99
  zright_net               |  -0.09 |  [-0.20, 0.02] |      0.95 |                    0.50 |             0.00
  zleft_rating:zleft_net   |   0.07 |  [-0.04, 0.18] |      0.90 |                    0.37 |             0.00
  zright_rating:zright_net |  -0.01 |  [-0.12, 0.10] |      0.60 |                    0.08 |             0.00
                                                                                                                                             

#exp2                                                                                                                                                                                                                                                                                                                    
 Parameter                |    Median |         95% CI | Direction | Significance (> |0.09|) | Large (> |0.54|)
 --------------------------------------------------------------------------------------------------------------
 Intercept                | -3.65e-03 |  [-0.09, 0.08] |      0.54 |                    0.02 |                0
 zleft_rating             |      0.82 |   [0.71, 0.94] |      1.00 |                    1.00 |                1
 zleft_net                |     -0.02 |  [-0.10, 0.05] |      0.72 |                    0.05 |                0
 zright_rating            |     -0.83 | [-0.95, -0.71] |      1.00 |                    1.00 |                1
 zright_net               |     -0.04 |  [-0.14, 0.05] |      0.83 |                    0.15 |                0
 zleft_sim                |      0.02 |  [-0.05, 0.09] |      0.70 |                    0.02 |                0
 zright_sim               |      0.03 |  [-0.05, 0.10] |      0.76 |                    0.04 |                0
 zleft_rating:zleft_net   |     -0.03 |  [-0.10, 0.05] |      0.75 |                    0.05 |                0
 zright_rating:zright_net |      0.04 |  [-0.03, 0.12] |      0.86 |                    0.10 |                0
 
