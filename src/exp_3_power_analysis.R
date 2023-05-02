# exp_3_power_analysis.R - power analysis simulations for sample size 
# calculations for experiment two
#
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
# 2023/04/02      Kianté  Fernandez                       wrote code
# 2023/04/05      Kianté  Fernandez                       updated parallel

library(lme4)
library(lmerTest)
library(simr)
library(parallel)

# Load the data (from analysis of experiment one and two)
#network_difference_regression_analysis.R for exp one
#exp2_network_difference_regression_analysis.R for exp two

# df_temp = create_dataset(df, type = "choice")
# df_exp2 <- df_temp[,c("subject_id", "choice","zleft_rating", "zright_rating", "zleft_net", "zright_net")]

load("data/pwr_analysis_exp_data.RData")
modelexp1 <- glmer(choice ~ (zleft_rating * zleft_net) + (zright_rating * zright_net) + (1 + zleft_rating + zright_rating | subject_id), data = df_exp1, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))
modelexp2 <- glmer(choice ~ (zleft_rating * zleft_net) + (zright_rating * zright_net) + (1 + zleft_rating + zright_rating | subject_id), data = df_exp2, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

# Print a summary of the model (make sure looks okay before simulations)
summary(modelexp1)
summary(modelexp2)

# Define a function for simulating power
simulate_power <- function(model, n, test) {
  model_n <- simr::extend(model, along = "subject_id", n = n)
  powerSim(model_n, test = test,
           nsim = 100, seed = 2023, alpha = 0.05)
}

n <- c(0, 30, 55, 70, 100)
# Simulate power for main effect
# main_effect_res1 <- mclapply(n, simulate_power, model = model, test = fixed(c("zleft_net"), "z"))
# main_effect_res2 <- mclapply(n, simulate_power, model = model, test = fixed(c("zright_net"), "z"))
# a naive parallel lapply can be created using mcparallel alone:
main_effect_res1exp1 <- lapply(n, function(n) parallel::mcparallel(simulate_power(n, model = modelexp1, test = fixed(c("zleft_net"), "z"))))
main_effect_res2exp1 <- lapply(n, function(n) parallel::mcparallel(simulate_power(n, model = modelexp1, test = fixed(c("zright_net"), "z"))))
main_effect_res1exp2 <- lapply(n, function(n) parallel::mcparallel(simulate_power(n, model = modelexp2, test = fixed(c("zleft_net"), "z"))))
main_effect_res2exp2 <- lapply(n, function(n) parallel::mcparallel(simulate_power(n, model = modelexp2, test = fixed(c("zright_net"), "z"))))
# wait for both jobs to finish and collect all results
res <- parallel::mccollect(list(main_effect_res1exp1, main_effect_res2exp1, main_effect_res1exp2, main_effect_res2exp2))

save(res,file  = "data/pwr_analysis_pc_results.RData")
names(res) <- as.character(c(rep(n + 24,2), rep(n + 51,2)))

mean(do.call(rbind, purrr::map(res,summary))[c(1,5),]$mean)
mean(do.call(rbind, purrr::map(res,summary))[c(1,5),]$lower)
mean(do.call(rbind, purrr::map(res,summary))[c(1,5),]$upper)

mean(do.call(rbind, purrr::map(res,summary))[c(11,16),]$mean)
mean(do.call(rbind, purrr::map(res,summary))[c(11,16),]$lower)
mean(do.call(rbind, purrr::map(res,summary))[c(11,16),]$upper)

