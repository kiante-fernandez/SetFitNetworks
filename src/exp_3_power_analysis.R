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

library(lme4)
library(lmerTest)
library(simr)

library(parallel)
# library(faux)
# Load the data (from analysis of experiment one and two)

df_temp = create_dataset(df, type = "choice")

model <- glmer(choice ~ (zleft_rating * zleft_net) + (zright_rating * zright_net) + (1 + zleft_rating + zright_rating | subject_id), 
                data = df_temp, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

# Print a summary of the model
summary(model)
# test = simr::fixed("zright_net", "z")
# model_n <- simr::extend(model, along = "subject_id", n = 0)
# test = doTest(model, fixed(c("zleft_net"), "z"))
# tespwr <- powerSim(model_n, test =  simr::fixed(c("zleft_net"), "z"),
#          nsim = 10, seed = 2022, alpha = 0.05)
# tespwr
# Define a function for simulating power
simulate_power <- function(model, n, test) {
  model_n <- simr::extend(model, along = "subject_id", n = n)
  powerSim(model_n, test = test,
           nsim = 100, seed = 2022, alpha = 0.05)
}

n <- c(0, 30, 55, 70, 100)
# Simulate power for main effect
main_effect_res1 <- mclapply(n, simulate_power, model = model, test = fixed(c("zleft_net"), "z"))
main_effect_res2 <- mclapply(n, simulate_power, model = model, test = fixed(c("zright_net"), "z"))

# Stop the parallel cluster
stopCluster(cluster)

