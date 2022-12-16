# exp_2_power_analysis.R - power analysis simulations for sample size 
# calculations for experiment two
#
# Copyright (C) 2022 Kianté Fernandez, <kiantefernan@gmail.com>
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
# 09/11/22      Kianté  Fernandez                       wrote code

library(simr)
library(lme4)
library(lmerTest)
library(parallel)
# Load the data (from other script)
data <- model_dat

# Define the model
model <- glmer(correct ~ vd*nd + ov*on + (vd*nd + ov*on | subject_id), 
               data = data, 
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))

# Print a summary of the model
summary(model)

# Define a function for simulating power
simulate_power <- function(model, n, test) {
  model_n <- simr::extend(model, along = "subject_id", n = n)
  powerSim(model_n, test = test,
           nsim = 100, seed = 2022, alpha = 0.05)
}

n <- c(0, 30, 55, 70, 100)
# Simulate power for main effect
main_effect_res <- mclapply(n, simulate_power, model = model, test = simr::fixed("nd", "z"))

# Simulate power for interaction effect
interaction_effect_res <- mclapply(n, simulate_power, model = model, test = simr::fixed("vd:nd", "z"))

# Stop the parallel cluster
stopCluster(cluster)

