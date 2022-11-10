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

# NOTE YOU NEED TO RUN NETWORK_DIFFERENCES FIRST 
library(simr)
library(lme4)
library(lmerTest)

#LOAD THE DATA FROM THE OTHER SCRIPT THEN:
#RUN THE INTIAL MODEL
gmlm<- glmer(choice ~ vd*nd + ov + on + (vd*nd + ov + on | subject_id), data = model_dat, 
                family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
summary(gmlm)
doTest(gmlm,test = fixed("vd:nd"))

res_p0 <- powerSim(gmlm,test = simr::fixed("vd:nd", "z"),nsim = 100,seed = 2022,alpha = 0.05)
res_p
#add 30 subjects?
model1 <- extend(gmlm, along="subject_id", n=30)
res_p1 <- powerSim(model1,test = simr::fixed("vd:nd", "z"),nsim = 100,seed = 2022,alpha = 0.05)
res_p1
#add 55 subjects?
model2 <- extend(gmlm,along = "subject_id", n=55)
res_p2 <- powerSim(model2,test = simr::fixed("vd:nd", "z"),nsim = 100,seed = 2022,alpha = 0.05)
res_p2
#add 70 subjects?
model3 <- extend(gmlm,along = "subject_id", n=70)
res_p3 <- powerSim(model3,test = simr::fixed("vd:nd", "z"),nsim = 100,seed = 2022,alpha = 0.05)
res_p3
#add 100 subjects?
model4 <- extend(gmlm,along = "subject_id", n=100)
res_p4 <- powerSim(model4,test = simr::fixed("vd:nd", "z"),nsim = 100,seed = 2022,alpha = 0.05)
res_p4

