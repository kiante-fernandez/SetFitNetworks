# fernandez_DDM_analysis.py - run hddm for study two data
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
# 2022/12/29      Kianté  Fernandez                   wrote initial code


##hddm and laura package for doing ddm
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import hddm
import pickle

#load the data in hddm format
# data = hddm.load_csv("~/Documents/ddm_fatigue/data/HDDM_data.csv")

#write our model specification that are the same as a regression model \
# model_spec4 = {'v': 'reward'}

model_spec
model_spec = [model_spec1, model_spec2, model_spec3, model_spec4]


#create a loop todo mutiple chains
res = []
res_model_idx = []
dic_info = []

for model_idx in range(len(model_spec)):
  m_temp = hddm.HDDM(HC, depends_on=model_spec[model_idx])
  #m_temp.find_starting_values()
  m_temp.sample(5000, burn=2500)
  res.append(m_temp.gen_stats())
  res_model_idx.append(model_idx)
  dic_info.append(m_temp.dic)
  pd.DataFrame(m_temp.gen_stats()).to_csv('model_results/HC_model_results_' + str(model_idx)+ '.csv')

#model comparisons

#PPC
#how do you get samples for 
hddm_model.plot_posterior_predictive(value_range = np.arange(0, 5, 0.1), samples = 100, alpha = 0.01)

