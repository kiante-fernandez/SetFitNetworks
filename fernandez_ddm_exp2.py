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

print(hddm.__version__)

#load the data in hddm format
# create the proper dataset structure for the HDDM functions
data = hddm.load_csv("/Users/fernandez.332/Documents/SetFitNetworks/data/HDDM_data.csv")

v_reg = {'model': 'v ~ 1 + vd + nd1 + nd2', 'link_func': lambda x: x}

m_temp = hddm.HDDMnnRegressor(data, models = v_reg, include=['v', 'a', 't'])
m_temp.find_starting_values()
m_temp.sample(5000, burn=2500)


v_reg1 = {'model': 'v ~ 1 + vd', 'link_func': lambda x: x}
v_reg2 = {'model': 'v ~ 1 + vd + nd1', 'link_func': lambda x: x}
v_reg3 = {'model': 'v ~ 1 + vd + nd1 + nd2', 'link_func': lambda x: x}
reg_descrs = [v_reg1, v_reg2, v_reg3]

res = []
res_model_idx = []
dic_info = []
#create a for loop to  mutiple chains
for model_idx in range(len(reg_descrs)):
  m_temp = hddm.HDDMnnRegressor(data, models = reg_descrs[model_idx], include=['v', 'a', 't'])
  m_temp.find_starting_values()
  m_temp.sample(5000, burn=2500)
  res.append(m_temp.gen_stats())
  res_model_idx.append(model_idx)
  dic_info.append(m_temp.dic)
  pd.DataFrame(m_temp.gen_stats()).to_csv('/Users/fernandez.332/Documents/SetFitNetworks/fits/network_model_results_' + str(model_idx)+ '.csv')


pd.DataFrame(dic_info).to_csv('/Users/fernandez.332/Documents/SetFitNetworks/fits/HDDMRegressor_model_comparisons.csv')

#model comparisons

#PPC
#how do you get samples for 
#hddm_model.plot_posterior_predictive(value_range = np.arange(0, 5, 0.1), samples = 100, alpha = 0.01)

#note if you do this in a .ipynb file notebook you can get it to work
hddm.plotting.plot_posterior_predictive(model = m_temp,
                                        value_range = np.arange(0, 6, 0.1),
                                        **{'alpha': 0.1,
                                        'ylim': 3,
                                        'add_posterior_uncertainty_model': True,
                                        'add_posterior_uncertainty_rts': True,
                                        'add_posterior_mean_rts': True,
                                        'samples': 200})

plt.show()

ppc_data = hddm.utils.post_pred_gen(m_temp) #works to get samples for the regression models

ppc_data = hddm.utils.post_pred_gen(m_temp, groupby = ['subj_idx'])

