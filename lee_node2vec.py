# lee_node2vec.py- conduct network representation learning
# saves results for further analysis 
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
# 2023/10/02   Kianté  Fernandez                      version one


#use node2vec to extract features from association network
# %% load packages
import networkx as nx
import pandas as pd
from gensim.models.word2vec import Word2Vec
import matplotlib.pyplot as plt
from karateclub import Node2Vec

# %% importing the food association network from bootEGA analysis

# Read the CSV file into a DataFrame
df = pd.read_csv('data/edge_list.csv')

# Create an empty Graph object
G = nx.Graph()
# Add edges and their weights to the graph
for index, row in df.iterrows():
    G.add_edge(row[0], row[1], weight=row[2])

# Preserve original node names before relabeling for Node2Vec processing
food_names = list(G.nodes())
G = nx.relabel_nodes(G, {node: i for i, node in enumerate(G.nodes())})

# Visualize the graph
pos = nx.spring_layout(G, seed=42)
nx.draw(G, pos, with_labels=True, node_color='lightblue', edge_color='gray', node_size=1000, width=[d['weight'] for _, _, d in G.edges(data=True)])
plt.show()

# %% Initialize and train a Node2Vec model
node2vec = Node2Vec(
    walk_number=10, 
    walk_length=80, 
    p=1.0, 
    q=1.0, 
    dimensions=128, 
    workers=4, 
    window_size=5, 
    epochs=1, 
    learning_rate=0.05, 
    min_count=1, 
    seed=42
)

# Fit the model to the graph
node2vec.fit(G)

# Extract embeddings
embeddings = node2vec.get_embedding()

# Display stats
print('Number of food items:', len(G.nodes))
print('Embedding array shape generated from food association network:', embeddings.shape)

# Create a dataframe with embeddings, using original node names as the index
node2vec_results = pd.DataFrame(embeddings, index=food_names)

# Save node2vec_results to a CSV file
node2vec_results.to_csv("data/node2vec_embeddings.csv")
