#use node2vec to extract features from association network

# Importing nessesary Libraries 

import networkx as nx # to visulaize and load graph data
import random
import numpy as np
from gensim.models.word2vec import Word2Vec # for implimenting Skip-Gram Model

import matplotlib.pyplot as plt

from IPython.display import display
from PIL import Image

from karateclub import Node2Vec

# importing the food association network from bootEGA analysis

# Load the edge list from a CSV file
with open('edge_list.csv', 'r') as f:
    edges = [tuple(map(int, line.split(','))) for line in f]

# Create a graph from the edge list
G = nx.Graph()
G.add_edges_from(edges)

# Print the graph
print(G)

# Create a list of edges, where each edge is a tuple of (source, destination, weight)
edges = [(0, 1, 1.0), (1, 2, 2.0), (2, 3, 3.0)]

# Add the edges to the graph
nx.add_weighted_edges_from(graph, edges)

# Print the graph
print(graph)




model = Node2Vec(walk_length=10,walk_number=5,window_size=20,dimensions=20,p=5.0,q=10.0)
model.fit(G)
embeddings = model.get_embedding()


print('Number of food items:', len(G.nodes))
print('Embedding array shape generated from food association network:', embeddings.shape)
