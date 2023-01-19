library(igraph)
library(NetworkComparisonTest)
library(qgraph)
library(ggplot2)
library(patchwork)

load("~/Documents/OSU/SetFitNetworks/data/fernandez_lee_NCT_2.RData")

nw1 <- graph_from_adjacency_matrix(NCT_res$nw1,
                                               "undirected",
                                               weighted = TRUE
)
nw2 <- graph_from_adjacency_matrix(NCT_res$nw1,
                                              "undirected",
                                              weighted = TRUE
)

L <- averageLayout(nw1, nw2)

E(nw1)$color[E(nw1)$weight > 0] <- "forestgreen"
E(nw1)$color[E(nw1)$weight < 0] <- "red2"

E(nw2)$color[E(nw2)$weight > 0] <- "forestgreen"
E(nw2)$color[E(nw2)$weight < 0] <- "red2"

par(mfrow = c(1, 2)) # set the plotting area into a 1*3 array
plot(nw1,
     # layout = L,
     layout  = layout_in_circle(nw1),
     margin = .0,
     vertex.label = V(nw1)$name,
     vertex.label.color = "black",
     vertex.label.font = 2,
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     vertex.size = 5,
     # vertex.shape="none",
     # vertex.color = adjustcolor(alpha.f = .5),
     vertex.label.family = "Times",
     main = "Lee & Holyoak",
     edge.curved = .15,
     edge.width = abs(E(nw1)$weight) * 5,
     # mark.groups = value_name_data$dimension
)
plot(nw2,
     # layout = L,
     layout  = layout_in_circle(nw2),
     margin = .0,
     vertex.label = V(nw2)$name,
     vertex.label.color = "black",
     vertex.label.font = 2,
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     vertex.size = 5,
     # vertex.shape="none",
     # vertex.color = adjustcolor(alpha.f = .5),
     vertex.label.family = "Times",
     main = "Experiement one & two",
     edge.curved = .15,
     edge.width = abs(E(nw2)$weight) * 5,
     # mark.groups = value_name_data$dimension
)
NCT_res$nwinv.pval
p_temp1 <-  data.frame(x = NCT_res$nwinv.perm)
p1 <- ggplot(p_temp1, aes(x))+
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 30) +
  geom_vline(xintercept = NCT_res$nwinv.real, linetype = "dashed", size = 1.7, color  = "red") +
  geom_text(x=NCT_res$nwinv.real + 0.05, y=7.7, label="M = 0.24, p = 0.47")+
  theme_classic()+
  labs(x = "Maxoumum of difference in edge weights", y = "Frequency")+
  theme(axis.text = element_text(face = "bold"),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold"))

p_temp2 <-  data.frame(x = NCT_res$glstrinv.perm)
p2 <- ggplot(p_temp2, aes(x))+
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 20) +
  geom_vline(xintercept = NCT_res$glstrinv.real, linetype = "dashed", size = 1.7, color = "red") +
  geom_text(x=NCT_res$glstrinv.real + .35 , y=7.7, label="S = 3.81, p = 0.04")+
  theme_classic()+
  labs(x = "Difference in global strength", y = "Frequency")+
  theme(axis.text = element_text(face = "bold"),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold"))
p1 + p2

NCT_res$diffcen.real
NCT_res$diffcen.real[NCT_res$diffcen.pval < 0.05] 

p_temp3 <-  data.frame(x = NCT_res$diffcen.perm[,"pocky.strength"])
p3 <- ggplot(p_temp3, aes(x))+
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 20) +
  geom_vline(xintercept = NCT_res$diffcen.real[row.names(NCT_res$diffcen.real) == "pocky"], linetype = "dashed", size = 1.7, color = "red") +
  theme_classic()+
  labs(x = "Difference in strength", y = "Frequency", title = "Pocky")+
  theme(axis.text = element_text(face = "bold"),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold"))

p_temp4 <-  data.frame(x = NCT_res$diffcen.perm[,"raspberry.strength"])
p4 <- ggplot(p_temp4, aes(x))+
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 20) +
  geom_vline(xintercept = NCT_res$diffcen.real[row.names(NCT_res$diffcen.real) == "raspberry"], linetype = "dashed", size = 1.7, color = "red") +
  theme_classic()+
  labs(x = "Difference in strength", y = "Frequency", title = "Raspberry")+
  theme(axis.text = element_text(face = "bold"),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold"))

p_temp5 <-  data.frame(x = NCT_res$diffcen.perm[,"boule bread.strength"])
p5 <- ggplot(p_temp5, aes(x))+
  geom_histogram(color = "black", fill = "dodgerblue1", alpha = .8, bins = 20) +
  geom_vline(xintercept = NCT_res$diffcen.real[row.names(NCT_res$diffcen.real) == "boule bread"], linetype = "dashed", size = 1.7, color = "red") +
  theme_classic()+
  labs(x = "Difference in strength", y = "Frequency", title = "Boule Bread")+
  theme(axis.text = element_text(face = "bold"),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold"))
p3 + p4 + p5

