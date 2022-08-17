library(ggplot2)

x <- rnorm(100, 60, 5)
error <- rnorm(100, 0, 1)
y <- x + error

dat <- tibble::tibble(x,y)
cor(x, y)

ggplot(dat, aes(x, y)) + geom_point(color = "white")+
  geom_abline(intercept = 0, slope = 1)+theme_classic()+
  labs(x = "perceived similarity",
       y=  "network similarity")


c1 <- cluster_fast_greedy(g)
c2 <- cluster_edge_betweenness(g)
c3 <- cluster_infomap(g)
c4 <- cluster_leading_eigen(g)
c5 <- cluster_louvain(g)
c6 <- cluster_walktrap(g)
dendPlot(c6, "hclust")
