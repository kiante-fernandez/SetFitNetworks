# autoencoder in keras
library(keras)
library(igraph)
# standardise
minmax <- function(x) (x - min(x))/(max(x) - min(x))
load("/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/rating_network_graph.RData")
source(here::here("src", "utils.R"))

net_degree <- calculate_net_stats(g)

x_train <- apply(net_degree[,2:7], 2, minmax)

# set training data
x_train <- as.matrix(x_train)

# set model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 6, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 2, activation = "tanh", name = "bottleneck") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))

# view model layers
summary(model)

# compile model
model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)

# fit model
model %>% fit(
  x = x_train, 
  y = x_train, 
  epochs = 2000,
  verbose = 0
)

# evaluate the performance of the model
mse.ae2 <- evaluate(model, x_train, x_train)
mse.ae2

# extract the bottleneck layer
intermediate_layer_model <- keras_model(inputs = model$input, outputs = get_layer(model, "bottleneck")$output)
intermediate_output <- predict(intermediate_layer_model, x_train)

ggplot(data.frame(PC1 = intermediate_output[,1], PC2 = intermediate_output[,2]), aes(x = PC1, y = PC2, label = net_degree$Name, color = factor(net_degree$snack_type))) + 
  geom_point() +
  geom_text(hjust=0, vjust=0)+
  theme_classic()+
  scale_color_brewer(palette = "Set1") +
  # scale_y_continuous(limits = c(-.42, .49)) +
  # scale_x_continuous(limits = c(-.12, .49)) +
  labs(
    y = "",
    x = "",
    color = ""
  ) 

net_degree$unit1 <- intermediate_output[,1]
net_degree$unit2 <- intermediate_output[,2] 
# net_degree$unit3 <- intermediate_output[,3]

net_degree %>% 
  dplyr::select(degree,strength,eigen,weighted_transitivity,closeness,betweenness,unit1,unit2,PCA1, PCA2, PCA3, PCA4, PCA5, PCA6) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(type = "upper",
                         lab = TRUE)+
  theme_classic()+
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(face = "bold"),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
# plot the reduced dat set
# plotly::plot_ly(net_degree, x = ~PCA1, y = ~PCA2, z = ~PCA3, color = ~factor(net_degree$snack_type)) %>% plotly::add_markers()
# plotly::plot_ly(net_degree, x = ~unit1, y = ~unit2, z = ~unit3, color = ~factor(net_degree$snack_type)) %>% plotly::add_markers()
