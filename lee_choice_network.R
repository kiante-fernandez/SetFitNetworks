
library(here)
library(readr)
library(dplyr)
# library(bootnet)
library(EGAnet)

FoodNames <- readxl::read_excel(here("data","snackitemnames_nicholas","item_image_numbers_exp2_5_nicholas.xlsx"))
lee_2021_choice <- read_csv(here("data","lee_2021_choice_exp2_5.csv"),col_names = FALSE)

choice_matrix <- as.data.frame(matrix(nrow = nrow(lee_2021_choice),
                                      ncol =  60))

for (subject_id in 1:nrow(lee_2021_choice)){
  choice_matrix[subject_id,] <- as.numeric(FoodNames$Image %in% lee_2021_choice[subject_id,])
}
names(choice_matrix) <- FoodNames$Name

cor.snack_food <- SemNeT::similarity(choice_matrix, method = "cor")
# cor.snack_food <- SemNeT::similarity(choice_matrix, method = "cosine")

#note you have code here that should NOT work
m <- as.matrix(cor.snack_food)
mDim <- length(m[1, ]) # determine size of one dim of the matrix, which we assume is identical to the other dim.
# calulate the correlations
ggcorrplot::ggcorrplot(m[mDim:1, ],
                       colors = c("red", "white", "green"),
                       ggtheme = ggplot2::theme_classic,
                       outline.color = "white",
                       show.diag = T,
                       hc.order	=F
) + ggplot2::theme(
  axis.text.x = element_text(size = 4),
  axis.text.y = element_text(size = 4),
  axis.ticks = element_blank()
)+theme(legend.position="left")

net.snack_food <- SemNeT::TMFG(cor.snack_food)

boot_test <- EGAnet::bootEGA(cor.snack_food, 
                             iter = 1000,
                             n = 267,
                             algorithm = "walktrap",
                             ncores = 8, typicalStructure = T)

boot_test[["plot.typical.ega"]][["layers"]][[6]]<- NULL
boot_test$plot.typical.ega
