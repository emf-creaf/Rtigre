## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.width = 5,  fig.align = "center"-------------------------------------
ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width,
               group = Species, colour = Species)) + 
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = c("red", "blue", "green")) +
  ggplot2::labs(x = "Sepal length", y = "Sepal width")

## -----------------------------------------------------------------------------
iris$setosa <- iris$Species == "setosa"
iris$versicolor <- iris$Species == "versicolor"
iris$virginica <- iris$Species == "virginica"

## -----------------------------------------------------------------------------
m1 <- nls(Sepal.Width ~ c1 * exp(-(c2*Sepal.Length + c3*setosa + c4*versicolor)), iris,
         start = list(c1 = 1, c2 = .01, c3 = 0, c4 = 0))

m2 <- nls(Sepal.Width ~ exp(-(c2*Sepal.Length + c3*setosa + c4*versicolor + c5*virginica)), iris,
         start = list(c2 = .01, c3 = 0, c4 = 0,  c5 = 0))

## -----------------------------------------------------------------------------
library(Rtigre)
data("Punci_IFN")
Punci_IFN$Treatment <- sample(LETTERS[1:3], nrow(Punci_IFN), replace = T)
Punci_IFN$tdiff <- 10

## -----------------------------------------------------------------------------
Punci_IFN$A <- (Punci_IFN$Treatment == "A")*1
Punci_IFN$B <- (Punci_IFN$Treatment == "B")*1
Punci_IFN$C <- (Punci_IFN$Treatment == "C")*1
r <- fit_growth(Punci_IFN, ~ prec + temp + A + B, verbose = F)

