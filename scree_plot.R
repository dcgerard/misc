## Minimal R Code for Scree Plot Figure

## Install Necessary Packages -----------------------------------

# install.packages(c("PerMallows", "tensr", "tidyverse",
#                    "ggthemes", "gridExtra"))
library(PerMallows)
library(tensr)
library(tidyverse)
library(ggthemes)
library(gridExtra)

get_mat_rep <- function(x) {
  stopifnot(is.vector(x))
  rmat <- outer(x, x, FUN = `<`) * 1
  rmat[outer(x, x, FUN = `==`)] <- 0
  rmat[outer(x, x, FUN = `>`)] <- -1
  return(rmat)
}

get_tens_rep <- function(x) {
  stopifnot(is.matrix(x))
  tout <- array(apply(X = x, MARGIN = 2, FUN = get_mat_rep),
                dim = c(nrow(x), nrow(x), ncol(x)))
  return(tout)
}

## Generate Data -----------------------------------------

set.seed(1)
nind <- 100
rankmat1 <- apply(X = rmm(n = nind, sigma0 = 1:10, theta = 0.3),
                  MARGIN = 1,
                  FUN = order)
rankmat2 <- apply(X = rmm(n = nind, sigma0 = c(10:1), theta = 0.3),
                  MARGIN = 1,
                  FUN = order)
rankmat <- cbind(rankmat1, rankmat2)
group_vec <- c(rep("1", nind), rep("2", nind))

## Make Plots -----------------------------------------------

tout <- get_tens_rep(x = rankmat)
svout <- svd(mat(tout, 3))

data.frame(sv = svout$d, order = 1:length(svout$d), Group = group_vec) %>%
  ggplot(mapping = aes(x = order, xend = order, y = 0, yend = sv)) +
  geom_segment() +
  theme_bw() +
  xlab("Index") +
  ylab("Singular Value") +
  ggtitle("(A)") ->
  pl1

data.frame(sv1 = svout$u[, 1], sv2 = svout$u[, 2], Group = group_vec) %>%
  ggplot(mapping = aes(x = sv1, y = sv2, color = Group)) +
  geom_point() +
  scale_color_colorblind() +
  theme_bw() +
  xlab("First Mode-3 Singular Vector") +
  ylab("Second Mode-3 Singular Vector") +
  ggtitle("(B)") ->
  pl2

pdf(file = "combined.pdf", family = "Times", height = 2.5, width = 6)
gridExtra::grid.arrange(pl1, pl2, layout_matrix = matrix(c(1, 2), nrow = 1), widths = c(2.5, 3.5))
dev.off()
