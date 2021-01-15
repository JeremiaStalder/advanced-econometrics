#######################################################################################################################
# Project - Advanced Econometric Methods
# Erik Senn, Johannes Cordier, Mila Gorkun-Voevoda, Davia Kündig and Jeremia Stalder
#
# Description:
# Function used for "non_para_M_2.R"

pcatr <- function(data){
  col.means = as.vector(colMeans(data))
  m = matrix(col.means, nrow(data), NROW(col.means), byrow = T)
  x = data - m
  eig = eigen(cov(x))  # spectral decomposition  
  eig.v = eig$vectors
  x.m = as.matrix(x)
  pcs = x.m %*% eig.v
  return(as.data.frame(pcs))
}