library(dplyr)
library(np)


setwd("C:/Users/lmgv/iCloudDrive/SG MiQEF/3 semester/Advanced Econometric Methods/Project/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")

source("./code/non_para_M_functions.R")


npdata = mydata_transform
Y = npdata$tw_adjust_std #numeric vector with outcome variable
Ti = npdata$e401_std #Treatment 
indep_vars = variable_sets_modelling[["independent_vars_selection_std"]]
squared_vars = c("age_sq","age_cub","inc_sq","inc_cub")

X = dplyr::select(npdata, all_of(indep_vars))

#### Data preparation ####
## divide the data into two samples - where treatment equal to 0 and to 1
Y.0 = npdata[npdata[, "e401_std"] == min(X[, "e401_std"]),]$tw_adjust_std
Y.1 = npdata[npdata[, "e401_std"] == max(X[, "e401_std"]),]$tw_adjust_std

X.0 = X[X[, "e401_std"] == min(X[, "e401_std"]),]
X.1 = X[X[, "e401_std"] == max(X[, "e401_std"]),]

X.0 = dplyr::select(X.0, -c("e401_std"))
X.1 = dplyr::select(X.1, -c("e401_std"))

#### PCA ####
n_pca = 3 # number of principal components to use

PC.0 = pcatr(X.0)[, 1:n_pca]
PC.1 = pcatr(X.1)[, 1:n_pca]

#### Grid ####
## generate matrix x_p with grid for all of the variables to use in the kernel estimation
## grids are created to be from min to max of the variable with n = n

n = nrow(PC.0)
##n = 100

## this loop creates grids for each variable (which correspond to principal components)
## of each of the subsamples (PC.0 and PC.1)
## it allows for changing number of principal components used
## example: grid.0.1 corresponds to the grid of first PC of the 0-subsample

for (e in c(0, 1)){
  for (i in seq(1, n_pca)){
    variable = get(paste0("PC.", e))[, paste0("V", i)]
    assign(paste0("grid.", e, ".", i), seq(min(variable), max(variable), l = n))
  }
}


#### Bandwidth estimation ####
## Rule of thumb:
##h.rot.0.1 = 1.06*sd(PC.0$V1)*n^{-1/5}

# Least squares cross validation:

# this loop computes optimal bandwidths for each variable by LS CV
# example: h.vector.0 contains bandwidths for variables in PC.0
# takes time to run
for (e in c(0, 1)){
  assign(paste0("h.vector.", e), numeric(n_pca))
  hv = get(paste0("h.vector.", e))
  for (i in seq(1, n_pca)){
    variable = get(paste0("PC.", e))[, paste0("V", i)]
    assign(paste0("h.cv.", e, ".", i), npudensbw(variable, bwmethod="cv.ls"))
    if (e == 0){
      h.vector.0[i] = get(paste0("h.cv.0.", i))$bw
    }
    if (e == 1){
      h.vector.1[i] = get(paste0("h.cv.1.", i))$bw
    }
  }
}

#### Kernel estimation ####
# Individual Kernel Density Estimation

## this loop estimates individual kernel density for each variable
## example: k.0.3 = npudens(bws = h.cv.0.3$bw, PC.0$V3)


##for (e in c(0, 1)){
##  for (i in seq(1, n_pca)){
##    variable = get(paste0("PC.", e))[, paste0("V", i)]
##    bandwidth = get(paste0("h.cv.", e, ".", i))
##    assign(paste0("k.", e, ".", i), npudens(bws = bandwidth$bw, tdat = variable))
##  }
##}


# this loop estimates density for the set of variables used for each of the 0-1-subsamples
# example: k.0 is the density object for variables in PC.0

for (e in c(0, 1)){
  assign(paste0("k.", e), npudens(bws = get(paste0("h.vector.", e)), 
                                  tdat = get(paste0("PC.", e))))
}

k.test.0 = npudens(bws = h.vector.0, tdat = (cbind(grid.0.1, grid.0.2, grid.0.3) - PC.0))
k.test.1 = npudens(bws = h.vector.1, tdat = (cbind(grid.1.1, grid.1.2, grid.1.3) - PC.1))


##d1 = npudens(bws = c(h.cv.0.1$bw, h.cv.0.2$bw, h.cv.0.3$bw), tdat = PC.0)
##d1.f = fitted(npudens(bws = c(h.cv.0.1$bw, h.cv.0.2$bw, h.cv.0.3$bw), tdat = PC.0,
##                      edat = expand.grid(V1=grid.0.1, V2=grid.0.2, V3 = grid.0.3)))
##f <- matrix(d1.f, 100, 100)

#plot(PC.0$V1, k.0.1$dens)
# se(k.0.1)


#### Nadaraya-Watson estimator ####
# m^hat(x) = ( (\sum(y_i* K))/ (\sum(K)))

m.0 = sum(Y.0 * k.0$dens)/sum(k.0$dens)
m.1 = sum(Y.1 * k.1$dens)/sum(k.1$dens)

m.0 = sum(Y.0 * k.test.0$dens)/sum(k.test.0$dens)
m.1 = sum(Y.1 * k.test.1$dens)/sum(k.test.1$dens)


##m.0 = sum(Y.0 %*% k.0$dens)/sum(k.0$dens)
##m.1 = sum(Y.1 %*% k.1$dens)/sum(k.1$dens)

##gg = cbind(grid.0.1, grid.0.2, grid.0.3)
##Kx <- sapply(PC.0, function(Xi) npudens(tdat = (gg - Xi), bws = h.vector.0))
##W <- Kx / rowSums(Kx)


ate = m.1 - m.0

# Estimate individual bandwidths and individual univariate kernels for each of the PCs of each of the subsamples
# then estimate the K by multiplying them -- decided not to do this, but to estimate them jointly from the beginning
# then estimate the nadaraya-watson estimator by plugging them into the formula


#### package ####
library(stats)

PC = pcatr(X)[, 1:n_pca]
ksmooth(x = cbind(Ti, PC), y = Y)
ky = ksmooth(x = Ti, y = Y)$y
unique(ky)
