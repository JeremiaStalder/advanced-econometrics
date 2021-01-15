library(dplyr)
library(np)


setwd("C:/Users/lmgv/iCloudDrive/SG MiQEF/3 semester/Advanced Econometric Methods/Project/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")

source("./code/non_para_M_functions.R")


npdata = mydata_transform
Y = npdata$tw_adjust_std #numeric vector with outcome variable
D = npdata$e401_std #Treatment 
indep.vars = variable_sets_modelling[["independent_vars_std"]]
##squared_vars = c("age_sq","age_cub","inc_sq","inc_cub")

X = dplyr::select(npdata, all_of(indep.vars))

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

##h.cv.t = npudensbw(Ti, bwmethod="cv.ls")
##h.rot.t = 1.06*sd(Ti)*length(Ti)^{-1/5}

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

##k.test.0 = npudens(bws = h.vector.0, tdat = (cbind(grid.0.1, grid.0.2, grid.0.3) - PC.0))
##k.test.1 = npudens(bws = h.vector.1, tdat = (cbind(grid.1.1, grid.1.2, grid.1.3) - PC.1))

##k.test.t = dnorm(((Ti - (-0.7848699)) / h.rot.t)/h.rot.t)
##k.test.t1 = dnorm(((Ti - (1.273964)) / h.rot.t)/h.rot.t)
##k.test.t1.0 = npudens(bws = h.cv.t$bw, tdat = Ti - (-0.7848699))
##k.test.t1.1 = npudens(bws = h.cv.t$bw, tdat = Ti - 1.273964)

##k.test.t1.0 = npudens(bws = h.rot.t, tdat = Ti - (-0.7848699))
##k.test.t1.1 = npudens(bws = h.rot.t, tdat = Ti - 1.273964)


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

##m.0 = sum(Y.0 * k.test.0$dens)/sum(k.test.0$dens)
##m.1 = sum(Y.1 * k.test.1$dens)/sum(k.test.1$dens)

##m.0 = sum(Y * k.test.t)/sum(k.test.t)
##m.1 = sum(Y * k.test.t1)/sum(k.test.t1)


##m.0 = sum(Y.0 %*% k.0$dens)/sum(k.0$dens)
##m.1 = sum(Y.1 %*% k.1$dens)/sum(k.1$dens)

##gg = cbind(grid.0.1, grid.0.2, grid.0.3)
##Kx <- sapply(PC.0, function(Xi) npudens(tdat = (gg - Xi), bws = h.vector.0))
##W <- Kx / rowSums(Kx)


ate = m.1 - m.0

save(ate, file = "./output/results/nonparametric/ate_M.RData")

# Estimate individual bandwidths and individual univariate kernels for each of the PCs of each of the subsamples
# then estimate the K by multiplying them -- decided not to do this, but to estimate them jointly from the beginning
# then estimate the nadaraya-watson estimator by plugging them into the formula


### package ###
##library(stats)

##PC = pcatr(X)[, 1:n_pca]
##ksmooth(x = cbind(Ti, PC), y = Y)
##ky = ksmooth(x = Ti, y = Y)$y
##unique(ky)

#### CATEs ####

X = dplyr::select(npdata, all_of(indep.vars))
XY = cbind(Y, X, npdata$inc_quintile)
colnames(XY)[ncol(XY)] <- "quintile"


# this loop selects independent variables that only belong to a certain quintile
# then creates sub-samples that correspond to 0 and 1 treatment values (both X and Y)
# example: X.q.1 contains independent variables from the 1st quintile
#          X.q.1.0 contains independent variables from the 1st quintile of the 0-treatment subsample
#          Y.q.1.0 contains dependent variable corresponding to the 1st quintile of the 0-treatment subsample
for (i in 1:5){
    assign(paste0("X.q.", i, ".0"), XY[XY[, "e401_std"] == min(XY[, "e401_std"]) & XY[, "quintile"] == i, ])
    assign(paste0("X.q.", i, ".0"), dplyr::select(get(paste0("X.q.", i, ".0")), -c("quintile", "Y")))
    
    assign(paste0("X.q.", i, ".1"), XY[XY[, "e401_std"] == max(XY[, "e401_std"]) & XY[, "quintile"] == i, ])
    assign(paste0("X.q.", i, ".1"), dplyr::select(get(paste0("X.q.", i, ".1")), -c("quintile", "Y")))
    
    assign(paste0("Y.q.", i, ".0"), XY[XY[, "e401_std"] == min(XY[, "e401_std"]) & XY[, "quintile"] == i, "Y"])
    assign(paste0("Y.q.", i, ".1"), XY[XY[, "e401_std"] == max(XY[, "e401_std"]) & XY[, "quintile"] == i, "Y"])
}

### PCA

for (i in 1:5){
  assign(paste0("PC.q.", i, ".0"), pcatr(get(paste0("X.q.", i, ".0")))[, 1:n_pca])
  assign(paste0("PC.q.", i, ".1"), pcatr(get(paste0("X.q.", i, ".1")))[, 1:n_pca])
}

### Bandwidth

# this loop computes optimal bandwidths for each variable by LS CV
# example: h.vector.q.1.0 contains bandwidths for variables in PC.q.1.0
# takes time to run
for (i in 1:5){
  for (e in c(0, 1)){
    assign(paste0("h.vector.q.", i, ".", e), numeric(n_pca))
    hv = get(paste0("h.vector.q.", i, ".", e))
    for (j in seq(1, n_pca)){
      variable = get(paste0("PC.q.", i, ".", e))[, paste0("V", j)]
      assign(paste0("h.cv.q.", i, ".", e, ".", j), npudensbw(variable, bwmethod="cv.ls"))
      if (e == 0){
        if (i == 1){
          h.vector.q.1.0[j] = get(paste0("h.cv.q.1.0.", j))$bw
        }
        if (i == 2){
          h.vector.q.2.0[j] = get(paste0("h.cv.q.2.0.", j))$bw
        }
        if (i == 3){
          h.vector.q.3.0[j] = get(paste0("h.cv.q.3.0.", j))$bw
        }
        if (i == 4){
          h.vector.q.4.0[j] = get(paste0("h.cv.q.4.0.", j))$bw
        }
        if (i == 5){
          h.vector.q.5.0[j] = get(paste0("h.cv.q.5.0.", j))$bw
        }
      }
      if (e == 1){
        if (i == 1){
          h.vector.q.1.1[j] = get(paste0("h.cv.q.1.1.", j))$bw
        }
        if (i == 2){
          h.vector.q.2.1[j] = get(paste0("h.cv.q.2.1.", j))$bw
        }
        if (i == 3){
          h.vector.q.3.1[j] = get(paste0("h.cv.q.3.1.", j))$bw
        }
        if (i == 4){
          h.vector.q.4.1[j] = get(paste0("h.cv.q.4.1.", j))$bw
        }
        if (i == 5){
          h.vector.q.5.1[j] = get(paste0("h.cv.q.5.1.", j))$bw
        }
      }
    }
  }
}


### Kernel

for (i in 1:5){
  for (e in c(0, 1)){
    assign(paste0("k.q.", i, ".", e), npudens(bws = get(paste0("h.vector.q.", i, ".", e)), 
                                              tdat = get(paste0("PC.q.", i, ".", e))))
  }
}


### N-W estimator

m.q.1.0 = sum(Y.q.1.0 * k.q.1.0$dens)/sum(k.q.1.0$dens)
m.q.1.1 = sum(Y.q.1.1 * k.q.1.1$dens)/sum(k.q.1.1$dens)

m.q.2.0 = sum(Y.q.2.0 * k.q.2.0$dens)/sum(k.q.2.0$dens)
m.q.2.1 = sum(Y.q.2.1 * k.q.2.1$dens)/sum(k.q.2.1$dens)

m.q.3.0 = sum(Y.q.3.0 * k.q.3.0$dens)/sum(k.q.3.0$dens)
m.q.3.1 = sum(Y.q.3.1 * k.q.3.1$dens)/sum(k.q.3.1$dens)

m.q.4.0 = sum(Y.q.4.0 * k.q.4.0$dens)/sum(k.q.4.0$dens)
m.q.4.1 = sum(Y.q.4.1 * k.q.4.1$dens)/sum(k.q.4.1$dens)

m.q.5.0 = sum(Y.q.5.0 * k.q.5.0$dens)/sum(k.q.5.0$dens)
m.q.5.1 = sum(Y.q.5.1 * k.q.5.1$dens)/sum(k.q.5.1$dens)

for (i in 1:5){
  assign(paste0("ate.q.", i), get(paste0("m.q.", i, ".1")) - get(paste0("m.q.", i, ".0")))
}

cates = cbind(ate.q.1, ate.q.2, ate.q.3, ate.q.4, ate.q.5)

save(cates, file = "./output/results/nonparametric/cates_M.RData")