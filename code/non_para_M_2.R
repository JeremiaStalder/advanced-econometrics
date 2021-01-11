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


#### Bandwidth estimation ####
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
# this loop estimates density for the set of variables used for each of the 0-1-subsamples
# example: k.0 is the density object for variables in PC.0

for (e in c(0, 1)){
  assign(paste0("k.", e), npudens(bws = get(paste0("h.vector.", e)), 
                                  tdat = get(paste0("PC.", e))))
}


#### Nadaraya-Watson estimator ####
# m^hat(x) = ( (\sum(y_i* K))/ (\sum(K)))

m.0 = sum(Y.0 * k.0$dens)/sum(k.0$dens)
m.1 = sum(Y.1 * k.1$dens)/sum(k.1$dens)

ate = m.1 - m.0

save(ate, file = "./output/results/nonparametric/ate_M.RData")


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