library(dplyr)
library(np)

load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")

source("./code/non_para_M_functions.R")


npdata = mydata_transform
Y = npdata$tw_adjust_original
indep.vars = variable_sets_modelling[["independent_vars_std"]]

X = dplyr::select(npdata, all_of(indep.vars))

#### Data preparation ####
## divide the data into two samples - where treatment equal to 0 and to 1
Y.0 = npdata[npdata[, "e401_std"] == min(X[, "e401_std"]),]$tw_adjust_original
Y.1 = npdata[npdata[, "e401_std"] == max(X[, "e401_std"]),]$tw_adjust_original

X.0 = X[X[, "e401_std"] == min(X[, "e401_std"]),]
X.1 = X[X[, "e401_std"] == max(X[, "e401_std"]),]

X.0 = dplyr::select(X.0, -c("e401_std"))
X.1 = dplyr::select(X.1, -c("e401_std"))

#### PCA ####
n_pca = 3 # number of principal components to use

PC.0 = pcatr(X.0)[, 1:n_pca]
PC.1 = pcatr(X.1)[, 1:n_pca]


#### Bandwidth estimation ####

load("./output/np M interim/h_vector_0.Rdata")
load("./output/np M interim/h_vector_1.Rdata")

# pre-computed bandwidths are loaded to save time
# in order to re-compute the bandwidths uncomment the loop below

# Least squares cross validation:

# this loop computes optimal bandwidths for each variable by LS CV
# example: h.vector.0 contains bandwidths for variables in PC.0

###  ### ### ### ###  ###
### takes time to run ###
###  ### ### ### ###  ###
#for (e in c(0, 1)){
#  assign(paste0("h.vector.", e), numeric(n_pca))
#  hv = get(paste0("h.vector.", e))
#  for (i in seq(1, n_pca)){
#    variable = get(paste0("PC.", e))[, paste0("V", i)]
#    assign(paste0("h.cv.", e, ".", i), npudensbw(variable, bwmethod="cv.ls"))
#    if (e == 0){
#      h.vector.0[i] = get(paste0("h.cv.0.", i))$bw
#    }
#    if (e == 1){
#      h.vector.1[i] = get(paste0("h.cv.1.", i))$bw
#    }
#  }
#}

#### Grid ####
## generate matrix x_p with grid for all of the variables to use in the kernel estimation
## grids are created to be from min to max of the variable with n = n

n.0 = nrow(PC.0)
n.1 = nrow(PC.1)

## these lines create grids for each variable (which correspond to principal components)
## of each of the subsamples (PC.0 and PC.1)
## example: grid.0.1 corresponds to the grid of first PC of the 0-subsample

grid.0.1 = seq(min(PC.0$V1), max(PC.0$V1), l = n.0)
grid.0.2 = seq(min(PC.0$V2), max(PC.0$V2), l = n.0)
grid.0.3 = seq(min(PC.0$V3), max(PC.0$V3), l = n.0)

grid.1.1 = seq(min(PC.1$V1), max(PC.1$V1), l = n.1)
grid.1.2 = seq(min(PC.1$V2), max(PC.1$V2), l = n.1)
grid.1.3 = seq(min(PC.1$V3), max(PC.1$V3), l = n.1)

#### Kernel estimation ####
# these lines estimate density for the set of variables used for each of the 0-1-subsamples
# example: k.t.1.0 is the density object for variable 1 in PC.0 (first principal component)
# joint kernel is computed as a product kernel

# individual kernels

k.t.1.0 = npudens(bws = h.vector.0[1], tdat = grid.0.1 - PC.0$V1)
k.t.2.0 = npudens(bws = h.vector.0[2], tdat = grid.0.2 - PC.0$V2)
k.t.3.0 = npudens(bws = h.vector.0[3], tdat = grid.0.3 - PC.0$V3)
k.t.0 = k.t.1.0$dens * k.t.2.0$dens * k.t.3.0$dens

k.t.1.1 = npudens(bws = h.vector.1[1], tdat = grid.1.1 - PC.1$V1)
k.t.2.1 = npudens(bws = h.vector.1[2], tdat = grid.1.2 - PC.1$V2)
k.t.3.1 = npudens(bws = h.vector.1[3], tdat = grid.1.3 - PC.1$V3)
k.t.1 = k.t.1.1$dens * k.t.2.1$dens * k.t.3.1$dens


#### Nadaraya-Watson estimator ####
# m^hat(x) = ( (\sum(y_i* K))/ (\sum(K)))

m.0 = sum(Y.0 * k.t.0)/sum(k.t.0)
m.1 = sum(Y.1 * k.t.1)/sum(k.t.1)

ate = m.1 - m.0

print(ate)
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

load("./output/np M interim/h_vector_q_1_0.Rdata")
load("./output/np M interim/h_vector_q_1_1.Rdata")
load("./output/np M interim/h_vector_q_2_0.Rdata")
load("./output/np M interim/h_vector_q_2_1.Rdata")
load("./output/np M interim/h_vector_q_3_0.Rdata")
load("./output/np M interim/h_vector_q_3_1.Rdata")
load("./output/np M interim/h_vector_q_4_0.Rdata")
load("./output/np M interim/h_vector_q_4_1.Rdata")
load("./output/np M interim/h_vector_q_5_0.Rdata")
load("./output/np M interim/h_vector_q_5_1.Rdata")

# pre-computed bandwidths are loaded to save time
# in order to re-compute the bandwidths uncomment the loop below

# this loop computes optimal bandwidths for each variable by LS CV
# example: h.vector.q.1.0 contains bandwidths for variables in PC.q.1.0
# takes time to run

# for (i in 1:5){
#   for (e in c(0, 1)){
#     assign(paste0("h.vector.q.", i, ".", e), numeric(n_pca))
#     hv = get(paste0("h.vector.q.", i, ".", e))
#     for (j in seq(1, n_pca)){
#       variable = get(paste0("PC.q.", i, ".", e))[, paste0("V", j)]
#       assign(paste0("h.cv.q.", i, ".", e, ".", j), npudensbw(variable, bwmethod="cv.ls"))
#       if (e == 0){
#         if (i == 1){
#           h.vector.q.1.0[j] = get(paste0("h.cv.q.1.0.", j))$bw
#         }
#         if (i == 2){
#           h.vector.q.2.0[j] = get(paste0("h.cv.q.2.0.", j))$bw
#         }
#         if (i == 3){
#           h.vector.q.3.0[j] = get(paste0("h.cv.q.3.0.", j))$bw
#         }
#         if (i == 4){
#           h.vector.q.4.0[j] = get(paste0("h.cv.q.4.0.", j))$bw
#         }
#         if (i == 5){
#           h.vector.q.5.0[j] = get(paste0("h.cv.q.5.0.", j))$bw
#         }
#       }
#       if (e == 1){
#         if (i == 1){
#           h.vector.q.1.1[j] = get(paste0("h.cv.q.1.1.", j))$bw
#         }
#         if (i == 2){
#           h.vector.q.2.1[j] = get(paste0("h.cv.q.2.1.", j))$bw
#         }
#         if (i == 3){
#           h.vector.q.3.1[j] = get(paste0("h.cv.q.3.1.", j))$bw
#         }
#         if (i == 4){
#           h.vector.q.4.1[j] = get(paste0("h.cv.q.4.1.", j))$bw
#         }
#         if (i == 5){
#           h.vector.q.5.1[j] = get(paste0("h.cv.q.5.1.", j))$bw
#         }
#       }
#     }
#   }
# }

### Grid ###
## generate matrix x_p with grid for all of the variables to use in the kernel estimation
## grids are created to be from min to max of the variable with n = n

for (i in 1:5){
  assign(paste0("n.q.", i, ".0"), nrow(get(paste0("PC.q.", i, ".0"))))
  assign(paste0("n.q.", i, ".1"), nrow(get(paste0("PC.q.", i, ".1"))))
}


## this loop creates grids for each variable for each quintile
## of each of the subsamples (PC.0 and PC.1)
## example: grid.q.2.0.1 corresponds to the grid of first PC of the 0-subsample
## of the second quintile

for (i in 1:5){
  for (j in seq(1, n_pca)){
    variable = get(paste0("PC.q.", i, ".0"))[, paste0("V", j)]
    assign(paste0("grid.q.", i ,".0.", j), seq(min(variable), 
                                               max(variable), l = get(paste0("n.q.", i, ".0"))))
    
    variable = get(paste0("PC.q.", i, ".1"))[, paste0("V", j)]
    assign(paste0("grid.q.", i ,".1.", j), seq(min(variable), 
                                               max(variable), l = get(paste0("n.q.", i, ".1"))))
  }
}


### Kernel
# kernel densities are computed the same way as in the previous section

k.q.1.0.1 = npudens(bws = h.vector.q.1.0[1], tdat = grid.q.1.0.1 - PC.q.1.0$V1)
k.q.1.0.2 = npudens(bws = h.vector.q.1.0[2], tdat = grid.q.1.0.2 - PC.q.1.0$V2)
k.q.1.0.3 = npudens(bws = h.vector.q.1.0[3], tdat = grid.q.1.0.3 - PC.q.1.0$V3)
k.q.1.0 = k.q.1.0.1$dens * k.q.1.0.2$dens * k.q.1.0.3$dens

k.q.1.1.1 = npudens(bws = h.vector.q.1.1[1], tdat = grid.q.1.1.1 - PC.q.1.1$V1)
k.q.1.1.2 = npudens(bws = h.vector.q.1.1[2], tdat = grid.q.1.1.2 - PC.q.1.1$V2)
k.q.1.1.3 = npudens(bws = h.vector.q.1.1[3], tdat = grid.q.1.1.3 - PC.q.1.1$V3)
k.q.1.1 = k.q.1.1.1$dens * k.q.1.1.2$dens * k.q.1.1.3$dens

k.q.2.0.1 = npudens(bws = h.vector.q.2.0[1], tdat = grid.q.2.0.1 - PC.q.2.0$V1)
k.q.2.0.2 = npudens(bws = h.vector.q.2.0[2], tdat = grid.q.2.0.2 - PC.q.2.0$V2)
k.q.2.0.3 = npudens(bws = h.vector.q.2.0[3], tdat = grid.q.2.0.3 - PC.q.2.0$V3)
k.q.2.0 = k.q.2.0.1$dens * k.q.2.0.2$dens * k.q.2.0.3$dens

k.q.3.0.1 = npudens(bws = h.vector.q.3.0[1], tdat = grid.q.3.0.1 - PC.q.3.0$V1)
k.q.3.0.2 = npudens(bws = h.vector.q.3.0[2], tdat = grid.q.3.0.2 - PC.q.3.0$V2)
k.q.3.0.3 = npudens(bws = h.vector.q.3.0[3], tdat = grid.q.3.0.3 - PC.q.3.0$V3)
k.q.3.0 = k.q.3.0.1$dens * k.q.3.0.2$dens * k.q.3.0.3$dens

k.q.4.0.1 = npudens(bws = h.vector.q.4.0[1], tdat = grid.q.4.0.1 - PC.q.4.0$V1)
k.q.4.0.2 = npudens(bws = h.vector.q.4.0[2], tdat = grid.q.4.0.2 - PC.q.4.0$V2)
k.q.4.0.3 = npudens(bws = h.vector.q.4.0[3], tdat = grid.q.4.0.3 - PC.q.4.0$V3)
k.q.4.0 = k.q.4.0.1$dens * k.q.4.0.2$dens * k.q.4.0.3$dens

k.q.5.0.1 = npudens(bws = h.vector.q.5.0[1], tdat = grid.q.5.0.1 - PC.q.5.0$V1)
k.q.5.0.2 = npudens(bws = h.vector.q.5.0[2], tdat = grid.q.5.0.2 - PC.q.5.0$V2)
k.q.5.0.3 = npudens(bws = h.vector.q.5.0[3], tdat = grid.q.5.0.3 - PC.q.5.0$V3)
k.q.5.0 = k.q.5.0.1$dens * k.q.5.0.2$dens * k.q.5.0.3$dens



k.q.1.1.1 = npudens(bws = h.vector.q.1.1[1], tdat = grid.q.1.1.1 - PC.q.1.1$V1)
k.q.1.1.2 = npudens(bws = h.vector.q.1.1[2], tdat = grid.q.1.1.2 - PC.q.1.1$V2)
k.q.1.1.3 = npudens(bws = h.vector.q.1.1[3], tdat = grid.q.1.1.3 - PC.q.1.1$V3)
k.q.1.1 = k.q.1.1.1$dens * k.q.1.1.2$dens * k.q.1.1.3$dens

k.q.2.1.1 = npudens(bws = h.vector.q.2.1[1], tdat = grid.q.2.1.1 - PC.q.2.1$V1)
k.q.2.1.2 = npudens(bws = h.vector.q.2.1[2], tdat = grid.q.2.1.2 - PC.q.2.1$V2)
k.q.2.1.3 = npudens(bws = h.vector.q.2.1[3], tdat = grid.q.2.1.3 - PC.q.2.1$V3)
k.q.2.1 = k.q.2.1.1$dens * k.q.2.1.2$dens * k.q.2.1.3$dens

k.q.3.1.1 = npudens(bws = h.vector.q.3.1[1], tdat = grid.q.3.1.1 - PC.q.3.1$V1)
k.q.3.1.2 = npudens(bws = h.vector.q.3.1[2], tdat = grid.q.3.1.2 - PC.q.3.1$V2)
k.q.3.1.3 = npudens(bws = h.vector.q.3.1[3], tdat = grid.q.3.1.3 - PC.q.3.1$V3)
k.q.3.1 = k.q.3.1.1$dens * k.q.3.1.2$dens * k.q.3.1.3$dens

k.q.4.1.1 = npudens(bws = h.vector.q.4.1[1], tdat = grid.q.4.1.1 - PC.q.4.1$V1)
k.q.4.1.2 = npudens(bws = h.vector.q.4.1[2], tdat = grid.q.4.1.2 - PC.q.4.1$V2)
k.q.4.1.3 = npudens(bws = h.vector.q.4.1[3], tdat = grid.q.4.1.3 - PC.q.4.1$V3)
k.q.4.1 = k.q.4.1.1$dens * k.q.4.1.2$dens * k.q.4.1.3$dens

k.q.5.1.1 = npudens(bws = h.vector.q.5.1[1], tdat = grid.q.5.1.1 - PC.q.5.1$V1)
k.q.5.1.2 = npudens(bws = h.vector.q.5.1[2], tdat = grid.q.5.1.2 - PC.q.5.1$V2)
k.q.5.1.3 = npudens(bws = h.vector.q.5.1[3], tdat = grid.q.5.1.3 - PC.q.5.1$V3)
k.q.5.1 = k.q.5.1.1$dens * k.q.5.1.2$dens * k.q.5.1.3$dens

### N-W estimator

m.q.1.0 = sum(Y.q.1.0 * k.q.1.0)/sum(k.q.1.0)
m.q.1.1 = sum(Y.q.1.1 * k.q.1.1)/sum(k.q.1.1)

m.q.2.0 = sum(Y.q.2.0 * k.q.2.0)/sum(k.q.2.0)
m.q.2.1 = sum(Y.q.2.1 * k.q.2.1)/sum(k.q.2.1)

m.q.3.0 = sum(Y.q.3.0 * k.q.3.0)/sum(k.q.3.0)
m.q.3.1 = sum(Y.q.3.1 * k.q.3.1)/sum(k.q.3.1)

m.q.4.0 = sum(Y.q.4.0 * k.q.4.0)/sum(k.q.4.0)
m.q.4.1 = sum(Y.q.4.1 * k.q.4.1)/sum(k.q.4.1)

m.q.5.0 = sum(Y.q.5.0 * k.q.5.0)/sum(k.q.5.0)
m.q.5.1 = sum(Y.q.5.1 * k.q.5.1)/sum(k.q.5.1)

for (i in 1:5){
  assign(paste0("ate.q.", i), get(paste0("m.q.", i, ".1")) - get(paste0("m.q.", i, ".0")))
}

cates = cbind(ate.q.1, ate.q.2, ate.q.3, ate.q.4, ate.q.5)
print(cates)

save(cates, file = "./output/results/nonparametric/cates_M.RData")