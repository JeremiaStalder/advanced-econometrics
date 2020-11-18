library(tidyverse)
library(np)
library(bbemkr)

setwd("C:/Users/lmgv/iCloudDrive/SG MiQEF/3 semester/Advanced Econometric Methods/Project/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_descriptives.Rdata")
load("./output/variable_sets_modelling.Rdata")


npdata <- mydata_transform
Y <- npdata$tw #numeric vector with outcome variable
Ti <- npdata$e401 #Treatment 
indep_vars <- variable_sets_modelling[["independent_vars_selection"]]
squared_vars <- c("age_sq","age_cub","inc_sq","inc_cub")

X <- select(npdata, all_of(indep_vars))
#X <- select(X, -c("e401"))#no treatment
X <- select(X, -squared_vars)#dont need squared variables

Y.0 = npdata[npdata[, "e401"] == 0,]$tw
Y.1 = npdata[npdata[, "e401"] == 1,]$tw

#### Estimate the bandwidth ####
# сначала сделаю по всему датасету, чтобы посмотреть как оно работает. но потом, наверное, надо поделить на 
# два датасета, где x=0 и x=1 и прогнать на этих двух частях по-отдельности
# сначала посмотрю, со сколькими факторами норм по времени
X1 = select(X, c("e401", "age", "inc"))
bw <- npregbw(xdat = X1, ydat = Y)
save(bw, file = "./output/non_para_M_bw.RData")
#dd <- npudens(bws = bw$bw, data = X1)

# теперь на всём датасете (мб отдельно на двух подсэмплах?)
bw.0 <- npregbw(xdat = Xe0, ydat = Y.0, nmulti = 3)
save(bw, file = "./output/non_para_M_bw_Xe0.RData")

bw.1 <- npregbw(xdat = Xe1, ydat = Y.1, nmulti = 3)
save(bw, file = "./output/non_para_M_bw_Xe1.RData")

### теперь попробую сделать PCA до одной переменной
## first divide into two samples - where treatment equal to 0 and to 1
Xe0 = X[X[, "e401"] == 0,]
Xe1 = X[X[, "e401"] == 1,]

Xe0 = select(Xe0, -c("e401"))
Xe1 = select(Xe1, -c("e401"))

## PCA
mean.0 = as.vector(colMeans(Xe0))
m.0    = matrix(mean.0, nrow(Xe0), NROW(mean.0), byrow = T)
x.0    = Xe0 - m.0
eig.0  = eigen(cov(x.0))  # spectral decomposition  
eva.0  = eig.0$values
eve.0  = eig.0$vectors
xm.0   = as.matrix(x.0)
y.0    = xm.0 %*% eve.0
ym.0   = y.0[, 1]       # first eigenvector = first PC

mean.1 = as.vector(colMeans(Xe1))
m.1    = matrix(mean.1, nrow(Xe1), NROW(mean.1), byrow = T)
x.1    = Xe1 - m.1
eig.1  = eigen(cov(x.1))  # spectral decomposition  
eva.1  = eig.1$values
eve.1  = eig.1$vectors
xm.1   = as.matrix(x.1)
y.1    = xm.1 %*% eve.1
ym.1   = y.1[, 1] 

bw.cv.0 = npudensbw(ym.0, bwmethod="cv.ls")	# least squares cv for bw
bw.cv.1 = npudensbw(ym.1, bwmethod="cv.ls")	# least squares cv for bw

#### Kernel Estimation ####
## Nadaraya-Watson
nwk.0 = NadarayaWatsonkernel(ym.0, Y.0, h = bw.cv.0$bw, gridpoint = seq(-3, 3, length.out = 100))
nwk.1 = NadarayaWatsonkernel(ym.1, Y.1, h = bw.cv.1$bw, gridpoint = seq(-3, 3, length.out = 100))
