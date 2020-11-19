library(tidyverse)
library(np)
library(bbemkr)
library(dplyr)
library(ks)

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

X <- dplyr::select(npdata, all_of(indep_vars))
#X <- select(X, -c("e401"))
#X <- dplyr::select(X, -squared_vars) #dont need squared variables


## divide the data into two samples - where treatment equal to 0 and to 1
Y.0 = npdata[npdata[, "e401_std"] == min(X[, "e401_std"]),]$tw_adjust_std
Y.1 = npdata[npdata[, "e401_std"] == max(X[, "e401_std"]),]$tw_adjust_std

X.0 = X[X[, "e401_std"] == min(X[, "e401_std"]),]
X.1 = X[X[, "e401_std"] == max(X[, "e401_std"]),]

#### Estimate the bandwidth ####

# вопрос - нужно ли делать кернел на двух сабсэмплах по-отдельности, или на всем датасете вместе?

## perform PCA first to reduce the number of features
PC.0 = pcatr(X.0)
PC.1 = pcatr(X.1)

## use function npregbw to estimate the optimal bandwidth matrix - using three principal components
## this function takes time to run, so it is commented. The results are loaded from saved RData files

#bw.0 <- npregbw(xdat = PC.0[, 1:3], ydat = Y.0)
#save(bw.0, file = "./output/non_para_M_bw_0.RData")

#bw.1 <- npregbw(xdat = PC.1[, 1:3], ydat = Y.1)
#save(bw.1, file = "./output/non_para_M_bw_1.RData")

load("./output/non_para_M_bw_0.RData")
load("./output/non_para_M_bw_1.RData")

H.0 = Hlscv(PC.0[, 1:3])
H.1 = Hlscv(PC.1[, 1:3])

#### Kernel Estimation ####

kd.0 = kde(PC.0[, 1:3], H = H.0)






# сначала сделаю по всему датасету, чтобы посмотреть как оно работает. но потом, наверное, надо поделить на 
# два датасета, где x=0 и x=1 и прогнать на этих двух частях по-отдельности
# сначала посмотрю, со сколькими факторами норм по времени
X1 = dplyr:: select(X, c("e401", "age", "inc"))
bw <- npregbw(xdat = X1, ydat = Y)
save(bw, file = "./output/non_para_M_bw.RData")
#dd <- npudens(bws = bw$bw, data = X1)


### теперь попробую сделать PCA до одной переменной
## first divide into two samples - where treatment equal to 0 and to 1
Xe0 = X[X[, "e401"] == 0,]
Xe1 = X[X[, "e401"] == 1,]

Xe0 = dplyr::select(Xe0, -c("e401"))
Xe1 = dplyr::select(Xe1, -c("e401"))

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


## Nadaraya-Watson
nwk.0 = NadarayaWatsonkernel(ym.0, Y.0, h = bw.cv.0$bw, gridpoint = seq(-3, 3, length.out = 100))
nwk.1 = NadarayaWatsonkernel(ym.1, Y.1, h = bw.cv.1$bw, gridpoint = seq(-3, 3, length.out = 100))


# использовать стандартизированные данные для bw и kernel
# check if I can get standard errors for the estimator

#### Scatter plot for PCs ####
#mydata_transform_scatter = read.table("./output/mydata_transform_scatter.csv", sep = ",", header = TRUE)

#X.scatter <- dplyr::select(mydata_transform_scatter, -c("tw_adjust_original"))
#PC.scatter = pcatr(X.scatter)

#PC.scatter["tw"] <- mydata_transform_scatter["tw_adjust_original"]

#ggplot(PC.scatter, aes(x = V1, y = V2, color = tw)) + geom_point() + 
#  scale_color_gradientn(colours = rainbow(5)) + 
#  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
#        panel.grid.major = element_line(colour = "lightgrey"),
#        panel.grid.minor = element_line(colour = "lightgrey")) +
#  xlab("PC 1") +
#  ylab("PC 2") +
#  ggtitle("First two principal components")

#ggsave("./output/descriptives/paper/first_two_PCs.png")
