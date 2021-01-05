library(ATE)
library (BNSP)
library(bbemkr)
library(tidyverse)
library(factoextra)
library(np)	


setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics")
load("./output/mydata_transform.Rdata")
load("./output/variable_sets_modelling.Rdata")

#Data prep
lassodata <- mydata_transform
#Xvars <- c(variable_sets_modelling[["independent_vars_std"]][1:(length(variable_sets_modelling[["independent_vars_std"]])-2)])
X <- select(lassodata, c(variable_sets_modelling[["independent_vars_std"]]))
X <- select(X, -c("e401_std"))
D <- select(lassodata, c("e401"))
Y <- select(lassodata, c("tw_adjust_std"))
C <- select(lassodata, c("inc_quintile"))

#PCA
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

#pca_X <- pcatr(X)[,1:2]


##################KERNEL#########################

kernel <- function(Y,D,X){
X <- pcatr(X)[,1:3]
non_para_data <- cbind(Y,D,X)

colnames(non_para_data)[3:5] <- c("dim1","dim2","dim3")


model.pl_nonpara <- npplreg(tw_adjust_std ~ e401
                            | dim1 + dim2 + dim3,
                            data = non_para_data)

kernel_ATE <- model.pl_nonpara$xcoef[[1]] #COEFF
kernel_SE <- model.pl_nonpara$xcoeferr[[1]] #SE
coef(model.pl_nonpara, errors = TRUE)

output_ker <- c(kernel_ATE, kernel_SE)

return(output_ker)
}


########CATE KERNEL####################################

kernel_cate <- function(Y,D,X,C){#Y = outcome, X=covariates  D= e401, C= conditional varibale
  output_matrix <- matrix(NA, 6, 2)
  colnames(output_matrix) <- c("Coef","SE")
  rownames(output_matrix) <- c("ATE", "CATE q1", "CATE q2", "CATE q3", "CATE q4", "CATE q5")
  names_list <- list(1,2,3,4,5,6)
  X_vars <- colnames(X)
  Y_vars <- colnames(Y)
  D_vars <- colnames(D)
  #Combine the three dataframes
  dat <- as.data.frame(cbind(Y,D,X,C))
  colnames(dat)[ncol(dat)] <- "quintile"
  
  
  for (n in 1:5) {

    dat_n <- dat[which(dat$quintile == n),]
    #seperate datasets
    Y_cate <- select(dat_n, all_of(Y_vars))
    X_cate <- select(dat_n, all_of(X_vars))
    D_cate <- select(dat_n, all_of(D_vars))
    output <- kernel(Y_cate, D_cate ,X_cate)
    
    output_matrix[n+1,] <- output
    
  }
  
  #ATE
  output_ate <- kernel(Y,D,X)
  output_matrix[1,] <- output_ate
  
  return(output_matrix)
}
###########################

###Output####
kernel_output <- kernel_cate(Y,D,X,C)
non_table <- kernel_output

#recaleing
#mu <- mean(mydata_transform$tw_adjust_original)
#sd <- sd(mydata_transform$tw_adjust_original)


#non_table[,1] <- (non_table[,1]*sd)+ mu
#non_table[,2] <- non_table[,2]*sd

#Confidence Intervals
CIu <- non_table[,1]+(1.96*non_table[,2])
CIl <- non_table[,1]-(1.96*non_table[,2])

non_table<- cbind(non_table,CIl,CIu)


####Save output####
save(non_table, file = "./output/results/nonparametric/non-parametric_output_JC.RData")
