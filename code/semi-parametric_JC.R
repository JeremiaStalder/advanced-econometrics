#######################################################################################################################
# Project - Advanced Econometric Methods
# Erik Senn, Johannes Cordier, Mila Gorkun-Voevoda, Davia Kündig and Jeremia Stalder
#
# Description:
# Semi-Parametric estimation
# For computation time reason we calculate the Semi-Parametric Estimation only with one non-parametric parameter.
# However, in "semi-parametric_JC.R" you can set it to two or three non-parametric parameters (option at the top of the file, might take up to 36h)
# Estimated computation time: around one hour

# ------------------------- Libraries ---------------------------------
library(ATE)
library(bbemkr)
library(BNSP)
library(tidyverse)
library(factoextra)
library(np)	

# ---------------------------- Option ---------------------------------
# Set the number of non-parametric parameters (1,2 or 3):
# 1: inc_std non-parametric
# 2: inc_std and age_std non-parametric (takes around 5h)
# 3: inc_std, age_std and fsize_std non-parametric (takes around 36h)

option <- 1

# --------------------------------------------------------------------


load("./output/mydata_transform.Rdata")
load("./output/variable_sets_modelling.Rdata")

#Data prep
lassodata <- mydata_transform
#Xvars <- c(variable_sets_modelling[["independent_vars_std"]][1:(length(variable_sets_modelling[["independent_vars_std"]])-2)])
X <- select(lassodata, c(variable_sets_modelling[["independent_vars_std"]]))
X <- select(X, -c("e401_std"))
D <- select(lassodata, c("e401"))
Y <- select(lassodata, c("tw_adjust_original"))
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

semi_kernel <- function(Y,D,X){
  non_para_data <- cbind(Y,D,X)
  
  if (option == 1) {
    model.pl_nonpara <- npplreg(tw_adjust_original ~ e401 +
                                age_std + 
                                fsize_std +                      
                                educ_std + 
                                db_std +                          
                                marr_std +                       
                                male_std + 
                                twoearn_std +                                           
                                pira_std + 
                                hs_std +                          
                                smcol_std +                      
                                col_std + 
                                hown_std + 
                                i2_std +                         
                                i3_std +                          
                                i4_std +                          
                                i5_std +                         
                                i6_std +                          
                                i7_std +                          
                                a2_std +                         
                                a3_std +                          
                                a4_std +                          
                                a5_std +                         
                                withdrawal_std +                  
                                other_assets_std +                
                                working_couple_std +             
                                fsize_2_std +                     
                                fsize_3_std +                     
                                fsize_4_std +                    
                                fsize_5_or_above_std +            
                                hmort_dummy_std +                 
                                hval_dummy_right_censored_std +  
                                hmort_dummy_right_censored_std +  
                                hequity_dummy_right_censored_std + 
                                age_sq_std +                     
                                age_cub_std | inc_std,
                              data = non_para_data)
  } else if (option == 2) {
  model.pl_nonpara <- npplreg(tw_adjust_original ~ e401 +
                                fsize_std +                      
                                educ_std + 
                                db_std +                          
                                marr_std +                       
                                male_std + 
                                twoearn_std +                                           
                                pira_std + 
                                hs_std +                          
                                smcol_std +                      
                                col_std + 
                                hown_std + 
                                i2_std +                         
                                i3_std +                          
                                i4_std +                          
                                i5_std +                         
                                i6_std +                          
                                i7_std +                          
                                a2_std +                         
                                a3_std +                          
                                a4_std +                          
                                a5_std +                         
                                withdrawal_std +                  
                                other_assets_std +                
                                working_couple_std +             
                                fsize_2_std +                     
                                fsize_3_std +                     
                                fsize_4_std +                    
                                fsize_5_or_above_std +            
                                hmort_dummy_std +                 
                                hval_dummy_right_censored_std +  
                                hmort_dummy_right_censored_std +  
                                hequity_dummy_right_censored_std | age_std + inc_std,
                              data = non_para_data)
  } else {
  model.pl_nonpara <- npplreg(tw_adjust_original ~ e401 +
                                age_std + 
                                educ_std + 
                                db_std +                          
                                marr_std +                       
                                male_std + 
                                twoearn_std +                                           
                                pira_std + 
                                hs_std +                          
                                smcol_std +                      
                                col_std + 
                                hown_std + 
                                i2_std +                         
                                i3_std +                          
                                i4_std +                          
                                i5_std +                         
                                i6_std +                          
                                i7_std +                          
                                a2_std +                         
                                a3_std +                          
                                a4_std +                          
                                a5_std +                         
                                withdrawal_std +                  
                                other_assets_std +                
                                working_couple_std +             
                                hmort_dummy_std +                 
                                hval_dummy_right_censored_std +  
                                hmort_dummy_right_censored_std +  
                                hequity_dummy_right_censored_std | age_std + inc_std + fsize_std,
                              data = non_para_data)
  }
  kernel_ATE <- model.pl_nonpara$xcoef[[1]] #COEFF
  kernel_SE <- model.pl_nonpara$xcoeferr[[1]] #SE
  coef(model.pl_nonpara, errors = TRUE)
  
  output_ker <- c(kernel_ATE, kernel_SE)
  
  return(output_ker)
}


########CATE KERNEL####################################

semi_kernel_cate <- function(Y,D,X,C){#Y = outcome, X=covariates  D= e401, C= conditional varibale
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
  
  
  # for (n in 1:5) {
  #   print(n)
  #   dat_n <- dat[which(dat$quintile == n),]
  #   #seperate datasets
  #   Y_cate <- select(dat_n, all_of(Y_vars))
  #   X_cate <- select(dat_n, all_of(X_vars))
  #   D_cate <- select(dat_n, all_of(D_vars))
  #   output <- semi_kernel(Y_cate, D_cate ,X_cate)
  #   
  #   output_matrix[n+1,] <- output
  #   
  # }
  # 
  #ATE
  output_ate <- semi_kernel(Y,D,X)
  output_matrix[1,] <- output_ate
  list(output_matrix,output_ate)
  return(output_matrix)
}
###########################

kernel_output <- semi_kernel_cate(Y,D,X,C)
semi_table <- kernel_output

#recaleing
# mu <- mean(mydata_transform$e401)
# sd <- sd(mydata_transform$e401)
# 
# 
# semi_table[,1] <- (semi_table[,1]*sd)+ mu
# semi_table[,2] <- semi_table[,2]*sd

#Confidence Intervals
CIu <- semi_table[,1]+(1.96*semi_table[,2])
CIl <- semi_table[,1]-(1.96*semi_table[,2])

semi_table<- cbind(semi_table,CIl,CIu)


save(semi_table, file = "./output/results/semiparametric/semiparametric_output_JC.RData")
print("Results of Semi-Parametric estimation:")
print(semi_table)
