load("C:/Users/johan/Documents/GitHub/advanced-econometrics/output/results/lasso/lasso_output.RData") 
load("C:/Users/johan/Documents/GitHub/advanced-econometrics/output/results/nonparametric/kernel_model_output.RData") 
load("C:/Users/johan/Documents/GitHub/advanced-econometrics/output/results/nonparametric/kernel_output_JC.RData") 
load("C:/Users/johan/Documents/GitHub/advanced-econometrics/output/results/nonparametric/kernel_output_v1_CATE.RData") 



kernel_ATE <- model.pl_nonpara$xcoef[[1]] #COEFF
kernel_SE <- model.pl_nonpara$xcoeferr[[1]] #SE

kernel_output[1,1]<- model.pl_nonpara$xcoef[[1]]
kernel_output[1,2]<- model.pl_nonpara$xcoeferr[[1]]


CIu <- kernel_output[,1]+(1.96*kernel_output[,2])
CIl <- kernel_output[,1]-(1.96*kernel_output[,2])

kernel_output <- cbind(kernel_output,CIl,CIu)
save(kernel_output, file = "./output/results/nonparametric/kernel_output_JC.RData")



#lasso_output[[2]][,2]

CIu <- lasso_output[[2]][,1]+(1.96*lasso_output[[2]][,2])
CIl <- lasso_output[[2]][,1]-(1.96*lasso_output[[2]][,2])

lasso_output[[2]] <- cbind(lasso_output[[2]],CIl,CIu)
save(kernel_output, file = "./output/results/lasso/lasso_output.RData")
