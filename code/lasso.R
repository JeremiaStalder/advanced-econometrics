#lasso with glmnet with all vars
#choose vars, interpret vars (but biased)
#normal OLS compare with lasso 

lasso.cv <- cv.glmnet(X"(including D)", Y, type.measure = "mse", family = "gaussian", nfolds = 5, alpha = 1) #кросс-валидация

lambdas <- cbind(c(lasso.cv$lambda),as.numeric(c(lasso.cv$nzero)))
rownames(lambdas) <- lambdas[,2]
lambdas <- as.data.frame(t(lambdas[,-2]))
lambda2 <- lambdas$`2`
lambda2


coef_lasso1 <- coef(lasso.cv, s = lambda2) # save for later comparison
print(coef_lasso1)
coef_lasso1[which(coef_lasso1 != 0 ) ]
coef_lasso1@Dimnames[[1]][which(coef_lasso1 != 0 ) ]