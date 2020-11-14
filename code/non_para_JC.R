library(ATE)
library (BNSP)

#####Non-Parametric####
npdata <- mydata_transform
Y <- npdata$tw #numeric vector with outcome variable
Ti <- npdata$e401 #Treatment 
X <- select(npdata, all_of(independent_vars_selection))
X <- select(npdata, -c("e401",))#Control variables

#get the initial values why a A numeric vector or matrix of possible initial values for the Newton-Raphson
#algorithm. Must be a J X K matrix where J is the number of treatment arms.
#need starting values

npATE <- ATE (Y, Ti, X, theta = 0, ATT = FALSE,
     verbose = FALSE, max.iter = 100, tol = 1e-10,
     initial.values = NULL,
     backtrack = TRUE, backtrack.alpha = 0.3,
     backtrack.beta = 0.5)

plot(npATE)
summary(npATE)


####Semi-Parametric####

