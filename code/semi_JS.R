library(np)

load("./output/mydata_transform.Rdata")
load("./output/variable_sets_modelling.Rdata")


data <- mydata_transform
names <- variable_sets_modelling


d_name <- "e401"
d <- as.matrix(data[,d_name])
x_name <- as.vector(names$independent_vars_selection[names$independent_vars_selection!="e401"])
x <- as.matrix(data[,x_name]) 
y_name <- "tw_adjust"  
y <- as.matrix(data[,y_name]) 
colnames(d)[1] <- "d"

model.pl <- npplreg(y ~ d + male +
                      hs +
                      marr + 
                      fsize +
                      db + 
                      hown +
                      educ +
                      twoearn +
                      hmort + 
                      hequity +
                      hval | age + inc,
                    data = cbind(y,d,data))
summary(model.pl)