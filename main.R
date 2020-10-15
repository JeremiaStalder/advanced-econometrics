### Project Pension - Advanced Econometrics Methods, HSG

# librarys
library(readr)
library(zoo)
library(stats)
library(TSPred)
library(forecast)
library(lubridate)
library(data.table)
library(tidyverse)
library(hdm)
library(GGally)
library(ggcorrplot)

filter <- dplyr::filter
select <- dplyr::select
ggpairs <- GGally::ggpairs
ggcorrplot <- ggcorrplot::ggcorrplot

setwd("C:/Users/eriks/Desktop/MiQE_F/Advanced Econometrics Methods/project") # setwd
# source("functions.R") # functions
outpath = "./output/" # output
datapath = "./data/" # data files (input datafile from package)

# Import Data
View(data("pension"))
mydata <- as.data.frame(pension)

# Descriptives
colnames(mydata)
key_vars=c("p401","e401","a401","tw")
financial_vars = c("inc","tfa","ira","net_tfa","nifa","net_nifa","net_n401")
other_controls_with_key_vars = c("age","fsize","marr","db","hown","educ","male","twoearn","hmort","hequity","hval")
# question: what are the i1-7 and a1-7 variables?
summary(mydata)

for (i in 1:ncol(mydata)){
  png(file=paste0(outpath,"hist_",colnames(mydata)[i],".png"),
      width=600, height=350)
  hist(mydata[,i], main = paste("Hist for",colnames(mydata)[i]))
  dev.off()
}

summary(is.na(mydata))

# Correlations
ggpairs(mydata[,key_vars])+
  ggtitle(paste("Correlations Variables - Key Vars"))
  ggsave(file=paste0(outpath,"corr_plot_key_vars",".png"),  width=6, height=4, dpi=300)

ggpairs(mydata[,c(key_vars, financial_vars)])+
  ggtitle(paste("Correlations Variables - Financial Vars") )
  ggsave(file=paste0(outpath,"corr_plot_key_vars",".png"),  width=6, height=4, dpi=300)

ggpairs(mydata[,c(key_vars,other_controls_with_key_vars)])+
  ggtitle(paste("Correlations Variables - Other Control Vars") )
  ggsave(file=paste0(outpath,"corr_plot_control_vars",".png"),  width=6, height=4, dpi=300)

ggcorrplot(cor(mydata[,c(key_vars,financial_vars,other_controls_with_key_vars)], method="pearson"), tl.cex=6) +
  ggtitle(paste("Corr Matrix"))
 ggsave(file=paste0(outpath,"corr_matrix_all",".png"),  width=6, height=4, dpi=300)

 