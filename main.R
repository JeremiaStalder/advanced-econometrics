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

# setwd("C:/Users/eriks/Desktop/MiQE_F/Advanced Econometrics Methods/project") # setwd
setwd("C:/Users/johan/Documents/GitHub/advanced-econometrics") # setwd
# source("functions.R") # functions


outpath = "./output/" # output
datapath = "./data/" # data files (input datafile from package)

# Import Data
View(data("pension"))
mydata <- as.data.frame(pension)

# Data overview 
  # Descriptives
  colnames(mydata)
  key_vars=c("p401","e401","a401","tw")
  financial_vars = c("inc","tfa","tfa_he","ira","net_tfa","nifa","net_nifa","net_n401")
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
  
   inc = log(mydata$inc)
   
  # Plausibility Checks
    # home value variables
     mydata$eq_check = -mydata$hmort+mydata$hval
     print("Home Equity Check")
     sum(mydata$eq_check==mydata$hequity)
     sum(mydata$eq_check!=mydata$hequity)
     
     hown_check = mydata$hval > 0 
     print("Home Owner Check")
     sum(hown_check==mydata$hown)
     sum(hown_check!=mydata$hown)
     
   # education dummies
     educ_dummy_check = mydata$col+ mydata$smcol + mydata$hs + mydata$nohs
     print("All education Dummys sum up to 1?")
     mean(educ_dummy_check)
     
     educ_dummy_check2 = mydata$educ<12
     print("Non-High-School Dummy constructed via education years")
     all(educ_dummy_check2==mydata$nohs)
   
 ######## TO DO, wealth vars didnt work out yet ############ 
    # replicate fta
     mydata$tfa_check = mydata$a401+mydata$nifa+mydata$ira
     sum(mydata$tfa_check==mydata$tfa)
     sum(mydata$tfa_check!=mydata$tfa)
     
    # replicate net fin assets
     mydata$nifa_check = mydata$a401 + mydata$net_n401 
     hist(mydata$nifa_check-mydata$nifa)
     sum(mydata$nifa_check==mydata$nifa)
     sum(mydata$nifa_check!=mydata$nifa)
     
    # replicate total wealth: not possible since value of bus,property = value_other etc is missing. Can generate it
     mydata$value_other = mydata$tw - mydata$nifa - mydata$hequity
     sum(mydata$tw_check==mydata$tfa)
     sum(mydata$tw_check!=mydata$tfa)
############################################
   
 #### Data Transformation ####
 mydata_transform <- mydata
   
  # Remove observations and invalid values
  # drop observations with negative income (only 2 obs, therefore drop outliers to avoid fitting to them)
   mydata_transform <- filter(mydata_transform, inc >= 0) 
 
  # New variables
   # family variable
   mydata_transform$busy_couple = as.numeric(mydata_transform$fsize==2 & mydata_transform$twoearn==1)
   
  # Variable transformations
    # Dummies for cutoffs
     mydata_transform = mutate(mydata_transform, hmort_dummy = hmort>0) # mortgage dummy
     mydata_transform = mutate(mydata_transform, hequity_dummy_neg = hequity<0) # hequity dummy negative
     mydata_transform = mutate(mydata_transform, hequity_dummy = hequity>0) # hequity dummy
     
     # Standardize continuous variables.
       # ! Hint: need to do all "content-related" variable transformations before this step. 
       # ! Hint2: add newly created continuous variables to the vector
       continuous_vars = c("a401","tw","tfa","ira","net_tfa","tfa_he", "nifa","net_nifa","net_n401","inc","hmort","hequity","hval")
       mydata_transform[,continuous_vars] = scale(mydata_transform[,continuous_vars])
      
  # Drop Variables
   mydata_transform = select(mydata_transform, -zhat)    # remove IV variable
   mydata_transform = select(mydata_transform, -nohs)  # no high school as reference category 
   
   
