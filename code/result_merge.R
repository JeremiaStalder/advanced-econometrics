# ------------------------- Libraries ---------------------------------

library(data.table) # used for transposing some data tables
library(xtable)
library(ggplot2)

# ------------------------- Functions and Variables -------------------
transpose <- data.table::transpose
path = "./output/results/"

# ------------------------- Collection of Results ---------------------

# Load all data
non_para <- get(load("./output/results/nonparametric/non-parametric_output_JC.RData"))
lasso <- get(load("./output/results/lasso/lasso_output_final.RData"))
parametric_all_results <- get(load("./output/results/parametric/parametric_all_results.Rdata"))
semiparametric_output <- get(load("./output/results/semiparametric/semiparametric_output_3nonpara.RData"))
crf_results_all <- get(load("./output/results/random_forest/crf_results_all.Rdata"))
ds_lasso <- get(load("./output/results/lasso/DS_lasso_output_final.RData"))

# Define Names
row_names <- c("ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
col_names <- c("Coef", "SE", "CIl", "CIu")

## Random forest results
crf_data <- transpose(data.table(crf_results_all$tw_adjust_original)) # transpose
crf <- as.data.frame(crf_data[,c(1, 2, 4, 3)])
rownames(crf) <- c("Causal Random Forest ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(crf) <- col_names

## Mean comparison
mean_comparison_data <- data.table::transpose(data.table(parametric_all_results$tw_adjust_original$`Mean Comparison`)) # transpose
mean_comparison <- as.data.frame(mean_comparison_data[,c(1, 2, 4, 3)])
rownames(mean_comparison) <- c("Mean Comparison ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(mean_comparison) <- col_names

## Con. Means
cond_means_data <- transpose(data.table(parametric_all_results$tw_adjust_original$`Cond. Means`)) # transpose
cond_means <- as.data.frame(cond_means_data[,c(1, 2, 4, 3)])
rownames(cond_means) <- c("Linear Conditional Outcome Regression ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(cond_means) <- col_names

## GAM
gam_data <- transpose(data.table(parametric_all_results$tw_adjust_original$GAM)) # transpose
gam <- as.data.frame(gam_data[,c(1, 2, 4, 3)])
rownames(gam) <- c("Generalized Additive Model ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(gam) <- col_names

## Cond_Means_flex
#cond_Means_flex_data <- transpose(data.table(parametric_all_results$tw_adjust_original$Cond_Means_flex)) # transpose
#cond_Means_flex <- as.data.frame(cond_Means_flex_data[,c(1, 2, 4, 3)])
#rownames(cond_Means_flex) <- c("Flexible Linear Conditional Outcome Regression ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
#colnames(cond_Means_flex) <- colnames(non_para)

## IPW
ipw_data <- transpose(data.table(parametric_all_results$tw_adjust_original$IPW)) # transpose
ipw <- as.data.frame(ipw_data[,c(1, 2, 4, 3)])
rownames(ipw) <- c("IPW ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(ipw) <- col_names

## IPW_restricted
ipw_restricted_data <- transpose(data.table(parametric_all_results$tw_adjust_original$IPW_restricted)) # transpose
ipw_restricted <- as.data.frame(ipw_restricted_data[,c(1, 2, 4, 3)])
rownames(ipw_restricted) <- c("IPW Restricted ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(ipw_restricted) <- col_names

## IPW_restricted2
ipw_restricted2_data <- transpose(data.table(parametric_all_results$tw_adjust_original$IPW_restricted2)) # transpose
ipw_restricted2 <- as.data.frame(ipw_restricted2_data[,c(1, 2, 4, 3)])
rownames(ipw_restricted2) <- c("IPW Restricted 2 ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(ipw_restricted2) <- col_names

## Doubly_robust_base
doubly_robust_base_data <- transpose(data.table(parametric_all_results$tw_adjust_original$Doubly_robust_base)) # transpose
doubly_robust_base <- as.data.frame(doubly_robust_base_data[,c(1, 2, 4, 3)])
rownames(doubly_robust_base) <- c("Doubly Robust Parametric ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(doubly_robust_base) <- col_names

## Doubly_robust_restricted
doubly_robust_restricted_data <- transpose(data.table(parametric_all_results$tw_adjust_original$Doubly_robust_restricted)) # transpose
doubly_robust_restricted <- as.data.frame(doubly_robust_restricted_data[,c(1, 2, 4, 3)])
rownames(doubly_robust_restricted) <- c("Doubly Robust Restricted ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(doubly_robust_restricted) <- col_names

## Doubly_robust_restricted2
doubly_robust_restricted2_data <- transpose(data.table(parametric_all_results$tw_adjust_original$Doubly_robust_restricted2)) # transpose
doubly_robust_restricted2 <- as.data.frame(doubly_robust_restricted2_data[,c(1, 2, 4, 3)])
rownames(doubly_robust_restricted2) <- c("Doubly Robust Restricted 2 ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
colnames(doubly_robust_restricted2) <- col_names


rownames(non_para) <- c("Non-Parametric Kernel Regression ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
rownames(semiparametric_output) <- c("Semi-Parametric Regression ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
rownames(lasso) <- c("Lasso ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
rownames(ds_lasso) <- c("Double Selection Lasso ATE", "Q1", "Q2", "Q3", "Q4", "Q5")
all_results <- rbind(mean_comparison, cond_means, ipw, ipw_restricted, ipw_restricted2, doubly_robust_base, doubly_robust_restricted, doubly_robust_restricted2, non_para, semiparametric_output, lasso, ds_lasso, crf)
all_results_quantile <- all_results[c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),]
all_results_ate <- all_results[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),]


print(xtable(all_results), type="latex",paste0(path, "all_results.tex"))
print(xtable(all_results_ate), type="latex",paste0(path, "all_results_ate.tex"))


# ------------------------- Creation of Graphs ---------------------

results_graph <- rbind(mean_comparison, cond_means, non_para, ds_lasso, semiparametric_output, crf)
results_graph_quantile <- results_graph[c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),]
results_graph_ate <- results_graph[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),]

# Only selection of methods
selected_estimators <- data.frame(cate = rep(c("Mean Comparison", "Linear Conditional Outcome Regression", "Non-Parametric Kernel Regression", "Semi-Parametric Regression", "Double Selection Lasso", "Causal Random Forest"), each=5),
                  quant=rep(c("Q1", "Q2", "Q3", "Q4", "Q5"),6),
                  coef = results_graph_quantile[,1],
                  CIl = results_graph_quantile[,3],
                  CIu = results_graph_quantile[,4])

# Confidence Interval with Error Line
ggplot(selected_estimators, aes(x=quant, y=coef, group=cate, color=cate)) + 
  geom_errorbar(aes(ymin=CIl, ymax=CIu), width=.05, position=position_dodge(0.1)) +
  geom_line() +
  scale_color_brewer(palette="Paired")+theme_minimal() + 
  labs(x = "Income Quintile", y = "Treatment Effect", color = "Methods:")

ggsave(paste0(path, "comparison_selection_error_line.png"), width = 25, height = 13, units = "cm")

# Confidence Interval with grey area
ggplot(selected_estimators, aes(x=quant, y=coef, group=cate, color=cate)) + 
  geom_ribbon(aes(ymin=CIl, ymax=CIu), linetype=2, alpha=0.05) +
  geom_line() +
  scale_color_brewer(palette="Paired")+theme_minimal() + 
  labs(x = "Income Quintile", y = "Treatment Effect", color = "Methods:")

ggsave(paste0(path, "comparison_selection_error_grey.png"), width = 25, height = 13, units = "cm")

# All Methods (except cond means flexible and doubly robust restricted 2) without CIs
all_results_no_cond_means_flex <- rbind(mean_comparison, cond_means, doubly_robust_base, non_para, semiparametric_output, lasso, ds_lasso, crf, gam)
# all_results_no_cond_means_flex <- rbind(mean_comparison, cond_means, doubly_robust_base, non_para, semiparametric_output, lasso, ds_lasso, crf)
all_results_quantile <- all_results_no_cond_means_flex[c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),]
# all_estimators <- data.frame(cate = rep(c("Mean Comparison", "Conditional Means", "IPW", "IPW Restricted", "IPW Restricted 2", "Doubly Robust Base","Doubly Robust Restricted", "Nonparametric", "Semiparametric", "Lasso", "Double Selection Lasso", "Causal Random Forest"), each=5),
all_estimators <- data.frame(cate = rep(c("Mean Comparison", "Linear Conditional Outcome Regression", "Doubly Robust Parametric", "Nonparametric", "Semi-Parametric Regression", "Lasso", "Double Selection Lasso", "Causal Random Forest", "Generalized Additive Model"), each=5),
                                  quant=rep(c("Q1", "Q2", "Q3", "Q4", "Q5"),9),
                                  coef = all_results_quantile[,1],
                                  CIl = all_results_quantile[,3],
                                  CIu = all_results_quantile[,4])


ggplot(all_estimators, aes(x=quant, y=coef, group=cate, color=cate)) + 
  geom_line() +
  scale_color_brewer(palette="Paired")+theme_minimal() + 
  labs(x = "Income Quintile", y = "Treatment Effect", color = "Methods:")

  ggsave(paste0(path, "comparison_all_cate.png"), width = 25, height = 13, units = "cm")

# Comparison Parametric
parametric_graph <- rbind(mean_comparison, doubly_robust_base, cond_means, lasso, ds_lasso, gam)
parametric_graph_quantile <- parametric_graph[c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),]

parametric_estimators <- data.frame(cate = rep(c("Mean Comparison", "Doubly Robust Parametric", "Linear Conditional Outcome Regression", "Lasso", "Double Selection Lasso", "Generalized Additive Model"), each=5),
                                  quant=rep(c("Q1", "Q2", "Q3", "Q4", "Q5"),6),
                                  coef = parametric_graph_quantile[,1],
                                  CIl = parametric_graph_quantile[,3],
                                  CIu = parametric_graph_quantile[,4])


# Confidence Interval with grey area
ggplot(parametric_estimators, aes(x=quant, y=coef, group=cate, color=cate)) + 
  geom_ribbon(aes(ymin=CIl, ymax=CIu), linetype=2, alpha=0.05) +
  geom_line() +
  scale_color_brewer(palette="Paired")+theme_minimal() + 
  labs(x = "Income Quintile", y = "Treatment Effect", color = "Methods:")

ggsave(paste0(path, "comparison_parametric.png"), width = 25, height = 13, units = "cm")

# Comparison Nonparametric and Semiparametric
parametric_graph <- rbind(non_para, semiparametric_output, crf)
parametric_graph_quantile <- parametric_graph[c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),]

# Only selection of methods
parametric_estimators <- data.frame(cate = rep(c("Non-Parametric Kernel Regression", "Semi-Parametric Regression", "Causal Random Forest"), each=5),
                                    quant=rep(c("Q1", "Q2", "Q3", "Q4", "Q5"),3),
                                    coef = parametric_graph_quantile[,1],
                                    CIl = parametric_graph_quantile[,3],
                                    CIu = parametric_graph_quantile[,4])


# Confidence Interval with grey area
ggplot(parametric_estimators, aes(x=quant, y=coef, group=cate, color=cate)) + 
  geom_ribbon(aes(ymin=CIl, ymax=CIu), linetype=2, alpha=0.05) +
  geom_line() +
  scale_color_brewer(palette="Paired")+theme_minimal() + 
  labs(x = "Income Quintile", y = "Treatment Effect", color = "Methods:")
ggsave(paste0(path, "comparison_nonparametric_semiparametric.png"), width = 25, height = 13, units = "cm")


###### ATE plot
ate_plot <- rbind(doubly_robust_base, cond_means, gam, semiparametric_output, non_para, lasso, ds_lasso, crf)
ate_plot_data <- ate_plot[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),]
# all_estimators <- data.frame(cate = rep(c("Mean Comparison", "Conditional Means", "IPW", "IPW Restricted", "IPW Restricted 2", "Doubly Robust Base","Doubly Robust Restricted", "Nonparametric", "Semiparametric", "Lasso", "Double Selection Lasso", "Causal Random Forest"), each=5),
ate_plot_data <- data.frame(ate = rep(c("Doubly Robust Parametric", "Linear Conditional Outcome Regression",  "Generalized Additive Model", "Semi-Parametric Regression", "Non-Parametric", "Lasso", "Double Selection Lasso", "Causal Random Forest"), each=1),
                             coef = ate_plot_data[,1],
                             CIl = ate_plot_data[,3],
                             CIu = ate_plot_data[,4])


ggplot(ate_plot_data, aes(x=ate, y=coef)) + 
  geom_errorbar(mapping = aes(x = ate, ymin = CIl, ymax = CIu), width = 0.3, alpha = 0.9)+theme_minimal() + 
  labs(x = "Method", y = "Treatment Effect", color = "Methods:") + theme(axis.text.x = element_text(angle = 50, vjust = 0.9, hjust=1)) +
  geom_point(mapping=aes(x=ate, y=coef), size=4, shape=21, fill="white")

ggsave(paste0(path, "ate_plot.png"), width = 25, height = 13, units = "cm")
  
