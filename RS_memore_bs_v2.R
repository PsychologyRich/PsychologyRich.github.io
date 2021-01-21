library(broom)
library(dplyr)
library(boot)
library(tidyverse)

#Set working directory
setwd("C:/Users/user/R/MEMORE in R")

# open dataset
# dataset should be organised in 4 columns labelled M1, M2, Y1, Y2
# as in Table 1, Montoya & Hayes (2016) 
data <- read.csv("data.csv")

# Calculate M_diff, Y_diff, Msum, MeanMsum, MsumCen (centred)
data$M_diff <- data$M2 - data$M1
data$Y_diff <- data$Y2 - data$Y1
data$Msum <-  data$M1 + data$M2
data$MeanMsum <- mean (data$Msum)
data$MsumCen <-  data$Msum - data$MeanMsum

# calculate value of coef a
a <- mean(data$M_diff)

# calculate value of coef b, SE of coef b, and 95% CI of b
# via regression of M_diff and MsumCen on Y_diff
# Uses matrix function to send regression output to a matrix...
# and then extracts b and SE b from the matrix
# final two lines calculate the upper and lower 95% CI
matrix_b <- summary(lm(Y_diff ~ M_diff + MsumCen, data))$coefficients
b <- matrix_b[2 , 1]
b_se <- matrix_b[2 , 2]
lower_ci_b <- b -(1.96 * b_se)
upper_ci_b <- b +(1.96 * b_se)

# calculate ab, coef of indirect effect, as product a*b
ab <- a * b

# function to calculate a*b for the bootstrap resampling
# this is entered into the boot command
foo <- function(data, indices, cor.type){
  data_bs <- data[indices,]
  data_bs$M_diff <- data_bs$M2 - data_bs$M1
  data_bs$Y_diff <- data_bs$Y2 - data_bs$Y1
  data_bs$Msum <-  data_bs$M1 + data_bs$M2
  data_bs$MeanMsum <- mean (data_bs$Msum)
  data_bs$MsumCen <-  data_bs$Msum - data_bs$MeanMsum
  f_a <- mean(data_bs$M_diff)
  f_matrix_b <- summary(lm(Y_diff ~ M_diff + MsumCen, data))$coefficients
  f_b <- f_matrix_b[2 , 1]
  f_ab <- f_a * f_b
  return(f_ab)
}

# Does X predict Y? Paired samples t-test
# the mutate command changes Y1 to Y3 to correct sign of t and 95% CI
beta_ydiff <- mean (data$Y_diff)
beta_ydiff #beta value (c)
data_y.long <- data %>%
  select(Y2, Y1) %>%
  gather(key = "group_y", value = "score_y", Y2, Y1) %>%
  mutate(group_y = ifelse(as.character(group_y) == "Y1", "Y3", as.character(group_y)))
res_y <- t.test(score_y ~ group_y, data = data_y.long, paired = TRUE)

# Does X predict M? Paired samples t-test
# the mutate command changes M1 to M3 to correct sign of t and 95% CI
beta_mdiff <- mean (data$M_diff)
beta_mdiff #beta value (a)
data_m.long <- data %>%
  select(M1, M2) %>%
  gather(key = "group_m", value = "score_m", M2, M1) %>%
  mutate(group_m = ifelse(as.character(group_m) == "M1", "M3", as.character(group_m)))
res_m <- t.test(score_m ~ group_m, data = data_m.long, paired = TRUE)

# Run boostrap for a*b
# Recommend use "percentile" bootstrap estimate of 95% CI
# Use value R = x to set number of boostrap samples
set.seed(42)
result <- boot(data, foo, R=5000)

# *******************OUTPUT STARTS HERE************************************

# ********Does X predict Y? Paired samples t-test output********
# ********Coefficient c is "mean of the differences" ********
res_y

# ********Does X predict M? Paired samples t-test output********
# ********Coefficient a is "mean of the differences"********
res_m

# ********Does M predict Y? Regression of M_diff and MsumCen on Y_diff********
# ********Coefficient b (M_diff) is labelled "b" ********
b
lower_ci_b
upper_ci_b

# ********Indirect effect of X on Y, via M (ab)********
# ********Coefficient c' (ab)********
ab 

# Boostrap analysis of distribution of Coefficient c' (ab)
# Recommend use "percentile" bootstrap estimate of 95% CI
boot.ci(result, index=1)