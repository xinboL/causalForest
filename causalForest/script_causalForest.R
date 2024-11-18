#The latest release of the package can be installed through CRAN:
install.packages("grf")

library(grf)
#The GRF library requires input data structured in matrix or data frame format. 
#Each row represents an observation, 
#columns include features (predictors), treatment assignments, and outcome variables. 
#For causal inference, the data should include a binary or continuous treatment variable 
#and outcome, along with relevant covariates to model heterogeneity effectively.
# Generate data.
n <- 2000#number of samples
p <- 10#number of features
X <- matrix(rnorm(n * p), n, p)#n independent and identically distributed training examples, feature vector in between 0 and 1
X.test <- matrix(0, 101, p)
X.test[, 1] <- seq(-2, 2, length.out = 101)

# Train a causal forest.
W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))
#w is treatment indicator, its value equals 0 or 1
#rbinom(n, 1, prob)?????????????????????,??????????????????????????????(0???1)
#0.4 + 0.2 * (X[, 1] > 0)??????,??????X???????????????????????????0,??????????????????????????????0.4??????????????????0.2,???????????????????????????X[, 1]???????????????0,????????????0.6,?????????0.4???

Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#Y_i=max(X[i,1],0)*W_i+X[i.2]+min(X[i,3],0)+epsilon_i i=1,2,...,n

tau.forest <- causal_forest(X, Y, W)

# Estimate treatment effects for the training data using out-of-bag prediction.
tau.hat.oob <- predict(tau.forest)
hist(tau.hat.oob$predictions)

# Estimate treatment effects for the test sample.
tau.hat <- predict(tau.forest, X.test)
plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 2)

# Estimate the conditional average treatment effect on the full sample (CATE).
#The average treatment effect (target.sample = all): E[Y(1) - Y(0)].
average_treatment_effect(tau.forest, target.sample = "all")

# Estimate the conditional average treatment effect on the treated sample (CATT).
#The average treatment effect on the treated (target.sample = treated): E[Y(1) - Y(0) | Wi = 1].
average_treatment_effect(tau.forest, target.sample = "treated")
#
# Add confidence intervals for heterogeneous treatment effects; growing more trees is now recommended.
tau.forest <- causal_forest(X, Y, W, num.trees = 4000)#Parameter Tuning
tau.hat <- predict(tau.forest, X.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[, 1], tau.hat$predictions + 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[, 1], tau.hat$predictions - 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 1)