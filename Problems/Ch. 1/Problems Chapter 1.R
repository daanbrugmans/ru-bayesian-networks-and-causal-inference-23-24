  # Problem 1.3
simpson.simulator <- function(sample_size, noise_std, true_causal_effect
                              ){
  Z1 <- rnorm(sample_size,0,noise_std)
  Z3 <- rnorm(sample_size,0,noise_std) + Z1
  Z5 <- rnorm(sample_size,0,noise_std) + Z3
  U <- rnorm(sample_size,0,noise_std) + Z1
  Z4 <- rnorm(sample_size,0,noise_std) + Z5 + U
  Z2 <- rnorm(sample_size,0,noise_std) + Z3 + U
  X <- rnorm(sample_size,0,noise_std) + U
  Y <- rnorm(sample_size,0,noise_std) + true_causal_effect *X + 10*Z5
  data.frame(Y,X,Z1,Z2,Z3,Z4,Z5)
}

dataset <- simpson.simulator(1000,0.01,1)

lm_no_covariates <- lm(dataset[, 1:2])
lm_z1_covariate <- lm(dataset[,1:3])
lm_z1_z2_covariates <- lm(dataset[,1:4])

lm_no_covariates[["coefficients"]][["X"]]
lm_z1_covariate[["coefficients"]][["X"]]
lm_z1_z2_covariates[["coefficients"]][["X"]]

