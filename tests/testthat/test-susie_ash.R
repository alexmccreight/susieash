test_that("Basic Data Setup", {
  # Basic Data Setup
  set.seed(1)
  n = 1000
  p = 1000
  L = 10
  beta = rep(0,p)
  beta[1:4] = 1
  X = matrix(rnorm(n*p),nrow = n,ncol = p)
  X = scale(X,center = TRUE,scale = TRUE)
  y = drop(X %*% beta + rnorm(n))

  # Run susie_ash()
  res <- susie_ash(X, y, L = L)

  ## Class Check
  expect_s3_class(res, "susie")

  ## Output Dimension Check
  expect_equal(dim(res$alpha), c(min(10,ncol(X)), ncol(X)))
  expect_equal(dim(res$mu), c(min(10,ncol(X)), ncol(X)))
  expect_equal(length(res$pip), ncol(X))

  ## Numerical Properties Check
  expect_true(all(res$pip >= 0 & res$pip <= 1))
  expect_true(res$sigma2 > 0)
  expect_true(all(res$V >= 0)) # Can be equal to zero for non-causal effect
  expect_true(all(diff(res$elbo) >= 0)) # Check for non-decreasing ELBO

  ## Consistency Check
  expect_true(res$converged)
  expect_true(res$niter <= 100)

  ## Fitted Values Check
  expect_true(cor(res$fitted, y) > 0.7) # Expect to see y and y_hat to be highly correlated
  expect_true(cor(colSums(res$alpha * res$mu), beta) > 0.7) # Expect to see beta and beta_hat to be highly correlated
})

#test_that("Nonsparse Effects From Normal Distribution", {
  #### Nonsparsity Theta to Beta Heritability Definitions ####
  # Low Nonsparsity -->       0.5:1
  # Moderate Nonsparsity -->  1.4:1
  # High Nonsparsity -->        3:1
  # Very High Nonsparsity -->   5:1
  ############################################################


#   # Normally Distributed Nonsparse Effects
#   set.seed(1)
#   generate_data <- function(X, total_heritability, sparse_effects, nonsparse_coverage, theta_beta_ratio) {
#     n <- nrow(X)
#     p <- ncol(X)
#
#     # Generate sparse effects (beta.true)
#     beta.true <- rep(0, p)
#     beta.true[sample(p, sparse_effects)] <- rnorm(sparse_effects, mean = 0, sd = 0.5)
#
#     # Generate nonsparse effects (theta.true)
#     num_nonsparse <- round(p * nonsparse_coverage)
#     theta.true <- rep(0, p)
#     theta.true[sample(p, num_nonsparse)] <- rnorm(num_nonsparse, mean = 0, sd = 0.0005)
#
#     # Scale X Matrix
#     X <- scale(X, center = TRUE, scale = TRUE)
#
#     # Adjust the effect sizes based on the theta to beta heritability ratio
#     var_beta <- var(X %*% beta.true)
#     var_theta <- var(X %*% theta.true)
#     ratio_factor <- as.numeric((theta_beta_ratio * var_beta) / var_theta)
#     theta.true <- theta.true * sqrt(ratio_factor)
#
#     # Recalculate the variance of the adjusted nonsparse effects
#     var_theta_adjusted <- var(X %*% theta.true)
#
#     # Calculate the residual variance based on the total heritability
#     sigmasq_error <- (var_beta + var_theta_adjusted) * (1 - total_heritability) / total_heritability
#
#     # Create Outcomes
#     y <- X %*% matrix(beta.true, ncol = 1) + X %*% matrix(theta.true, ncol = 1) + rnorm(n, 0, sqrt(sigmasq_error))
#     y <- scale(y, center = TRUE, scale = FALSE)
#
#     y_oracle <- y - X %*% matrix(beta.true, ncol = 1)
#
#     # Store Information
#     return(list(y = y,X = X, error = sigmasq_error, beta = beta.true, theta = theta.true, y_oracle = y_oracle))
#   }
#
#   # Read in X matrix and verify its dimensions
#   X4 = readRDS(test_path("/Users/alexmccreight/Columbia/Research/SuSiE-ASH/SuSiE-ASH/data/X4")) # 5000x1979 matrix
#   expect_equal(dim(X4), c(5000, 1979))
#
#   # Generate data based upon above definitions
#   low_nonsparse_data <- generate_data(X = X4, total_heritability = 0.5, sparse_effects = 2, nonsparse_coverage = 0.01, theta_beta_ratio = 0.5)
#   moderate_nonsparse_data <- generate_data(X = X4, total_heritability = 0.5, sparse_effects = 2, nonsparse_coverage = 0.01, theta_beta_ratio = 1.4)
#   high_nonsparse_data <- generate_data(X = X4, total_heritability = 0.5, sparse_effects = 2, nonsparse_coverage = 0.01, theta_beta_ratio = 3)
#   veryhigh_nonsparse_data <- generate_data(X = X4, total_heritability = 0.5, sparse_effects = 2, nonsparse_coverage = 0.01, theta_beta_ratio = 5)
#
#   #### LOW NONSPARSITY ####
#   # Run susie_ash and susie
#   res <- susie_ash(low_nonsparse_data$X, low_nonsparse_data$y, L = 10, standardize = F)
#   res_susie <- susieR::susie(low_nonsparse_data$X, low_nonsparse_data$y, L = 10, standardize = F)
#
#   ## Class Check
#   expect_s3_class(res, "susie")
#
#   ## Output Dimension Check
#   expect_equal(dim(res$alpha), c(min(10,ncol(low_nonsparse_data$X)), ncol(low_nonsparse_data$X)))
#   expect_equal(dim(res$mu), c(min(10,ncol(low_nonsparse_data$X)), ncol(low_nonsparse_data$X)))
#   expect_equal(length(res$pip), ncol(low_nonsparse_data$X))
#
#   ## Numerical Properties Check
#   expect_true(all(res$pip >= 0 & res$pip <= 1))
#   expect_true(res$sigma2 > 0)
#   expect_true(all(res$V >= 0)) # Can be equal to zero for non-causal effect
#   expect_true(all(diff(res$elbo) >= 0)) # Check for non-decreasing ELBO
#
#   ## Consistency Check
#   expect_true(res$converged)
#   expect_true(res$niter <= 100)
#
#   ## Fitted Values Check
#   expect_true(cor(res$fitted, low_nonsparse_data$y) > 0.5) # Expect to see y and y_hat to be correlated
#   expect_true(cor(colSums(res$alpha * res$mu), low_nonsparse_data$beta) > 0.5) # Expect to see beta and beta_hat to be correlated
#
#   ## SuSiE Comparison Check
#   #expect_true(cor(res$fitted, low_nonsparse_data$y) > cor(res_susie$fitted, low_nonsparse_data$y))
#   #expect_true(cor(colSums(res$alpha * res$mu), low_nonsparse_data$beta) > cor(colSums(res_susie$alpha * res_susie$mu), low_nonsparse_data$beta))
#
#
#   #### MODERATE NONSPARSITY ####
#   # Run susie_ash and susie
#   res <- susie_ash(moderate_nonsparse_data$X, moderate_nonsparse_data$y, L = 10, standardize = F)
#   res_susie <- susieR::susie(moderate_nonsparse_data$X, moderate_nonsparse_data$y, L = 10, standardize = F)
#
#   ## Class Check
#   expect_s3_class(res, "susie")
#
#   ## Output Dimension Check
#   expect_equal(dim(res$alpha), c(min(10,ncol(moderate_nonsparse_data$X)), ncol(moderate_nonsparse_data$X)))
#   expect_equal(dim(res$mu), c(min(10,ncol(moderate_nonsparse_data$X)), ncol(moderate_nonsparse_data$X)))
#   expect_equal(length(res$pip), ncol(moderate_nonsparse_data$X))
#
#   ## Numerical Properties Check
#   expect_true(all(res$pip >= 0 & res$pip <= 1))
#   expect_true(res$sigma2 > 0)
#   expect_true(all(res$V >= 0)) # Can be equal to zero for non-causal effect
#   expect_true(all(diff(res$elbo) >= 0)) # Check for non-decreasing ELBO
#
#   ## Consistency Check
#   expect_true(res$converged)
#   expect_true(res$niter <= 100)
#
#   ## Fitted Values Check
#   expect_true(cor(res$fitted, moderate_nonsparse_data$y) > 0.5) # Expect to see y and y_hat to be correlated
#   expect_true(cor(colSums(res$alpha * res$mu), moderate_nonsparse_data$beta) > 0.5) # Expect to see beta and beta_hat to be correlated
#
#   ## SuSiE Comparison Check
#   expect_true(cor(res$fitted, moderate_nonsparse_data$y) > cor(res_susie$fitted, moderate_nonsparse_data$y))
#   expect_true(cor(colSums(res$alpha * res$mu), moderate_nonsparse_data$beta) > cor(colSums(res_susie$alpha * res_susie$mu), moderate_nonsparse_data$beta))
# })
