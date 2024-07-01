test_that("susie_ash works in basic set up", {
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

  # Check susie_ash() outputs
  expect_equal(dim(res$alpha), c(min(10,ncol(X)), ncol(X)))
  expect_equal(dim(res$mu), c(min(10,ncol(X)), ncol(X)))
  expect_equal(length(res$pip), ncol(X))
  expect_true(all(res$pip >= 0 & res$pip <= 1))
})
