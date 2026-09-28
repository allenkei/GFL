library(vars)
library(MASS)
library(reticulate)
library(ggplot2)
library(reshape2)
library(igraph)
library(clue)
library(mclust)

np <- import("numpy")

simulate_var1_randomPhi_blockSigma <- function(
    T = 100,
    mus = c(-0.95, 0, 0.95),                # mean per cluster
    cluster_sizes = c(30, 40, 50),   # 1..30, 31..70, 71..120
    phi_sd = 0.10,                   # spread of random Phi entries before shrinking
    target_rho = 0.98,               # cap spectral radius of Phi
    sigma2 = c(1, 1, 1),             # innovation variance by cluster
    rho_eps = c(0.5, 0.5, 0.5),   # within-cluster corr in Sigma blocks
    burn_in = 500,
    random_phi = FALSE
){
  stopifnot(length(cluster_sizes) == 3, length(mus) == 3)
  stopifnot(length(sigma2) == 3, length(rho_eps) == 3)
  
  K <- 3
  n <- sum(cluster_sizes)
  idx_start <- cumsum(c(1, head(cluster_sizes, -1)))
  idx_end   <- cumsum(cluster_sizes)
  
  # --- labels and means ---
  z  <- rep(1:(K), times = cluster_sizes)
  mu <- mus[z]
  
  if (random_phi) {
    # random Phi, then enforce stationarity by spectral shrinkage ---
    Phi_raw <- matrix(rnorm(n*n, mean = 0, sd = phi_sd), n, n)
    ev <- eigen(Phi_raw, only.values = TRUE)$values
    rho <- max(Mod(ev))
    if (rho >= target_rho) {
      Phi <- Phi_raw * (target_rho / rho)
    } else {
      Phi <- Phi_raw
    }
  } else {
  # alternative: diagonal Phi
    # Phi <- diag(1 / (1 + 1:n))
    Phi <- diag(rep(0.5, n))
  }
  # --- block-diagonal Sigma with compound symmetry in each cluster ---
  make_sigma_block <- function(m, sig2, r) {
    # PSD if r in (-1/(m-1), 1)
    stopifnot(r > -1/(m-1) && r < 1)
    (sig2 * (1 - r)) * diag(m) + (sig2 * r) * matrix(1, m, m)
  }
  Sigma <- matrix(0, n, n)
  for (k in 1:K) {
    i1 <- idx_start[k]; i2 <- idx_end[k]; m <- cluster_sizes[k]
    Sigma[i1:i2, i1:i2] <- make_sigma_block(m, sigma2[k], rho_eps[k])
  }
  
  # --- first observation calibration
  # vec_Sy <- solve(diag(n*n) - kronecker(Phi, Phi)) %*% as.vector(Sigma)
  # Sigma_y <- matrix(vec_Sy, n, n)
  
  # --- simulate VAR(1) ---
  Y <- matrix(0, ncol = burn_in + T, nrow = n)
  y <- rep(0, n)
  for (t in 1:(burn_in + T)) {
    eps <- mvrnorm(1, mu = rep(0, n), Sigma = Sigma)
    y <- Phi %*% (y-mu) + mu + eps
    Y[,t] <- y
  }
  X <- Y[,(burn_in + 1):(burn_in + T)]
  
  list(
    y = X,                     # 120 x T matrix
    Phi = Phi,                 # 120 x 120 random, stationary
    Sigma = Sigma,             # 120 x 120 block-diagonal
    labels = as.integer(z-1),                     # true cluster labels (0..2)
    mu = mu
  )
}

sbm_adjacency <- function(cluster_sizes = c(30,40,50),
                          p_in = 0.30, p_out = 0.15) {
  K <- length(cluster_sizes)
  z <- rep(seq_len(K), times = cluster_sizes)
  n <- sum(cluster_sizes)
  
  A <- matrix(NA, n, n)  # initialize with zeros
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      p <- if (z[i] == z[j]) p_in else p_out
      edge <- rbinom(1, 1, p)
      A[i, j] <- edge
      A[j, i] <- edge   # make symmetric
    }
  }
  
  diag(A) <- 0L   # no self-loops
  list(adj_matrices = A)
}





simulate_repeat <- function(nsim = 10, 
                            cluster_sizes = c(30, 40, 50), 
                            T = 100, 
                            mus = c(0, 1, 2),
                            rho_eps = c(0.5, 0.5, 0.5),
                            burn_in = 200,
                            random_phi = FALSE) {
  adj_matrices <- array(NA, dim = c(nsim, sum(cluster_sizes), sum(cluster_sizes)))
  y <- array(NA, dim = c(nsim, sum(cluster_sizes), T))
  labels <- matrix(NA, nrow = nsim, ncol = sum(cluster_sizes))
  for (i in 1:nsim) {
    sim <- simulate_var1_randomPhi_blockSigma(T = T, cluster_sizes = cluster_sizes, 
                                              mus = mus, rho_eps = rho_eps,
                                              random_phi = random_phi, burn_in = burn_in)
    adj <- sbm_adjacency(cluster_sizes = cluster_sizes)
    adj_matrices[i, , ] <- adj$adj_matrices
    y[i, , ] <- sim$y # n by T matrix
    labels[i, ] <- sim$labels
  }
  list(adj_matrices = adj_matrices, y = y, labels = labels)
}






set.seed(312001)
data_s3_n120 <- simulate_repeat(nsim = 25, cluster_sizes = c(30,40,50), 
                                mus = c(-0.9, 0, 0.9), T = 90,
                                rho_eps = c(0.15, 0.15, 0.15), burn_in = 100,
                                random_phi = FALSE)


set.seed(3210)
data_s3_n210 <- simulate_repeat(nsim = 25, cluster_sizes = c(60,70,80), T = 90,
                                mus = c(-0.9, 0, 0.9),
                                rho_eps = c(0.15, 0.15, 0.15), burn_in = 100,
                                random_phi = FALSE)



save_as_npz <- function(filename, y, adj_matrices, labels) {
  np <- import("numpy")
  # convert R arrays -> numpy arrays
  np$savez(filename,
           y = y,   # shape: S × n × T
           adj_matrices = adj_matrices,   # shape: S × n × n
           labels = labels)   # shape: S × n
}



save_as_npz("data/data_s3_n120.npz",
            y = data_s3_n120$y,  # S x n x T
            adj_matrices = data_s3_n120$adj_matrices,         # S x n x n
            labels = data_s3_n120$labels)                # S x n

save_as_npz("data/data_s3_n210.npz",
            y = data_s3_n210$y,  # S x n x T
            adj_matrices = data_s3_n210$adj_matrices,         # S x n x n
            labels = data_s3_n210$labels)       


