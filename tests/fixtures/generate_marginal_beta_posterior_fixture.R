#!/usr/bin/env Rscript

thinplate <- function(squared_distance) {
  ifelse(
    squared_distance > 0,
    0.5 * squared_distance * log(squared_distance),
    0
  )
}

surface_basis <- function(x, knots) {
  out <- matrix(0, nrow(x), nrow(knots))
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(nrow(knots))) {
      out[i, j] <- thinplate(sum((x[i, ] - knots[j, ])^2))
    }
  }
  out
}

additive_basis <- function(x, knots, counts) {
  columns <- list()
  start <- 1
  for (j in seq_along(counts)) {
    count <- counts[j]
    if (count > 0) {
      stop <- start + count - 1
      columns[[length(columns) + 1]] <- thinplate(
        outer(x[, j], knots[start:stop], "-")^2
      )
      start <- stop + 1
    }
  }
  do.call(cbind, columns)
}

make_p_matrices <- function(X, q_i, p_types) {
  out <- list()
  q1 <- c(1, q_i)
  for (i in seq_along(q_i)) {
    if (p_types[i] == "identity") {
      out[[i]] <- diag(q_i[i])
    } else if (p_types[i] == "X'X") {
      idx <- sum(q1[1:i]):sum(q_i[1:i])
      X_block <- X[, idx, drop = FALSE]
      out[[i]] <- crossprod(X_block)
    } else {
      stop("unknown P matrix type")
    }
  }
  out
}

coefficient_prior_covariance <- function(P_mats, sigma, log_shrinkage, beta_prior_variance) {
  p <- ncol(sigma)
  q_i <- vapply(P_mats, nrow, integer(1))
  q <- sum(q_i)
  out <- matrix(0, q * p, q * p)
  row_start <- 1
  for (i in seq_along(P_mats)) {
    rows <- row_start:(row_start + q_i[i] - 1)
    lambda <- exp(log_shrinkage[i, ])
    response_cov <- beta_prior_variance * (sqrt(lambda) %o% sqrt(lambda) * sigma)
    block_cov <- kronecker(solve(P_mats[[i]]), response_cov)
    idx <- unlist(lapply(rows, function(row) (row - 1) * p + seq_len(p)))
    out[idx, idx] <- block_cov
    row_start <- row_start + q_i[i]
  }
  out
}

mvnormal_logpdf <- function(value, covariance) {
  value <- matrix(value, ncol = 1)
  logdet <- determinant(covariance, logarithm = TRUE)$modulus[1]
  -0.5 * length(value) * log(2 * pi) -
    0.5 * logdet -
    0.5 * drop(t(value) %*% solve(covariance, value))
}

x <- matrix(
  c(
    -0.8, -0.3,
     0.2,  0.4,
     0.7, -0.6,
     1.1,  0.9,
    -1.2,  0.8
  ),
  ncol = 2,
  byrow = TRUE
)
additive_knots <- matrix(c(-0.6, 0.3, -0.2), ncol = 1)
surface_knots <- matrix(c(-0.4, 0.1, 0.8, -0.7), ncol = 2, byrow = TRUE)
additive_counts <- c(2, 1)

X <- cbind(
  1,
  x,
  additive_basis(x, additive_knots, additive_counts),
  surface_basis(x, surface_knots)
)

beta <- matrix(
  c(
     0.20, -0.10,
     1.10,  0.40,
    -0.50,  0.70,
     0.30, -0.20,
    -0.25,  0.15,
     0.45, -0.35,
    -0.10,  0.25,
     0.05,  0.30
  ),
  ncol = 2,
  byrow = TRUE
)
residual <- matrix(
  c(
     0.10, -0.08,
    -0.05,  0.02,
     0.03,  0.06,
    -0.07,  0.04,
     0.02, -0.03
  ),
  ncol = 2,
  byrow = TRUE
)
y <- X %*% beta + residual
sigma <- matrix(c(1.3, 0.25, 0.25, 0.9), ncol = 2)
log_shrinkage <- matrix(
  log(c(1.2, 0.7, 0.8, 1.5, 1.6, 0.9)),
  ncol = 2,
  byrow = TRUE
)
beta_prior_variance <- 1.7

P_mats <- make_p_matrices(
  X,
  q_i = c(3, 3, 2),
  p_types = c("X'X", "identity", "identity")
)
beta_cov <- coefficient_prior_covariance(
  P_mats,
  sigma,
  log_shrinkage,
  beta_prior_variance
)

y_vec <- as.vector(t(y))
design_for_vec <- kronecker(X, diag(ncol(y)))
observation_cov <- kronecker(diag(nrow(y)), sigma)
marginal_cov <- observation_cov + design_for_vec %*% beta_cov %*% t(design_for_vec)
marginal_log_likelihood <- mvnormal_logpdf(y_vec, marginal_cov)

prior_precision <- solve(beta_cov)
observation_precision <- solve(observation_cov)
posterior_precision <- prior_precision +
  t(design_for_vec) %*% observation_precision %*% design_for_vec
posterior_beta_covariance <- solve(posterior_precision)
posterior_beta_mean_vec <- posterior_beta_covariance %*%
  (t(design_for_vec) %*% observation_precision %*% y_vec)
posterior_beta_mean <- matrix(posterior_beta_mean_vec, ncol = ncol(y), byrow = TRUE)

records <- data.frame(kind = character(), row = integer(), col = integer(), value = numeric())
append_matrix <- function(kind, value) {
  for (i in seq_len(nrow(value))) {
    for (j in seq_len(ncol(value))) {
      records <<- rbind(
        records,
        data.frame(kind = kind, row = i, col = j, value = value[i, j])
      )
    }
  }
}

append_matrix("coefficient_prior_covariance", beta_cov)
append_matrix("posterior_beta_mean", posterior_beta_mean)
append_matrix("posterior_beta_covariance", posterior_beta_covariance)
records <- rbind(
  records,
  data.frame(
    kind = "marginal_log_likelihood",
    row = 0,
    col = 0,
    value = marginal_log_likelihood
  )
)

write.csv(records, row.names = FALSE, quote = FALSE)
