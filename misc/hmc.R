# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         Utils                                         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
ltail <- function(l) {
  l[[length(l)]]
}


lappend <- function(l, object) {
  l[[length(l) + 1]] <- object
  l
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                          HMC                                          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

leapfrog <- function(p, q, neg_dlogp, path_length, step_size) {
  leap_q <- list()
  leap_p <- list()
  p <- p - step_size * neg_dlogp(q) / 2
  
  for (i in seq_len(round(path_length / step_size) - 1)) {
    q <- q + step_size * p
    p <- p - step_size * neg_dlogp(q)
    leap_q[[i]] <- q
    leap_p[[i]] <- p
  }
  q <- q + step_size * p
  p <- p - step_size * neg_dlogp(q) / 2
  
  # Momentum flip
  return(list(q = q, p = -p, leap_q = leap_q, leap_p = leap_p))
}


SamplerHMC <- R6::R6Class(
  "SamplerHMC",
  public = list(
    neg_logp = NULL,
    neg_dlogp = NULL,
    initial_position = c(1.6, -0.6),
    path_length = 1, 
    step_size = 0.01,
    generated_samples = list(),

    initialize = function(
      neg_logp, 
      neg_dlogp, 
      initial_position = NULL, 
      path_length = NULL, 
      step_size = NULL
    ) {
      if (!is.null(initial_position)) {
        self$initial_position = initial_position
      }
      if (!is.null(path_length)) {
        self$set_path_length(path_length)
      }
      if (!is.null(step_size)) {
        self$set_step_size(step_size)
      }
      self$neg_logp = neg_logp
      self$neg_dlogp = neg_dlogp
      self$generated_samples <- lappend(
        self$generated_samples, self$initial_position
      )
    },
    
    set_path_length = function(x) {
      stopifnot(x > 0)
      self$path_length <- x
    },
    
    set_step_size = function(x) {
      stopifnot(x > 0, x < self$path_length)
      self$step_size <- x
    },

    generate_momentum = function() {
      n <- length(self$initial_position)
      mvtnorm::rmvnorm(1, rep(0, n), diag(n))
    },
    
    generate_sample = function() {
      p_current <- self$generate_momentum()
      q_current <- ltail(self$generated_samples)
      
      integration <- leapfrog(
        p_current, q_current, self$neg_dlogp, self$path_length, self$step_size
      )
      
      p_new <- integration$p
      q_new <- integration$q
      leap_p <- integration$leap_p
      leap_q <- integration$leap_q

      # Metropolis acceptance criteria
      current_logp <- self$neg_logp(q_current) - mvtnorm::dmvnorm(p_current, log = TRUE)
      new_logp <- self$neg_logp(q_new) - mvtnorm::dmvnorm(p_new, log = TRUE)
      
      accepted <- log(runif(1)) < current_logp - new_logp
      sample <- if (accepted) q_new else q_current
      self$generated_samples <- lappend(self$generated_samples, sample)
      
      return(
        list(
          sample = sample, 
          leap_q = leap_q, 
          momentum = p_current,
          accepted = accepted,
          new_sample = q_new
        )
      )
    }
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                  Some distributions                                   #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(ggplot2)

## Gaussian distribution

# Unnormalized gaussian logp
normal_logp <- function(x, y, mu, tau) {
  X <- c(x, y)
  -0.5 * t(X - mu) %*% tau %*% (X - mu)
}

# Derivative with respect to the first coordinate
normal_dlogpdx <- Deriv::Deriv(normal_logp, "x")

# Derivative with respect to the second coordinate
normal_dlogpdy <- Deriv::Deriv(normal_logp, "y")

# A function that returns derivative with respect to both 'x' and 'y'
normal_dlogp <- function(x, y, mu, tau) {
  c(normal_dlogpdx(x, y, mu, tau), normal_dlogpdy(x, y, mu, tau))
}

# The sampler needs neg_logp and neg_dlogp. 
# These functions only receive the positions, so parameter values must be taken from
# somewhere else. So we have this builder function.
make_neg_logp <- function(mu, tau) {
    function(x) as.numeric(- normal_logp(x[1], x[2], mu, tau))
}

make_neg_dlogp <- function(mu, tau) {
    function(x) as.numeric(- normal_dlogp(x[1], x[2], mu, tau))
}

mu <- c(0.3, -0.5)
Sigma <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
tau <- solve(Sigma)

neg_logp <- make_neg_logp(mu, tau)
neg_dlogp <- make_neg_dlogp(mu, tau)
neg_logp(c(0.1, 0.3))
neg_dlogp(c(0.1, 0.3))

sampler <- SamplerHMC$new(neg_logp, neg_dlogp, step_size = 0.05, path_length = 2)

# One sample
sampler$generate_sample()

draws <- matrix(0, nrow = 1000, ncol = 2)

for (i in seq_len(nrow(draws))) {
    draws[i, ] <- sampler$generate_sample()$new_sample
}

cor(draws)
apply(draws, 2, mean)

f <- function(x, y) {
    mu <- c(0.3, -0.5)
    Sigma <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
    tau <- solve(Sigma)
    X <- c(x, y)
    # Could also use the 'mvtnorm' package
    exp(-0.5 * t(X - mu) %*% tau %*% (X - mu))
}

x1 <- seq(-4, 4, length.out = 100)
x2 <- seq(-4, 4, length.out = 100)

data <- tidyr::crossing(x1 = x1, x2 = x2)
data |>
    dplyr::mutate(f = purrr::map2_dbl(x1, x2, ~ f(.x, .y))) |>
    ggplot() + 
    geom_raster(aes(x = x1, y = x2, fill = f)) +
    stat_contour(aes(x = x1, y = x2, z = f), color = "white", bins = 8) +
    geom_point(aes(x = X1, y = X2), color = "white", alpha = 0.7, data = data.frame(draws)) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    viridis::scale_fill_viridis()


## Banana distribution

# Unnormalized banana logp
banana_logp <- function(x, y, a, b) {
  - ((a - x) ^ 2 + b * (y - x ^ 2) ^ 2)
}

banana_dlogpdx <- Deriv::Deriv(banana_logp, "x")
banana_dlogpdy <- Deriv::Deriv(banana_logp, "y")
banana_dlogp <- function(x, y, a, b) {
  c(banana_dlogpdx(x, y, a, b), banana_dlogpdy(x, y, a, b))
}

make_neg_logp <- function(a, b) {
    function(x) as.numeric(- banana_logp(x[1], x[2], a, b))
}

make_neg_dlogp <- function(a, b) {
    function(x) as.numeric(- banana_dlogp(x[1], x[2], a, b))
}

a <- 0.5
b <- 5

neg_logp <- make_neg_logp(a, b)
neg_dlogp <- make_neg_dlogp(a, b)

neg_logp(c(0.1, 0.3))
neg_dlogp(c(0.1, 0.3))


sampler <- SamplerHMC$new(neg_logp, neg_dlogp, step_size = 0.02, path_length = 1)

# One sample
sampler$generate_sample()

draws <- matrix(0, nrow = 1000, ncol = 2)

for (i in seq_len(nrow(draws))) {
    draws[i, ] <- sampler$generate_sample()$new_sample
}

# Plot

f <- function(x, y) {
    a <- 0.5
    b <- 5
    exp(- ((a - x) ^ 2 + b * (y - x^2) ^ 2))
}

x1 <- seq(-2.5, 3, length.out = 100)
x2 <- seq(-1.5, 7.5, length.out = 100)

data <- tidyr::crossing(x1 = x1, x2 = x2)
data |>
    dplyr::mutate(f = purrr::map2_dbl(x1, x2, ~ f(.x, .y))) |>
    ggplot() + 
    geom_raster(aes(x = x1, y = x2, fill = f)) +
    stat_contour(aes(x = x1, y = x2, z = f), color = "white", bins = 8) +
    geom_point(aes(x = X1, y = X2), color = "white", alpha = 0.7, data = data.frame(draws)) + 
    scale_x_continuous(limits = c(-2.5, 3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1.5, 7.5), expand = c(0, 0)) +
    viridis::scale_fill_viridis()


## Beta distribution

# NOTE: This is different as the beta distribution has a constrained domain

# Transformations
logit <- function(x) log(x) - log(1 - x)
invlogit <- function(x) 1 / (1 + exp(-x))

beta_logp <- function(x, alpha, beta) {
  # x ^ (alpha - 1) * (1 - x) ^ (beta - 1)
  # log --> log(x ^ (alpha - 1)) + log((1 - x) ^ (beta - 1))
  # simplify --> (alpha - 1) * log(x) + (beta - 1) * log(1 - x)
  (alpha - 1) * log(x) + (beta - 1) * log(1 - x)
}

dinvlogit <- Deriv::Deriv(invlogit)

beta_logp_unconstrained <- function(x, alpha, beta) {
  beta_logp(invlogit(x), alpha, beta) + log(dinvlogit(x))
}

beta_dlogp_unconstrained <- Deriv::Deriv(beta_logp_unconstrained, "x")

beta_logp_unconstrained(5.5, 3, 2)
beta_dlogp_unconstrained(5.5, 3, 2)

make_neg_logp <- function(alpha, beta) {
    function(x) as.numeric(- beta_logp_unconstrained(x, alpha, beta))
}

make_neg_dlogp <- function(alpha, beta) {
    function(x) as.numeric(- beta_dlogp_unconstrained(x, alpha, beta))
}

neg_logp <- make_neg_logp(8, 2)
neg_dlogp <- make_neg_dlogp(8, 2)

neg_logp(0.5)
neg_dlogp(0.5)

neg_logp(3)
neg_dlogp(3)

sampler <- SamplerHMC$new(
  neg_logp, neg_dlogp, initial_position = 1, step_size = 0.01, path_length = 0.5
)

# One sample
sampler$generate_sample()

draws <- numeric(1000)

for (i in seq_along(draws)) {
    draws[i] <- sampler$generate_sample()$new_sample
}

x <- seq(0, 1, length.out = 200)
y <- dbeta(x, 8, 2)

data.frame(x = invlogit(draws)) |>
  ggplot() +
  geom_histogram(aes(x, after_stat(density)), fill="grey50", bins = 50) +
  geom_line(aes(x, y), color = "red", data = data.frame(x = x, y = y))

ggsave("plot3.png")
