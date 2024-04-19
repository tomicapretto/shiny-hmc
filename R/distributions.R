# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Base distribution class
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Distribution <- R6::R6Class(
  classname = "Distribution",
  public = list(
    initialize = function(...) {
      if (length(list(...))) self$set_params(...)
    },
    density = function(x) {
      stop("not implemented!")
    },
    neg_logp = function(x) {
      return(-log(self$density(x)))
    },
    dlogp = function(x) {
      stop("not implemented!")
    },
    neg_dlogp = function(x) {
      return(- self$dlogp(x))
    },
    get_range_x = function() {},
    get_range_y = function() {},
    get_range_z = function() {
      x_range <- self$get_range_x()
      y_range <- self$get_range_y()
      x_grid <- seq(x_range[1], x_range[2], length.out = 50)
      y_grid <- seq(y_range[1], y_range[2], length.out = 50)
      df_grid <- expand.grid(x = x_grid, y = y_grid)
      return(range(self$density(df_grid)))
    },
    get_params = function() {
      return(private$params)
    },
    set_params = function(...) {
      params <- list(...)
      params_names <- names(params)
      if (is.null(params_names) || any(params_names == "")) {
        stop("Parameters must be passed as named arguments")
      }
      for (name in params_names) {
        if (name %in% names(private$params)) {
          private$params[[name]] <- params[[name]]
        } else {
          warning(paste("Parameter", name, "not found in the distribution"))
        }
      }
    }
  ),
  private = list(
    params = list()
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Normal distribution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Unnormalized gaussian logp
normal_logp <- function(x, y, mu, tau) {
  X <- c(x, y)
  -0.5 * t(X - mu) %*% tau %*% (X - mu)
}

normal_dlogp_ <- Deriv::Deriv(normal_logp, c("x", "y"))

normal_dlogp <- function(x, y, mu, tau) {
  unname(normal_dlogp_(x, y, mu, tau))
}

NormalDistribution <- R6::R6Class(
  classname = "NormalDistribution",
  inherit = Distribution,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    density = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      mvtnorm::dmvnorm(x, private$params$mu, private$params$sigma)
    },
    dlogp = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) {
          normal_dlogp(x[1], x[2], private$params$mu, private$params$tau)
        }
      )
      return(as.vector(result))
    },
    get_range_x = function() {
      s <- sqrt(private$params$sigma[1, 1])
      r <- private$params$sigma[1, 2]
      s <- s / (1 - r * 0.2)
      return(c(-3.5 * s, 3.5 * s))
    },
    get_range_y = function() {
      s <- sqrt(private$params$sigma[2, 2])
      r <- private$params$sigma[1, 2]
      s <- s / (1 - r * 0.4)
      return(c(-3.5 * s, 3.5 * s))
    }, 
    get_max_logp_at_boundary = function() {
      x_range <- self$get_range_x()
      y_range <- self$get_range_y()
      x_grid <- seq(x_range[1], x_range[2], length.out = 50)
      y_grid <- seq(y_range[1], y_range[2], length.out = 50)
      df_grid <- expand.grid(x = x_grid, y = y_grid)
      
      x_border <- (df_grid$x == x_range[1]) | (df_grid$x == x_range[2])
      y_border <- (df_grid$y == y_range[1]) | (df_grid$y == y_range[2])
      
      df_grid <- df_grid[x_border | y_border, ]
      
      max_density <- max(self$density(df_grid))
      return(log(max_density))
    },
    set_params = function(...) {
      params <- list(...)
      params$tau <- solve(params$sigma)
      do.call(super$set_params, params)
    }
  ),
  private = list(
    params = list(mu = c(0, 0), sigma = diag(2), tau = NULL)
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Banana distribution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

banana_logp <- function(x, y, a, b) {
  # + 1.8 to translate the function on the "y" axis
  -((a - x) ^ 2 + b * (y + 1.8 - x ^ 2) ^ 2)
}

banana_dlogp_ <- Deriv::Deriv(banana_logp, c("x", "y"))

banana_dlogp <- function(x, y, a, b) {
  unname(banana_dlogp_(x, y, a, b))
}

BananaDistribution <- R6::R6Class(
  classname = "BananaDistribution",
  inherit = Distribution,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    density = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) banana_logp(x[1], x[2], private$params$a, private$params$b)
      )
      return(as.vector(exp(result)))
    },
    dlogp = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) banana_dlogp(x[1], x[2], private$params$a, private$params$b)
      )
      return(as.vector(result))
    },
    get_range_x = function() {
      return(c(-3, 3))
    },
    get_range_y = function() {
      return(c(-3, 3))
    }, 
    get_max_logp_at_boundary = function() {
      x_range <- self$get_range_x()
      y_range <- self$get_range_y()
      x_grid <- seq(x_range[1], x_range[2], length.out = 50)
      y_grid <- seq(y_range[1], y_range[2], length.out = 50)
      df_grid <- expand.grid(x = x_grid, y = y_grid)
      
      x_border <- (df_grid$x == x_range[1]) | (df_grid$x == x_range[2])
      y_border <- (df_grid$y == y_range[1]) | (df_grid$y == y_range[2])
      
      df_grid <- df_grid[x_border | y_border, ]
      max_density <- max(self$density(df_grid))
      return(log(max_density))
    }
  ),
  private = list(
    params = list(a = 0.4, b = 4)
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Funnel distribution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

funnel_logp_ <- function(x, m, s) {
  - log(s) - 0.5 * ((x - m) / s) ^ 2
}

funnel_logp <- function(x, y, m1, m2, sigma) {
  x0 <- y - 2
  x1 <- x
  funnel_logp_(x0, m1, sigma) + funnel_logp_(x1, m2, exp(x0 / 2))
}

funnel_dlogp_ <- Deriv::Deriv(funnel_logp, c("x", "y"))

funnel_dlogp <- function(x, y, m1, m2, sigma) {
  unname(funnel_dlogp_(x, y, m1, m2, sigma))
}

FunnelDistribution <- R6::R6Class(
  classname = "FunnelDistribution",
  inherit = Distribution,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    density = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) {
          funnel_logp(
            x[1], 
            x[2], 
            private$params$m1, 
            private$params$m2, 
            private$params$sigma
          )
        }
      )
      return(as.vector(exp(result)))
    },
    dlogp = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) {
          funnel_dlogp(
            x[1], 
            x[2], 
            private$params$m1, 
            private$params$m2, 
            private$params$sigma
          )
        }
      )
      return(as.vector(result))
    },
    get_range_x = function() {
      return(c(-4, 4))
    },
    get_range_y = function() {
      return(c(-5.5, 4))
    }, 
    get_max_logp_at_boundary = function() {
      return(-25)
    }
  ),
  private = list(
    params = list(m1 = 0, m2 = 0, sigma = 3)
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Normal mixture
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mixture_normal_logp <- function(x, y, mu1, tau1, mu2, tau2) {
  log(exp(normal_logp(x, y, mu1, tau1)) + exp(normal_logp(x, y, mu2, tau2)))
}

mixture_normal_dlogp_ <- Deriv::Deriv(mixture_normal_logp, c("x", "y"))

mixture_normal_dlogp <- function(x, y, mu1, tau1, mu2, tau2) {
  unname(mixture_normal_dlogp_(x, y, mu1, tau1, mu2, tau2))
}

NormalMixture <- R6::R6Class(
  classname = "NormalMixture",
  inherit = Distribution,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    density = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) {
          mixture_normal_logp(
            x[1], 
            x[2], 
            private$params$mu1, 
            private$params$tau1, 
            private$params$mu2, 
            private$params$tau2
          )
        }
      )
      return(as.vector(exp(result)))
    },
    dlogp = function(x) {
      if (is.vector(x)) {
        x <- matrix(x, ncol = length(x))
      } else if (is.data.frame(x)) {
        x <- as.matrix(x)
      } else {
        stopifnot(is.matrix(x))
      }
      result <- apply(
        x, 1, function(x) {
          mixture_normal_dlogp(
            x[1], 
            x[2], 
            private$params$mu1, 
            private$params$tau1, 
            private$params$mu2, 
            private$params$tau2
          )
        }
      )
      return(as.vector(result))
    },
    get_range_x = function() {
      return(c(-4, 4))
    },
    get_range_y = function() {
      return(c(-4, 4))
    }, 
    get_max_logp_at_boundary = function() {
      x_range <- self$get_range_x()
      y_range <- self$get_range_y()
      x_grid <- seq(x_range[1], x_range[2], length.out = 50)
      y_grid <- seq(y_range[1], y_range[2], length.out = 50)
      df_grid <- expand.grid(x = x_grid, y = y_grid)

      x_border <- (df_grid$x == x_range[1]) | (df_grid$x == x_range[2])
      y_border <- (df_grid$y == y_range[1]) | (df_grid$y == y_range[2])

      df_grid <- df_grid[x_border | y_border, ]
      max_density <- max(self$density(df_grid))
      return(log(max_density))
    },
    set_params = function(...) {
      params <- list(...)
      params$tau1 <- solve(params$sigma1)
      params$tau2 <- solve(params$sigma2)
      do.call(super$set_params, params)
    }
  ),
  private = list(
    params = list(
      mu1 = c(0, 0), sigma1 = diag(2), tau1 = NULL,
      mu2 = c(0, 0), sigma2 = diag(2), tau2 = NULL
    )
  )
)