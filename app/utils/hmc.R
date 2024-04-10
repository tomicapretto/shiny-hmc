lappend <- function(l, object) {
  l[[length(l) + 1]] <- object
  l
}

ltail <- function(l) {
  l[[length(l)]]
}

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
  
  # Flip del momentum 
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
      H_current <- self$neg_logp(q_current) - mvtnorm::dmvnorm(p_current, log = TRUE)
      H_new <- self$neg_logp(q_new) - mvtnorm::dmvnorm(p_new, log = TRUE)
      
      u <- log(runif(1))
      accepted <- u < H_current - H_new
      sample <- if (isTRUE(accepted)) q_new else q_current
      self$generated_samples <- lappend(self$generated_samples, sample)
      
      # Compare energies and determine if it's a divergence
      energy_difference <- abs(H_new - H_current)
      is_divergent <- energy_difference > 1000 # heuristic taken from PyMC

      # cat("H original:", H_current, "\n")
      # cat("H new:", H_new, "\n")
      # cat("u:", u, "\n")
      # cat("accepted:", accepted, "\n")
      # cat("Is divergent?", is_divergent, "\n")
      # cat("-------------------------------\n")  

      return(
        list(
          sample = matrix(sample, nrow = 1), 
          leap_q = leap_q, 
          momentum = matrix(p_current, nrow = 1),
          accepted = accepted,
          new_sample = q_new,
          h_current = H_current,
          h_proposal = H_new,
          divergent = is_divergent
        )
      )
    },

    generate_momentum = function() {
      n <- length(self$initial_position)
      mvtnorm::rmvnorm(1, rep(0, n), diag(n))
      #c(0, 0)
    },
    
    generate_xyz = function() {
      initial_point <- ltail(self$generated_samples)
      sample <- self$generate_sample()
      xy <- do.call("rbind", sample$leap_q)
      # Prepend the initial point so trajectories don't have a gap
      xy <- rbind(initial_point, xy) 
      data <- cbind(xy, self$neg_logp(xy))
      data <- rbind(
        data,
        cbind(sample$sample, self$neg_logp(sample$sample))
      )
      
      x <- data[, 1]
      y <- data[, 2]
      z <- data[, 3]

      return(
        list(
          segments = list(x = x, y = y, z = z),
          point = list(x = tail(x, 1), y = tail(y, 1), z = tail(z, 1)),
          momentum = list(x = sample$momentum[, 1], y = sample$momentum[, 2]),
          accepted = isTRUE(sample$accepted),
          h_current = sample$h_current,
          h_proposal = sample$h_proposal,
          divergent = isTRUE(sample$divergent),
          new_sample = c(sample$q_new, self$neg_logp(sample$new_sample))
        )
      )
    }
  )
)