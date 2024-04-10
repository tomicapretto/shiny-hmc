COLORS <- list(
  background = "grey85",
  contour = "grey50",
  surface = c("grey45", "grey80")
)

zero_one_scale <- function(x, lb = NULL, ub = NULL) {
  if (is.null(lb)) lb <- min(x)
  if (is.null(ub)) ub <- max(x)
  return((x - lb) / (ub - lb))
}

arbitrary_scale <- function(x, lb, ub, x_min = NULL, x_max = NULL) {
  if (length(x) == 0) return(Inf)
  if (is.null(x_min)) x_min <- min(x)
  if (is.null(x_max)) x_max <- max(x)
  ((x - x_min) / (x_max - x_min)) * (ub - lb) + lb
}

get_surface_colors <- function(x, colors, n = 50) {
  colors <- colorRampPalette(colors)(n)
  idxs <- ceiling(ecdf(unique(x))(x) * 50)
  return(colors[idxs])
}

group_in_segments <- function(x, target_n = 25) {
  # See how it works
  # group_in_segments(seq(0, 1, length.out = 101))
  # group_in_segments(seq(0, 1, length.out = 19))
  
  x_length <- length(x)
  if (x_length <= target_n + 1) {
    segments <- vector("list", x_length - 1)
    for (i in seq_along(segments)) {
      segments[[i]] <- x[c(i, i + 1)]
    }
    return(segments)
  } else {
    actual_n <- ceiling(x_length / target_n)
    segments_n <- x_length %/% actual_n
    segments_lengths <- rep(actual_n, segments_n)
    
    segments <- vector("list", segments_n)
    segments_idxs <- c(1, cumsum(segments_lengths))
    for (i in seq_len(segments_n)) {
      idxs <- (segments_idxs[i]):segments_idxs[i + 1]
      segments[[i]] <- x[idxs]
    }
    return(segments)
  }
}

make_trajectory_segments <- function(x, y, z, ...) {
  stopifnot(length(x) == length(y), length(y) == length(z))
  
  x_segments <- group_in_segments(x)
  y_segments <- group_in_segments(y)
  z_segments <- group_in_segments(z)
  
  segments <- vector("list", length(x_segments))
  
  for (i in seq_along(x_segments)) {
    segments[[i]] <- rgl::lines3d(
      x = x_segments[[i]], 
      y = y_segments[[i]], 
      z = z_segments[[i]], 
      ...
    )
  }
  return(segments)
}

#' Plot a surface of the negative logp of a distribution
#' 
#' @param dist Distribution
#' @param length_out The number of steps in the grid for 'x' and 'y'
plot_neg_logp <- function(dist, length_out = 50) {
  x_lims <- dist$get_range_x()
  y_lims <- dist$get_range_y()
  
  x <- seq(x_lims[1], x_lims[2], length.out = length_out)
  y <- seq(y_lims[1], y_lims[2], length.out = length_out)
  f <- dist$neg_logp(expand.grid(x = x, y = y))
  z <- matrix(f, nrow = length_out)
  
  # We clip the using the minimum negative logp at the boundary
  clip_z <- - dist$get_max_logp_at_boundary()

  # Scale values to the 0  - (x_range * 0.5) interval
  # Contains values above 1, but they will be discarded in the clipping phase
  x_range <- x_lims[2] - x_lims[1]
  z_scaled <- arbitrary_scale(z, 0, x_range * 0.5, x_max = clip_z)

  # Surface colors
  colors <- get_surface_colors(z_scaled, COLORS$surface)
  
  # Contour levels, minimum and maximum are 0 and 1 by construction
  contour_levels <- seq(0, x_range * 0.5, length.out = 9)[2:8]
  
  # A function to determine clipping
  clipping_fun <- function(values) {
    arbitrary_scale(dist$neg_logp(values[, 1:2]), 0, x_range * 0.5, x_max = clip_z)
  }
  
  # Set background color
  rgl::bg3d(color = COLORS$background)
  
  # Add surface
  surface_id <- rgl::persp3d(
    x = x, y = y, z = z_scaled, color = colors,
    xlab = "", ylab = "", zlab = "", aspect = "iso", alpha = 0.2,
    lit = FALSE, polygon_offset = 1, box = FALSE, axes = FALSE,
  )
  
  # Add contour lines
  rgl::contourLines3d(
    surface_id, levels = contour_levels, color = COLORS$contour, lwd = 1.4
  )
  
  # Clip the surface
  rgl::clipObj3d(surface_id, clipping_fun, bound = x_range * 0.5, greater = FALSE)
  
  # Add grid
  # TODO: determine the values of 'at' with the distribution
  rgl::grid3d("z", lwd = 0.75)
  rgl::axis3d("x", at = seq(-3, 3, by = 1))
  rgl::axis3d("y", at = seq(-3, 3, by = 1))
}

#' Plot a surface of the density function of a distribution
#' 
#' @param dist Distribution
#' @param length_out The number of steps in the grid for 'x' and 'y'
plot_density <- function(dist, length_out = 100) {
  x_lims <- dist$get_range_x()
  y_lims <- dist$get_range_y()
  
  x <- seq(x_lims[1], x_lims[2], length.out = length_out)
  y <- seq(y_lims[1], y_lims[2], length.out = length_out)
  f <- dist$density(expand.grid(x = x, y = y))
  z <- matrix(f, nrow = length_out)
  
  # Scale to 0-1 interval
  x_range <- x_lims[2] - x_lims[1]
  z_scaled <- arbitrary_scale(z, 0, x_range * 0.5)

  # Surface colors
  colors <- get_surface_colors(z_scaled, rev(COLORS$surface))
  
  # Contour levels, minimum and maximum are 0 and x_range * 0.7 by construction
  contour_levels <- seq(0, x_range * 0.7, length.out = 9)[2:8]
  
  # A function to determine clipping
  clipping_fun <- function(values) {
    arbitrary_scale(dist$density(values[, 1:2]), 0, x_range * 0.7)
  }
  
  # Set background color
  rgl::bg3d(color = COLORS$background)
  
  # Add surface
  surface_id <- rgl::persp3d(
    x = x, y = y, z = z_scaled, color = colors, 
    xlab = "", ylab = "", zlab = "", aspect = "iso", alpha = 0.2, 
    lit = FALSE, polygon_offset = 1, box = FALSE, axes = FALSE
  )
  
  # Add contour lines
  rgl::contourLines3d(
    surface_id, levels = contour_levels, color = COLORS$contour, lwd = 1.4
  )
  
  # Clip the surface. Actually, nothing is clipped. But, somehow, 
  # this fixes some rendering issues... who knows why!
  rgl::clipObj3d(surface_id, clipping_fun, bound = x_range * 0.71, greater = FALSE)
  
  # Add grid
  # TODO: determine the values of 'at'
  rgl::grid3d("z", lwd = 0.75)
  rgl::axis3d("x", at = seq(-3, 3, by = 1))
  rgl::axis3d("y", at = seq(-3, 3, by = 1))
}