#' @importFrom grDevices colorRampPalette
#' @importFrom utils head tail
SPEED_TO_MS <- c(500, 250, 100)

# Create covariance matrix for a normal distribution with a given correlation
make_sigma <- function(r = 0) {
  sigma <- diag(2)
  if (r != 0) {
    sigma[1, 2] <- r
    sigma[2, 1] <- r
  }
  sigma
}

# Initialize distributions
normal_1 <- NormalDistribution$new(mu = c(0, 0), sigma = make_sigma())
normal_2 <- NormalDistribution$new(mu = c(0, 0), sigma = make_sigma(0.3))
normal_3 <- NormalDistribution$new(mu = c(0, 0), sigma = make_sigma(0.7))
banana <- BananaDistribution$new()
funnel <- FunnelDistribution$new()
normal_mixture <- NormalMixture$new(
  mu1 = c(-1.5, -1.7),
  mu2 = c(0.9, 0.5),
  sigma1 = diag(2) * 0.2,
  sigma2 = diag(2) * 1.25
)

DISTRIBUTIONS <- list(
  "normal_1" = normal_1,
  "normal_2" = normal_2,
  "normal_3" = normal_3,
  "banana" = banana,
  "funnel" = funnel,
  "normal_mixture" = normal_mixture
)

LENGTHS_OUT <- list(
  "normal_1" = 60,
  "normal_2" = 60,
  "normal_3" = 60,
  "normal_mixture" = 60,
  "banana" = 80,
  "funnel" = 80
)

INITIAL_POSITIONS <- list(
  "normal_1" = c(1.6, -0.6),
  "normal_2" = c(1, 2),
  "normal_3" = c(-1.3, 0),
  "normal_mixture" = c(1.5, 0.5),
  "banana" = c(0.25, -2),
  "funnel" = c(-0.5, -1)
)

# A data container that stores different quantities required in several places
AppDataContainer <- R6::R6Class(
  classname = "AppDataContainer",
  public = list(
    sampler = NULL,
    clip_z = NULL,
    z_min = NULL,
    z_upper = NULL,
    length_out = NULL,
    sampler_data = NULL,
    arrow_obj = NULL
  )
)

# A stack to store the points on the subscenes
Stack <- R6::R6Class(
  classname = "Stack",
  public = list(
    items = list(),
    push = function(x) {
      self$items <- c(self$items, x)
      invisible(self)
    },
    pop = function() {
      item <- self$items[[self$length()]]
      self$items <- self$items[-self$length()]
      item
    },
    clear = function() {
      self$items <- list()
    },
    length = function() {
      length(self$items)
    }
  )
)

app_data <- AppDataContainer$new()
points_objects_dev1 <- Stack$new()
points_objects_dev2 <- Stack$new()

server <- function(input, output, session) {
  options_to_save <- options(rgl.inShiny = TRUE)
  on.exit(options(options_to_save))

  dist <- shiny::reactiveVal()
  keep_sampling_rv <- shiny::reactiveVal(FALSE)

  shiny::observe({
    # Set new value
    dist(DISTRIBUTIONS[[input$distribution]])

    step_size <- shiny::isolate(input$step_size)
    path_length <- shiny::isolate(input$path_length)
    initial_position <- INITIAL_POSITIONS[[shiny::isolate(input$distribution)]]

    app_data$sampler <- SamplerHMC$new(
      dist()$neg_logp,
      dist()$neg_dlogp,
      step_size = step_size,
      path_length = path_length,
      initial_position = initial_position
    )

    app_data$length_out <- LENGTHS_OUT[[input$distribution]]

    x_lims <- dist()$get_range_x()
    y_lims <- dist()$get_range_y()

    x <- seq(x_lims[1], x_lims[2], length.out = 101)
    y <- seq(y_lims[1], y_lims[2], length.out = 101)
    f <- dist()$neg_logp(expand.grid(x = x, y = y))

    app_data$clip_z <- - dist()$get_max_logp_at_boundary()
    app_data$z_min <- min(f)
    app_data$z_upper <- (x_lims[2] - x_lims[1]) * 0.5
  })


  output$rglPlot <- rgl::renderRglwidget({
    shiny::req(dist())
    rgl::mfrow3d(1, 2, sharedMouse = TRUE)
    rgl::view3d(theta = 0, phi = -55, zoom = 0.9)
    plot_neg_logp(dist(), app_data$length_out)

    rgl::next3d()
    rgl::view3d(theta = 0, phi = -55, zoom = 0.9)
    plot_density(dist(), app_data$length_out)
    rgl::rglwidget()
  })


  # Reactively update the sampler's path length and step size
  shiny::observe(
    {
      # Check step size is smaller than path length
      req(input$step_size < input$path_length)
      app_data$sampler$set_path_length(input$path_length)
      app_data$sampler$set_step_size(input$step_size)
    }
  )

  shiny::observeEvent(list(input$add_point, input$continue_sampling), {
    # Add trajectory and point to the first subscene
    momentum_xy <- c(isolate(input$momentum_x), isolate(input$momentum_y))
    if (isolate(input$manual_momentum)) {
      app_data$sampler_data <- app_data$sampler$generate_xyz(momentum_xy)
    } else {
      app_data$sampler_data <- app_data$sampler$generate_xyz()
    }

    data <- app_data$sampler_data

    momentum <- paste0("(", round(data$momentum$x, 3), ", ", round(data$momentum$y, 3), ")")
    shinyjs::html("text_momentum", paste("Momentum:", momentum))

    if (data$accepted) {
      point_color <- "red"
      segments_x <- data$segments$x
      segments_y <- data$segments$y
      segments_z <- data$segments$z
      point_x <- data$point$x
      point_y <- data$point$y
      point_z <- data$point$z
    } else {
      point_color <- "grey30"
      segments_x <- head(data$segments$x, -1)
      segments_y <- head(data$segments$y, -1)
      segments_z <- head(data$segments$z, -1)
      point_x <- tail(segments_x, 1)
      point_y <- tail(segments_y, 1)
      point_z <- tail(segments_z, 1)
    }

    # Add momentum arrow
    app_data$arrow_obj <- rgl::arrow3d(
      p0 = c(segments_x[1], segments_y[1], 0),
      p1 = c(data$momentum$x, data$momentum$y, 0),
      color = "grey30",
      n = 2,
      barblen = 0.02,
      width = 1 / 2,
      type = "lines"
    )

    subscene <- rgl::subsceneList()[[1]]
    msg_objects <- get_objects(list(app_data$arrow_obj), subscene)
    session$sendCustomMessage(
      "addToRglPlot",
      list(id = "rglPlot", objects = msg_objects, subscene = subscene)
    )

    # Create segments and point objects, adjusting the height 'z'
    segments_z <- arbitrary_scale(
      segments_z, 0, app_data$z_upper, x_min = app_data$z_min, x_max = app_data$clip_z
    )

    point_z <- arbitrary_scale(
      point_z, 0, app_data$z_upper, x_min = app_data$z_min, x_max = app_data$clip_z
    )

    segments <- make_trajectory_segments(x = segments_x, y = segments_y, z = segments_z)

    point <- rgl::points3d(
        x = point_x, y = point_y, z = point_z, color = point_color, size = 4
    )

    # Add segments and points to the subscene
    objects_list <- c(segments, point)
    msg_objects <- get_objects(objects_list, subscene)

    session$sendCustomMessage(
      "drawTrajectoryHMC",
      list(
        id = "rglPlot",
        objects = msg_objects,
        subscene = subscene,
        completed_id = "trajectory_done",
        total_time = SPEED_TO_MS[[input$speed]]
      )
    )
    points_objects_dev1$push(point)
  }, ignoreInit = TRUE)


  shiny::observeEvent(input$manual_momentum, {
    if (input$manual_momentum) {
      shinyjs::enable("momentum_x")
      shinyjs::enable("momentum_y")
    } else {
      shinyjs::disable("momentum_x")
      shinyjs::disable("momentum_y")
    }
  })

  # `input$trajectory_done` is changed when the program finishes plotting the trajectory
  shiny::observeEvent(input$trajectory_done, {

    data <- app_data$sampler_data

    # Update statistics on the first subscene
    status <- if (isTRUE(data$accepted)) "Accepted" else "Rejected"

    position <- paste0("(", round(data$point$x, 3), ", ", round(data$point$y, 3), ")")
    shinyjs::html("text_position", paste("Position:", position))

    shinyjs::html("text_status", paste("Status:", status))

    energy_diff <- round(data$h_current - data$h_proposal, 5)
    shinyjs::html("text_h_diff", paste("H(current) - H(proposal):", energy_diff))

    divergent <- if (data$divergent) "Yes" else "No"
    shinyjs::html("text_divergent", paste("Divergent:", divergent))

    # Add the sampled point to the second subscene, where we have the density function.
    shiny::req(input$trajectory_done)
    subscene <- rgl::subsceneList()[[2]]

    if (data$accepted) {
      point <- rgl::points3d(
        x = data$point$x, y = data$point$y, z = 0, color = "red", size = 4
      )
      msg_objects <- get_objects(list(point), subscene)
      session$sendCustomMessage(
        "addToRglPlot",
        list(id = "rglPlot", objects = msg_objects, subscene = subscene)
      )
      points_objects_dev2$push(point)
    }

    if (isolate(keep_sampling_rv())) {
      session$sendCustomMessage("updateInputValue", list(id = "continue_sampling"))
    }

    # Delete the momentum arrow on the first subscene
    subscene <- rgl::subsceneList()[[1]]
    session$sendCustomMessage(
      "deleteFromRglPlot",
      list(id = "rglPlot", objects = list(app_data$arrow_obj), subscene = subscene)
    )
  })

  # Start sampling
  shiny::observeEvent(input$start_sampling, {
    shinyjs::disable("start_sampling")
    shinyjs::disable("add_point")
    shinyjs::enable("stop_sampling")
    keep_sampling_rv(TRUE)
    session$sendCustomMessage("updateInputValue", list(id = "continue_sampling"))
  })

  # Stop sampling
  shiny::observeEvent(input$stop_sampling, {
    shinyjs::disable("stop_sampling")
    shinyjs::enable("start_sampling")
    shinyjs::enable("add_point")
    keep_sampling_rv(FALSE)
  })

  # Remove all sampled points from all subscenes
  shiny::observeEvent(input$remove_points, {
    # Remove points from the first subscene
    subscene <- rgl::subsceneList()[[1]]
    session$sendCustomMessage(
      "deleteFromRglPlot",
      list(id = "rglPlot", objects = points_objects_dev1$items, subscene = subscene)
    )
    points_objects_dev1$clear()

    # Remove points from the second subscene
    subscene <- rgl::subsceneList()[[2]]
    session$sendCustomMessage(
      "deleteFromRglPlot",
      list(id = "rglPlot", objects = points_objects_dev2$items, subscene = subscene)
    )
    points_objects_dev2$clear()
  })
}