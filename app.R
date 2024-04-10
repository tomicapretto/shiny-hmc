options(rgl.useNULL = TRUE)

start_app <- function() {
  shiny::shinyAppDir(
    here::here("app"),
    options = list(
      launch.browser = FALSE,
      port = 1234
    )
  )
}

start_app()