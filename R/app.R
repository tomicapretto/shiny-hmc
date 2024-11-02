#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp runApp
run_app <- function(
  port = getOption("shiny.port"),
  launch.browser = getOption("shiny.launch.browser", interactive())
) {
  runApp(shinyApp(ui = ui, server = server), port = port, launch.browser = launch.browser)
}